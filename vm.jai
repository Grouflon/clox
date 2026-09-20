FRAMES_MAX :: 64;
STACK_MAX :: FRAMES_MAX * 256;

VM :: struct
{
    frames: [FRAMES_MAX]CallFrame;
    frame_count: int;

    stack: [STACK_MAX]Value;
    sp: *Value;

    globals: Table(*ObjString, Value);
    objects: *Obj;
    strings: Table(u32, *ObjString);
    open_upvalues: *ObjUpvalue;

    // GC
    bytes_allocated: s64;
    next_gc: s64;
}

CallFrame :: struct
{
    closure: *ObjClosure;
    ip: *u8;
    slots: *Value;
}

init :: (vm: *VM)
{
    reset_stack(vm);
    vm.next_gc = 1024*1024;
    
    base_time = current_time_consensus();
    define_native(vm, "clock", clock_native);
}

shutdown :: (vm: *VM)
{
    table_reset(*vm.globals);
    free_objects(vm);
    deinit(*vm.strings);

    assert(vm.bytes_allocated == 0);
}

free_objects :: (vm: *VM)
{
    obj: = vm.objects;
    while obj != null
    {
        next: = obj.next;
        free_object(vm, obj);
        obj = next;
    }
    vm.objects = null;
}

collect_garbage :: (vm: *VM)
{
    GC_HEAP_GROW_FACTOR :: 2;

    if vm.frame_count == 0 return; // prevent GC during compilation

    #if DEBUG_LOG_GC
    {
        log("-- gc begin");
        before: = vm.bytes_allocated;
    }

    gray_stack: [..]*Obj;
    gray_stack.allocator = temporary_allocator;

    mark_roots(vm, *gray_stack);
    trace_references(vm, *gray_stack);

    // Remove white strings in string table
    for str, key : vm.strings
    {
        if !str.is_marked
        {
            table_remove(*vm.strings, key);
        }
    }

    sweep(vm);

    vm.next_gc = vm.bytes_allocated * GC_HEAP_GROW_FACTOR;

    #if DEBUG_LOG_GC
    {
        log("-- gc end");
        log("   collected % bytes (from % to %) next at %", before - vm.bytes_allocated, before, vm.bytes_allocated, vm.next_gc);
    }
}

mark_roots :: (vm: *VM, gray_stack: *[..]*Obj)
{
    // Mark stack
    slot: = vm.stack.data;
    while slot < vm.sp
    {
        mark_value(vm, gray_stack, slot.*);
        slot += 1;
    }

    // Mark globals
    for value, key: vm.globals
    {
        mark_object(vm, gray_stack, key);
        mark_value(vm, gray_stack, value);
    }

    // Mark frames
    for i: 0..vm.frame_count-1
    {
        mark_object(vm, gray_stack, vm.frames[i].closure);
    }

    // Mark upvalues
    upvalue: = vm.open_upvalues;
    while upvalue != null
    {
        mark_object(vm, gray_stack, upvalue);
        upvalue = upvalue.next_upvalue;
    }
}

mark_value :: (vm: *VM, gray_stack: *[..]*Obj, value: Value)
{
    if value.type != .OBJ return;
    mark_object(vm, gray_stack, value.obj);
}

mark_object :: (vm: *VM, gray_stack: *[..]*Obj, obj: *Obj)
{
    if obj == null return;
    if obj.is_marked return;

    obj.is_marked = true;

    array_add(gray_stack, obj);

    #if DEBUG_LOG_GC
    {
        log("% mark %", obj, object_to_string(obj));
    }
}

trace_references :: (vm: *VM, gray_stack: *[..]*Obj)
{
    while gray_stack.count > 0
    {
        obj: = pop(gray_stack);
        blacken_object(vm, gray_stack, obj);
    }
}

blacken_object :: (vm: *VM, gray_stack: *[..]*Obj, obj: *Obj)
{
    #if DEBUG_LOG_GC
    {
        log("% blacken %", obj, object_to_string(obj));
    }

    if obj.type ==
    {
        case .CLOSURE;
        {
            c: = obj.(*ObjClosure);
            mark_object(vm, gray_stack, c.function);
            for upvalue: c.upvalues
            {
                mark_object(vm, gray_stack, upvalue);
            }
        }

        case .FUNCTION;
        {
            f: = obj.(*ObjFunction);
            mark_object(vm, gray_stack, f.name);
            for value: f.chunk.constants
            {
                mark_value(vm, gray_stack, value);
            }
        }

        case .UPVALUE;
        {
            mark_value(vm, gray_stack, obj.(*ObjUpvalue).closed);
        }

        case .NATIVE;
        case .STRING;
    }
}

sweep :: (vm: *VM)
{
    previous: *Obj;
    obj: = vm.objects;
    while obj != null
    {
        if obj.is_marked
        {
            obj.is_marked = false;
            previous = obj;
            obj = obj.next;
            continue;
        }

        unreached: = obj;
        obj = obj.next;
        if previous != null
        {
            previous.next = obj;
        }
        else
        {
            vm.objects = obj;
        }
        free_object(vm, unreached);
    }
}

InterpretResult :: enum
{
    OK;
    COMPILE_ERROR;
    RUNTIME_ERROR;
}

interpret :: (vm: *VM, function: *ObjFunction) -> InterpretResult
{
    assert(function != null);

    push(vm, obj_value(function));
    closure: = new_obj_closure(vm, function);
    pop(vm);
    push(vm, obj_value(closure));

    call(vm, closure, 0);

    return run(vm);
}

push :: (vm: *VM, value: Value)
{
    vm.sp.* = value;
    vm.sp += 1;
}

pop :: (vm: *VM) -> Value
{
    assert(vm.sp > vm.stack.data);

    vm.sp -= 1;
    return vm.sp.*;
}

peek :: (vm: *VM, distance: int = 0) -> Value
{
    assert(vm.sp > vm.stack.data);

    return vm.sp[-1 - distance];
}

call :: (vm: *VM, closure: *ObjClosure, arg_count: u8) -> bool
{
    if arg_count != closure.function.arity
    {
        runtime_error(vm, tprint("Expected % arguments but got %.", closure.function.arity, arg_count));
        return false;
    }

    if vm.frame_count == FRAMES_MAX
    {
        runtime_error(vm, "Stack overflow.");
        return false;
    }

    frame: = *vm.frames[vm.frame_count];
    vm.frame_count += 1;

    frame.closure = closure;
    frame.ip = frame.closure.function.chunk.code.data;
    frame.slots = vm.sp - arg_count - 1;
    return true;
}

call_value :: (vm: *VM, callee: Value, arg_count: u8) -> bool
{
    if callee.type == .OBJ
    {
        if callee.obj.type ==
        {
            case .CLOSURE;
            {
                return call(vm, as_obj_closure(callee), arg_count);
            }

            case .NATIVE;
            {
                native: = as_obj_native(callee);
                result: = native.function(arg_count, vm.sp - arg_count);
                vm.sp -= arg_count+1;
                push(vm, result);
                return true;
            }
        }
    }

    runtime_error(vm, "Can only call functions and classes.");
    return false;
}

capture_upvalue :: (vm: *VM, local: *Value) -> *ObjUpvalue
{
    prev_upvalue: *ObjUpvalue;
    upvalue: = vm.open_upvalues;

    while upvalue != null && upvalue.location > local
    {
        prev_upvalue = upvalue;
        upvalue = upvalue.next_upvalue;
    }

    if upvalue != null && upvalue.location == local
    {
        return upvalue;
    }

    created_upvalue: = new_object(vm, ObjUpvalue);
    created_upvalue.location = local;
    created_upvalue.next_upvalue = upvalue;

    if prev_upvalue == null
    {
        vm.open_upvalues = created_upvalue;
    }
    else
    {
        prev_upvalue.next = created_upvalue;
    }

    return created_upvalue;
}

close_upvalues :: (vm: *VM, last: *Value)
{
    while vm.open_upvalues != null && vm.open_upvalues.location >= last
    {
        upvalue: = vm.open_upvalues;
        upvalue.closed = upvalue.location.*;
        upvalue.location = *upvalue.closed;
        vm.open_upvalues = upvalue.next_upvalue;
    }
}

reset_stack :: (vm: *VM)
{
    vm.sp = vm.stack.data;
}

run :: (vm: *VM) -> InterpretResult
{
    read_byte :: (frame: *CallFrame) -> u8
    {
        byte: = frame.ip.*;
        frame.ip += 1;
        return byte;
    }

    read_short :: (frame: *CallFrame) -> u16
    {
        byte: = frame.ip.(*u16).*;
        frame.ip += 2;
        return byte;
    }

    read_constant :: (frame: *CallFrame) -> Value
    {
        return frame.closure.function.chunk.constants[read_byte(frame)];
    }

    read_string :: (frame: *CallFrame) -> *ObjString
    {
        value: = read_constant(frame);
        assert(value.type == .OBJ);
        assert(value.obj.type == .STRING);
        return value.obj.(*ObjString);
    }

    BINARY_OP :: (vm: *VM, $op: string) #expand
    {
        if peek(vm, 0).type != .NUMBER || peek(vm, 1).type != .NUMBER
        {
            runtime_error(vm, "Operands must be numbers");
            `return .RUNTIME_ERROR;
        }

        b: = pop(vm).number;
        a: = pop(vm).number;
        result: = #insert #run tprint("a % b;", op);

        push(vm, make_value(result));
    }

    frame: = current_frame(vm);
    while true
    {
        #if DEBUG_TRACE_EXECUTION
        {
            print("        ");
            sp: = vm.stack.data;
            while sp < vm.sp
            {
                print("[%]", value_to_string(sp.*));
                sp += 1;
            }
            print("\n");
            disassemble_instruction(*frame.closure.function.chunk, frame.ip - frame.closure.function.chunk.code.data);
        }

        instruction: OpCode = xx read_byte(frame);
        if #complete instruction == 
        {
            case .CONSTANT;
            {
                constant: = read_constant(frame);
                push(vm, constant);
            }
            case .NIL; push(vm, nil_value());
            case .TRUE; push(vm, bool_value(true));
            case .FALSE; push(vm, bool_value(false));
            case .EQUAL; push(vm, bool_value(values_equal(pop(vm), pop(vm))));
            case .GREATER; BINARY_OP(vm, ">");
            case .LESS; BINARY_OP(vm, "<");
            case .ADD;
            {
                if is_string(peek(vm, 0)) && is_string(peek(vm, 1))
                {
                    b: = as_obj_string(pop(vm));
                    a: = as_obj_string(pop(vm));
                    result: = obj_string_value(vm, tprint("%%", a.str, b.str));

                    push(vm, result);
                }
                else if is_number(peek(vm, 0)) && is_number(peek(vm, 1))
                {
                    b: = pop(vm).number;
                    a: = pop(vm).number;
                    push(vm, number_value(a + b));
                }
                else
                {
                    runtime_error(vm, "Operands must be two numbers or two strings.");
                    return .RUNTIME_ERROR;
                }
            }
            case .SUBTRACT; BINARY_OP(vm, "-");
            case .MULTIPLY; BINARY_OP(vm, "*");
            case .DIVIDE; BINARY_OP(vm, "/");
            case .NEGATE;
            {
                if peek(vm).type != .NUMBER
                {
                    runtime_error(vm, "Operand must be a number");
                    return .RUNTIME_ERROR;
                }

                push(vm, number_value(-pop(vm).number));
            }
            case .NOT; push(vm, bool_value(is_falsey(pop(vm))));
            case .PRINT;
            {
                log("%", value_to_string(pop(vm)));
            }
            case .POP;
            {
                pop(vm);
            }
            case .DEFINE_GLOBAL;
            {
                name: = read_string(frame);
                value: = pop(vm);
                table_set(*vm.globals, name, value);
            }
            case .GET_GLOBAL;
            {
                name: = read_string(frame);
                found, value: = table_find(*vm.globals, name);
                if !found
                {
                    runtime_error(vm, tprint("Undefined variable '%'.", name.str));
                    return .RUNTIME_ERROR;
                }
                push(vm, value);
            }
            case .SET_GLOBAL;
            {
                name: = read_string(frame);
                found, value: = table_find(*vm.globals, name);
                if !found
                {
                    runtime_error(vm, tprint("Undefined variable '%'.", name.str));
                    return .RUNTIME_ERROR;
                }
                table_set(*vm.globals, name, peek(vm));
                // push(vm, value);
            }
            case .GET_LOCAL;
            {
                slot: = read_byte(frame);
                push(vm, frame.slots[slot]);
            }
            case .SET_LOCAL;
            {
                slot: = read_byte(frame);
                frame.slots[slot] = peek(vm);
            }
            case .GET_UPVALUE;
            {
                slot: = read_byte(frame);
                push(vm, frame.closure.upvalues[slot].location.*);
            }
            case .SET_UPVALUE;
            {
                slot: = read_byte(frame);
                frame.closure.upvalues[slot].location.* = peek(vm);
            }
            case .JUMP_IF_FALSE;
            {
                offset: = read_short(frame);
                if is_falsey(peek(vm)) frame.ip += offset;
            }
            case .JUMP;
            {
                offset: = read_short(frame);
                frame.ip += offset;
            }
            case .LOOP;
            {
                offset: = read_short(frame);
                frame.ip -= offset;
            }
            case .CALL;
            {
                arg_count: = read_byte(frame);
                if !call_value(vm, peek(vm, arg_count), arg_count)
                {
                    return .RUNTIME_ERROR;
                }
                frame = current_frame(vm);
            }
            case .CLOSURE;
            {
                function: = as_obj_function(read_constant(frame));
                closure: = new_obj_closure(vm, function);
                push(vm, obj_value(closure));

                for i: 0..closure.upvalues.count-1
                {
                    is_local: = read_byte(frame);
                    index: = read_byte(frame);

                    if is_local
                    {
                        closure.upvalues[i] = capture_upvalue(vm, frame.slots + index);
                    }
                    else
                    {
                        closure.upvalues[i] = frame.closure.upvalues[index];
                    }
                }
            }
            case .CLOSE_UPVALUE;
            {
                close_upvalues(vm, vm.sp - 1);
                pop(vm);
            }

            case .RETURN;
            {
                result: = pop(vm);
                close_upvalues(vm, frame.slots);
                vm.frame_count -= 1;
                if vm.frame_count == 0
                {
                    pop(vm);
                    return .OK;
                }

                vm.sp = frame.slots;
                push(vm, result);
                frame = current_frame(vm);
            }
            case .NOP;
        }
    }

    return .RUNTIME_ERROR;
}


runtime_error :: (vm: *VM, message: string)
{
    log_error(message);

    for < i: 0..vm.frame_count-1
    {
        frame: = *vm.frames[i];
        function: = frame.closure.function;
        instruction: = frame.ip - function.chunk.code.data - 1;
        line: = get_line(*function.chunk, instruction);
        function_name: = ifx function.name != null then tprint("%()", function.name.str) else "script";
        log_error("[line %] in %", line, function_name);
    }

    reset_stack(vm);
}

define_native :: (vm: *VM, name: string, function: NativeFn)
{
    push(vm, obj_string_value(vm, name));
    push(vm, obj_native_value(vm, function));
    table_set(*vm.globals, as_obj_string(peek(vm, 1)), peek(vm, 0));
    pop(vm);
    pop(vm);
}

clox_allocator_proc :: (mode: Allocator_Mode, size: s64, old_size: s64, old_memory_pointer: *void, proc_data: *void) -> *void
{
    vm: = proc_data.(*VM);
    assert(vm != null);

    #if DEBUG_STRESS_GC
    {
        if size > old_size
        {
            collect_garbage(vm);
        }
    }
    else
    {
        if vm.bytes_allocated > vm.next_gc
        {
            collect_garbage(vm);
        }
    }

    old_usable_size: = usable_size(old_memory_pointer);

    result: = context.default_allocator.proc(mode, size, old_size, old_memory_pointer, context.default_allocator.data);

    new_usable_size: = usable_size(result);

    vm.bytes_allocated += new_usable_size - old_usable_size;
    assert(vm.bytes_allocated >= 0); // counting problem

    return result;
}

#scope_file

current_frame :: (vm: *VM) -> *CallFrame
{
    assert(vm.frame_count > 0);
    return *vm.frames[vm.frame_count-1];
}

base_time: Apollo_Time;
clock_native :: (arg_count: int, args: *Value) -> Value
{
    return number_value(to_microseconds(current_time_consensus() - base_time).(float) / 1_000_000);
}

#import "Default_Allocator";