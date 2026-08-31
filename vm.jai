FRAMES_MAX :: 64;
STACK_MAX :: FRAMES_MAX * 256;

VM :: struct
{
    frames: [FRAMES_MAX]CallFrame;
    frame_count: int;

    stack: [STACK_MAX]Value;
    sp: *Value;

    globals: Table(*ObjString, Value);
}

CallFrame :: struct
{
    function: *ObjFunction;
    ip: *u8;
    slots: *Value;
}

init :: (vm: *VM)
{
    reset_stack(vm);
    
    base_time = current_time_consensus();
    define_native(vm, "clock", clock_native);
}

shutdown :: (vm: *VM)
{
    table_reset(*vm.globals);
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
    call(vm, function, 0);

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

call :: (vm: *VM, function: *ObjFunction, arg_count: u8) -> bool
{
    if arg_count != function.arity
    {
        runtime_error(vm, tprint("Expected % arguments but got %.", function.arity, arg_count));
        return false;
    }

    if vm.frame_count == FRAMES_MAX
    {
        runtime_error(vm, "Stack overflow.");
        return false;
    }

    frame: = *vm.frames[vm.frame_count];
    vm.frame_count += 1;

    frame.function = function;
    frame.ip = frame.function.chunk.code.data;
    frame.slots = vm.sp - arg_count - 1;
    return true;
}

call_value :: (vm: *VM, callee: Value, arg_count: u8) -> bool
{
    if callee.type == .OBJ
    {
        if callee.obj.type ==
        {
            case .FUNCTION;
            {
                return call(vm, as_obj_function(callee), arg_count);
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
        return frame.function.chunk.constants[read_byte(frame)];
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
            disassemble_instruction(*frame.function.chunk, frame.ip - frame.function.chunk.code.data);
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
                    result: = obj_string_value(tprint("%%", a.str, b.str));

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

            case .RETURN;
            {
                result: = pop(vm);
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
        function: = frame.function;
        instruction: = frame.ip - function.chunk.code.data - 1;
        line: = get_line(*function.chunk, instruction);
        function_name: = ifx function.name != null then tprint("%()", function.name.str) else "script";
        log_error("[line %] in %", line, function_name);
    }

    reset_stack(vm);
}

define_native :: (vm: *VM, name: string, function: NativeFn)
{
    push(vm, obj_string_value(name));
    push(vm, obj_native_value(function));
    table_set(*vm.globals, as_obj_string(peek(vm, 1)), peek(vm, 0));
    pop(vm);
    pop(vm);
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