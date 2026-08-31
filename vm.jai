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
    frame: = *vm.frames[vm.frame_count];
    frame.function = function;
    frame.ip = function.chunk.code.data;
    frame.slots = vm.stack.data;

    vm.frame_count += 1;

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

            case .RETURN;
            {
                pop(vm);
                return .OK;
            }
            case .NOP;
        }
    }

    return .RUNTIME_ERROR;
}


runtime_error :: (vm: *VM, message: string)
{
    log_error(message);

    frame: = current_frame(vm);
    instruction: = frame.ip - frame.function.chunk.code.data - 1;
    line: = get_line(*frame.function.chunk, instruction);
    log_error("[line %] in script", line);
    reset_stack(vm);
}

#scope_file

current_frame :: (vm: *VM) -> *CallFrame
{
    assert(vm.frame_count > 0);
    return *vm.frames[vm.frame_count-1];
}