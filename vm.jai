STACK_SIZE :: 256;

VM :: struct
{
    chunk: *Chunk;
    ip: *u8;
    stack: [STACK_SIZE]Value;
    sp: *Value;
    globals: Table(*ObjString, Value);
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

interpret :: (vm: *VM, chunk: *Chunk) -> InterpretResult
{
    assert(chunk != null);

    vm.chunk = chunk;
    vm.ip = chunk.code.data;
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
    read_byte :: (vm: *VM) -> u8
    {
        byte: = vm.ip.*;
        vm.ip += 1;
        return byte;
    }

    read_constant :: (vm: *VM) -> Value
    {
        return vm.chunk.constants[read_byte(vm)];
    }

    read_string :: (vm: *VM) -> *ObjString
    {
        value: = read_constant(vm);
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
            disassemble_instruction(vm.chunk, vm.ip - vm.chunk.code.data);
        }

        instruction: OpCode = xx read_byte(vm);
        if #complete instruction == 
        {
            case .CONSTANT;
            {
                constant: = read_constant(vm);
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
                name: = read_string(vm);
                value: = pop(vm);
                table_set(*vm.globals, name, value);
            }
            case .GET_GLOBAL;
            {
                name: = read_string(vm);
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
                name: = read_string(vm);
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
                slot: = read_byte(vm);
                push(vm, vm.stack[slot]);
            }
            case .SET_LOCAL;
            {
                slot: = read_byte(vm);
                vm.stack[slot] = peek(vm);
            }

            case .RETURN;
            {
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

    instruction: = vm.ip - vm.chunk.code.data - 1;
    line: = get_line(vm.chunk, instruction);
    log_error("[line %] in script", line);
    reset_stack(vm);
}