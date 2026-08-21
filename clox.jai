#import "Basic";

#load "tools.jai";

DEBUG_TRACE_EXECUTION :: true;

// Chunk
OpCode :: enum u8
{
    NOP;
    CONSTANT;
    NEGATE;
    ADD;
    SUBTRACT;
    MULTIPLY;
    DIVIDE;
    RETURN;
}

LineInfo :: struct
{
    number: int;
    count: int;
}

Value :: float64;

Chunk :: struct
{
    code: [..]u8;
    lines: [..]LineInfo;
    constants: [..]Value;
}

shutdown :: (chunk: *Chunk)
{
    array_reset(*chunk.code);
    array_reset(*chunk.lines);
    array_reset(*chunk.constants);
}

write_chunk :: (chunk: *Chunk, byte: u8, line: int)
{
    array_add(*chunk.code, byte);
    if chunk.lines.count == 0 || chunk.lines[chunk.lines.count-1].number < line
    {
        array_add(*chunk.lines, .{line, 1});
    }
    else
    {
        assert(chunk.lines[chunk.lines.count-1].number == line, "lines must be written in order");
        chunk.lines[chunk.lines.count-1].count += 1;
    }
}

disassemble_chunk :: (chunk: *Chunk, name: string)
{
    log("== % ==", name);

    offset: = 0;
    while offset < chunk.code.count
    {
        offset = disassemble_instruction(chunk, offset);
    }
}

disassemble_instruction :: (chunk: *Chunk, offset: int) -> int
{
    print("% ", formatInt(offset, 16, 2));
    prev_line: = get_line(chunk, offset - 1);
    line: = get_line(chunk, offset);
    if (offset > 0 && prev_line == line)
    {
        print("   | ");
    }
    else
    {
        print("% ", tprint_fixed_length_value(line, 4, .LEFT));
    }

    defer print("\n");

    instruction: OpCode = xx chunk.code[offset];
    if #complete instruction ==
    {
        case .NOP; return simple_instruction("NOP", offset);
        case .CONSTANT; return constant_instruction("CONSTANT", chunk, offset);
        case .NEGATE; return simple_instruction("NEGATE", offset);
        case .ADD; return simple_instruction("ADD", offset);
        case .SUBTRACT; return simple_instruction("SUBTRACT", offset);
        case .MULTIPLY; return simple_instruction("MULTIPLY", offset);
        case .DIVIDE; return simple_instruction("DIVIDE", offset);
        case .RETURN; return simple_instruction("RETURN", offset);
        case;
        {
            print("Unknown OpCode: %", formatInt(instruction.(u8), 16, 2));
            return offset+1;
        }
    }
}

constant_instruction :: (name: string, chunk: *Chunk, offset: int) -> int
{
    constant_index: = chunk.code[offset + 1];
    print("% % '%'", tprint_fixed_length_string(name, 16), constant_index, value_to_string(chunk.constants[constant_index]));
    return offset+2;
}

simple_instruction :: (name: string, offset: int) -> int
{
    print("%", name);
    return offset+1;
}

add_constant :: (chunk: *Chunk, constant: Value) -> u8
{
    array_add(*chunk.constants, constant);
    return xx (chunk.constants.count-1);
}

get_line :: (chunk: *Chunk, offset: int) -> int
{
    c: = 0;
    for l: chunk.lines
    {
        if offset - c < l.count
        {
            return l.number;
        }
        c+= l.count;
    }
    return -1;
}

value_to_string :: (value: Value) -> string
{
    return tprint("%", value);
}

// VM
STACK_SIZE :: 256;

VM :: struct
{
    chunk: *Chunk;
    ip: *u8;
    stack: [STACK_SIZE]Value;
    sp: *Value;
}

init :: (vm: *VM)
{
    vm.sp = vm.stack.data;
}

shutdown :: (vm: *VM)
{

}

InterpretResult :: enum
{
    OK;
    COMPILE_ERROR;
    INTERPRET_ERROR;
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
    vm.sp -= 1;
    return vm.sp.*;
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

    BINARY_OP :: (vm: *VM, $op: string) #expand
    {
        b: = pop(vm);
        a: = pop(vm);
        push(vm, #insert #run tprint("a % b;", op));
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
            case .NEGATE;
            {
                push(vm, -pop(vm));
            }
            case .ADD; BINARY_OP(vm, "+");
            case .SUBTRACT; BINARY_OP(vm, "-");
            case .MULTIPLY; BINARY_OP(vm, "*");
            case .DIVIDE; BINARY_OP(vm, "/");
            case .RETURN;
            {
                log("%", value_to_string(pop(vm)));
                return .OK;
            }

            case .NOP;
        }
    }

    return .INTERPRET_ERROR;
}

main :: ()
{
    chunk: Chunk;
    constant: u8;
    defer shutdown(*chunk);

    // Write
    constant = add_constant(*chunk, 1.2);
    write_chunk(*chunk, xx OpCode.CONSTANT, 123);
    write_chunk(*chunk, constant, 123);

    constant = add_constant(*chunk, 3.4);
    write_chunk(*chunk, xx OpCode.CONSTANT, 123);
    write_chunk(*chunk, constant, 123);

    write_chunk(*chunk, xx OpCode.ADD, 123);

    constant = add_constant(*chunk, 5.6);
    write_chunk(*chunk, xx OpCode.CONSTANT, 123);
    write_chunk(*chunk, constant, 123);

    write_chunk(*chunk, xx OpCode.DIVIDE, 123);

    write_chunk(*chunk, xx OpCode.NEGATE, 123);

    write_chunk(*chunk, xx OpCode.RETURN, 123);

    // Print
    disassemble_chunk(*chunk, "test chunk");

    // Run
    vm: VM;
    init(*vm);
    defer shutdown(*vm);

    interpret(*vm, *chunk);
}