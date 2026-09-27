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
    print("% ", formatInt(offset, 16, 4));
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
        case .NIL; return simple_instruction("NIL", offset);
        case .TRUE; return simple_instruction("TRUE", offset);
        case .FALSE; return simple_instruction("FALSE", offset);
        case .EQUAL; return simple_instruction("EQUAL", offset);
        case .LESS; return simple_instruction("LESS", offset);
        case .GREATER; return simple_instruction("GREATER", offset);
        case .NEGATE; return simple_instruction("NEGATE", offset);
        case .ADD; return simple_instruction("ADD", offset);
        case .SUBTRACT; return simple_instruction("SUBTRACT", offset);
        case .MULTIPLY; return simple_instruction("MULTIPLY", offset);
        case .DIVIDE; return simple_instruction("DIVIDE", offset);
        case .NOT; return simple_instruction("NOT", offset);
        case .RETURN; return simple_instruction("RETURN", offset);
        case .PRINT; return simple_instruction("PRINT", offset);
        case .POP; return simple_instruction("POP", offset);
        case .DEFINE_GLOBAL; return constant_instruction("DEFINE_GLOBAL", chunk, offset);
        case .GET_GLOBAL; return constant_instruction("GET_GLOBAL", chunk, offset);
        case .SET_GLOBAL; return constant_instruction("SET_GLOBAL", chunk, offset);
        case .GET_LOCAL; return byte_instruction("GET_LOCAL", chunk, offset);
        case .SET_LOCAL; return byte_instruction("SET_LOCAL", chunk, offset);
        case .GET_UPVALUE; return byte_instruction("GET_UPVALUE", chunk, offset);
        case .SET_UPVALUE; return byte_instruction("SET_UPVALUE", chunk, offset);
        case .JUMP_IF_FALSE; return jump_instruction("JUMP_IF_FALSE", 1, chunk, offset);
        case .JUMP; return jump_instruction("JUMP", 1, chunk, offset);
        case .LOOP; return jump_instruction("LOOP", -1, chunk, offset);
        case .CALL; return byte_instruction("CALL", chunk, offset);
        case .CLOSURE;
        {
            constant: = chunk.code[offset + 1];
            print("% % %", tprint_fixed_length_string("CLOSURE", 16), constant, value_to_string(chunk.constants[constant]));
            f: = as_obj_function(chunk.constants[constant]);
            o: = offset + 2;

            for i: 0..f.upvalue_count-1
            {
                print("\n");

                is_local: = chunk.code[o];
                index: = chunk.code[o+1];
                print("% % % %", formatInt(o, 16, 4), tprint_fixed_length_string("   |", 23), ifx is_local then "local" else "upvalue", index);

                o += 2;
            }

            return o;
        }
        case .CLOSE_UPVALUE; return simple_instruction("CLOSE_UPVALUE", offset);
        case .CLASS; return constant_instruction("CLASS", chunk, offset);
        case .GET_PROPERTY; return constant_instruction("GET_PROPERTY", chunk, offset);
        case .SET_PROPERTY; return constant_instruction("SET_PROPERTY", chunk, offset);
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

byte_instruction :: (name: string, chunk: *Chunk, offset: int) -> int
{
    byte: = chunk.code[offset + 1];
    print("% %", tprint_fixed_length_string(name, 16), byte);
    return offset+2;
}

jump_instruction :: (name: string, sign: int, chunk: *Chunk, offset: int) -> int
{
    jump: = (chunk.code.data + offset + 1).(*u16).*;
    print("% % -> %", tprint_fixed_length_string(name, 16), formatInt(offset, 16, 2), formatInt(offset + 3 + sign * jump, 16, 2));
    return offset+3;
}
