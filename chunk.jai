OpCode :: enum u8
{
    NOP;
    CONSTANT;
    NIL;
    TRUE;
    FALSE;
    EQUAL;
    GREATER;
    LESS;
    ADD;
    SUBTRACT;
    MULTIPLY;
    DIVIDE;
    NEGATE;
    NOT;
    RETURN;
    PRINT;
    POP;
    DEFINE_GLOBAL;
    GET_GLOBAL;
    SET_GLOBAL;
}

LineInfo :: struct
{
    number: int;
    count: int;
}

Chunk :: struct
{
    code: [..]u8;
    lines: [..]LineInfo;
    constants: [..]Value;
}

init :: (chunk: *Chunk) {}
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

add_constant :: (chunk: *Chunk, constant: Value) -> int
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

// Value
ValueType :: enum u8
{
    NIL;
    BOOL;
    NUMBER;
    OBJ;
}

Value :: struct
{
    type: ValueType;
    union {
        boolean: bool;
        number: float64;
        obj: *Obj;
    }
}

value_to_string :: (value: Value) -> string
{
    if #complete value.type ==
    {
        case .NIL; return "nil";
        case .BOOL; return ifx value.boolean then "true" else "false";
        case .NUMBER; return tprint("%", value.number);
        case .OBJ; return object_to_string(value.obj);
    }
}

is_falsey :: (value: Value) -> bool
{
    if value.type ==
    {
        case .NIL; return true;
        case .BOOL; return !value.boolean;
        case .NUMBER; return value.number == 0;
        case .OBJ; return value.obj == null;
    }
    return false;
}

values_equal :: (a: Value, b: Value) -> bool
{
    if a.type != b.type return false;
    if #complete a.type ==
    {
        case .NIL; return true;
        case .BOOL; return a.boolean == b.boolean;
        case .NUMBER; return a.number == b.number;
        case .OBJ; return objects_equal(a.obj, b.obj);
    }
}

nil_value :: () -> Value { return .{ type = .NIL }; }
bool_value :: (boolean: bool) -> Value { return .{ type = .BOOL, boolean = boolean }; }
number_value :: (number: float64) -> Value { return .{ type = .NUMBER, number = number }; }

make_value :: (boolean: bool) -> Value { return bool_value(boolean); }
make_value :: (number: float64) -> Value { return number_value(number); }

is_number :: (value: Value) -> bool
{
    return value.type == .NUMBER;
}
