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
    GET_LOCAL;
    SET_LOCAL;
    GET_UPVALUE;
    SET_UPVALUE;
    JUMP_IF_FALSE;
    JUMP;
    LOOP;
    CALL;
    CLOSURE;
    CLOSE_UPVALUE;
    CLASS;
    GET_PROPERTY;
    SET_PROPERTY;
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
