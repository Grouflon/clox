
Compiler :: struct
{
    scanner: Scanner;

    compiling_chunk: *Chunk;
    previous: Token;
    current: Token;
    panic_mode: bool;
    had_error: bool;
}

Precedence :: enum
{
    NONE;
    ASSIGNMENT; // =
    OR;         // or
    AND;        // and
    EQUALITY;   // == !=
    COMPARISON; // < > <= >=
    TERM;       // + -
    FACTOR;     // * /
    UNARY;      // ! -
    CALL;       // . ()
    PRIMARY;
}

ParseFn :: #type (c: *Compiler);

ParseRule :: struct
{
    prefix: ParseFn;
    infix: ParseFn;
    precedence: Precedence;
}

rules :: ParseRule.[
    .{compile_grouping,  null,               .NONE},         // LEFT_PAREN;
    .{null,              null,               .NONE},         // RIGHT_PAREN;
    .{null,              null,               .NONE},         // LEFT_BRACE;
    .{null,              null,               .NONE},         // RIGHT_BRACE;
    .{null,              null,               .NONE},         // COMMA;
    .{null,              null,               .NONE},         // DOT;
    .{compile_unary,     compile_binary,     .TERM},         // MINUS;
    .{null,              compile_binary,     .TERM},         // PLUS;
    .{null,              null,               .NONE},         // SEMICOLON;
    .{null,              compile_binary,     .FACTOR},       // SLASH;
    .{null,              compile_binary,     .FACTOR},       // STAR;
    .{compile_unary,     null,               .NONE},         // BANG;
    .{null,              compile_binary,     .EQUALITY},     // BANG_EQUAL;
    .{null,              null,               .NONE},         // EQUAL;
    .{null,              compile_binary,     .EQUALITY},     // EQUAL_EQUAL;
    .{null,              compile_binary,     .COMPARISON},   // GREATER;
    .{null,              compile_binary,     .COMPARISON},   // GREATER_EQUAL;
    .{null,              compile_binary,     .COMPARISON},   // LESS;
    .{null,              compile_binary,     .COMPARISON},   // LESS_EQUAL;
    .{null,              null,               .NONE},         // IDENTIFIER;
    .{null,              null,               .NONE},         // STRING;
    .{compile_number,    null,               .NONE},         // NUMBER;
    .{null,              null,               .NONE},         // AND;
    .{null,              null,               .NONE},         // CLASS;
    .{null,              null,               .NONE},         // ELSE;
    .{compile_literal,   null,               .NONE},         // FALSE;
    .{null,              null,               .NONE},         // FOR;
    .{null,              null,               .NONE},         // FUN;
    .{null,              null,               .NONE},         // IF;
    .{compile_literal,   null,               .NONE},         // NIL;
    .{null,              null,               .NONE},         // OR;
    .{null,              null,               .NONE},         // PRINT;
    .{null,              null,               .NONE},         // RETURN;
    .{null,              null,               .NONE},         // SUPER;
    .{null,              null,               .NONE},         // THIS;
    .{compile_literal,   null,               .NONE},         // TRUE;
    .{null,              null,               .NONE},         // VAR;
    .{null,              null,               .NONE},         // WHILE;
    .{null,              null,               .NONE},         // ERROR;
    .{null,              null,               .NONE},         // EOF;
];


get_rule :: (type: TokenType) -> ParseRule
{
    return rules[type];
}

compile :: (source: string, chunk: *Chunk) -> bool
{
    compiler: Compiler;
    c: = *compiler;

    c.compiling_chunk = chunk;
    scanner_init(*c.scanner, source);
    advance(c);

    compile_expression(c);
    consume(c, .EOF, "Expect end of expression.");

    end_compiler(c);
    
    return !c.had_error;
}

current_chunk :: (c: *Compiler) -> *Chunk
{
    return c.compiling_chunk;
}

emit_byte :: (c: *Compiler, byte: u8)
{
    write_chunk(current_chunk(c), byte, c.previous.line);
}

emit_bytes :: (c: *Compiler, byte1: u8, byte2: u8)
{
    emit_byte(c, byte1);
    emit_byte(c, byte2);
}

emit_constant :: (c: *Compiler, value: Value)
{
    emit_bytes(c, xx OpCode.CONSTANT, make_constant(c, value));
}

make_constant :: (c: *Compiler, value: Value) -> u8
{
    constant: = add_constant(current_chunk(c), value);
    if constant > U8_MAX
    {
        error(c, "Too many constants in one chunk.");
        return 0;
    }
    return xx constant;
}

advance :: (c: *Compiler)
{
    c.previous = c.current;

    while true
    {
        c.current = scan_token(*c.scanner);
        if c.current.type != .ERROR break;

        error_at_current(c, c.current.lexeme);
    }
}

consume :: (c: *Compiler, type: TokenType, message: string)
{
    if c.current.type == type
    {
        advance(c);
        return;
    }

    error_at_current(c, message);
}

parse_precedence :: (c: *Compiler, precedence: Precedence)
{
    advance(c);
    prefix: = get_rule(c.previous.type).prefix;
    if prefix == null
    {
        error(c, "Expect expression");
        return;
    }

    prefix(c);

    while precedence <= get_rule(c.current.type).precedence
    {
        advance(c);
        infix: = get_rule(c.previous.type).infix;
        infix(c);
    }
}

compile_expression :: (c: *Compiler)
{
    parse_precedence(c, .ASSIGNMENT);
}

compile_number :: (c: *Compiler)
{
    assert(c.previous.type == .NUMBER);

    number: = string_to_float64(c.previous.lexeme);
    emit_constant(c, number_value(number));
}

compile_literal :: (c: *Compiler)
{
    if c.previous.type ==
    {
        case .FALSE; emit_byte(c, xx OpCode.FALSE);
        case .NIL; emit_byte(c, xx OpCode.NIL);
        case .TRUE; emit_byte(c, xx OpCode.TRUE);
        case; return; //unreachable
    }
}


compile_grouping :: (c: *Compiler)
{
    compile_expression(c);
    consume(c, .RIGHT_PAREN, "Expect ')' after expression.");
}

compile_unary :: (c: *Compiler)
{
    operator_type: = c.previous.type;

    parse_precedence(c, .UNARY);

    if operator_type ==
    {
        case .BANG; emit_byte(c, xx OpCode.NOT);
        case .MINUS; emit_byte(c, xx OpCode.NEGATE);
    }
}

compile_binary :: (c: *Compiler)
{
    operator_type: = c.previous.type;
    rule: = get_rule(operator_type);
    parse_precedence(c, rule.precedence + 1);

    if operator_type ==
    {
        case .BANG_EQUAL;    emit_bytes(c, xx OpCode.EQUAL, xx OpCode.NOT);
        case .EQUAL_EQUAL;   emit_byte(c, xx OpCode.EQUAL);
        case .GREATER;       emit_byte(c, xx OpCode.GREATER);
        case .GREATER_EQUAL; emit_bytes(c, xx OpCode.LESS, xx OpCode.NOT);
        case .LESS;          emit_byte(c, xx OpCode.LESS);
        case .LESS_EQUAL;    emit_bytes(c, xx OpCode.GREATER, xx OpCode.NOT);
        case .PLUS;          emit_byte(c, xx OpCode.ADD);
        case .MINUS;         emit_byte(c, xx OpCode.SUBTRACT);
        case .STAR;          emit_byte(c, xx OpCode.MULTIPLY);
        case .SLASH;         emit_byte(c, xx OpCode.DIVIDE);
        case;                return; // unreachable
    }
}

end_compiler :: (c: *Compiler)
{
    emit_byte(c, xx OpCode.RETURN);

    #if DEBUG_PRINT_CODE
    {
        if !c.had_error
        {
            disassemble_chunk(current_chunk(c), "code");
        }
    }
}

error :: (c: *Compiler, message: string)
{
    error_at(c, c.previous, message);
}

error_at_current :: (c: *Compiler, message: string)
{
    error_at(c, c.current, message);
}

error_at :: (c: *Compiler, token: Token, message: string)
{
    if c.panic_mode return;

    location: = "";
    if token.type ==
    {
        case .EOF; { location = " at end"; }
        case .ERROR; {}
        case; { location = tprint(" at '%'", token.lexeme); }
    }

    log_error("[line %] Error%: %", token.line, location, message);
    c.had_error = true;
    c.panic_mode = true;
}

#scope_file
#import "Math";