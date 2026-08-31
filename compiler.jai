Compiler :: struct
{
    scanner: Scanner;

    previous: Token;
    current: Token;

    function: *ObjFunction;
    function_type: FunctionType;

    locals: [U8_MAX+1]Local;
    local_count: int;
    scope_depth: int;

    panic_mode: bool;
    had_error: bool;
}

compile :: (c: *Compiler, source: string) -> *ObjFunction
{
    c.function = xx obj_function_value().obj;
    c.function_type = .SCRIPT;

    local: = *c.locals[c.local_count];
    local.name = .{ type = .IDENTIFIER, lexeme = "" };
    local.depth = 0;
    c.local_count += 1;

    scanner_init(*c.scanner, source);
    advance(c);

    while !try_consume(c, .EOF)
    {
        compile_declaration_statement(c);
    }

    end_compiler(c);
    
    return ifx !c.had_error then c.function else null;
}

compile_declaration_statement :: (c: *Compiler)
{
    if try_consume(c, .VAR)
    {
        compile_var_statement(c);
    }
    else
    {
        compile_statement(c);
    }

    if c.panic_mode
    {
        synchronize(c);
    }
}

compile_statement :: (c: *Compiler)
{
    if try_consume(c, .PRINT)
    {
        compile_print_statement(c);
    }
    else if try_consume(c, .LEFT_BRACE)
    {
        begin_scope(c);
        compile_block_statement(c);
        end_scope(c);
    }
    else if try_consume(c, .IF)
    {
        compile_if_statement(c);
    }
    else if try_consume(c, .WHILE)
    {
        compile_while_statement(c);
    }
    else if try_consume(c, .FOR)
    {
        compile_for_statement(c);
    }
    else
    {
        compile_expression_statement(c);
    }
}

compile_var_statement :: (c: *Compiler)
{
    global: = parse_variable(c, "Expect variable name.");

    if try_consume(c, .EQUAL)
    {
        compile_expression(c);
    }
    else
    {
        emit_byte(c, xx OpCode.NIL);
    }
    consume(c, .SEMICOLON, "Expect ';' after variable declaration.");

    define_variable(c, global);
}

compile_print_statement :: (c: *Compiler)
{
    compile_expression(c);
    consume(c, .SEMICOLON, "Expect ';' after value.");
    emit_byte(c, xx OpCode.PRINT);
}

compile_expression_statement :: (c: *Compiler)
{
    compile_expression(c);
    consume(c, .SEMICOLON, "Expect ';' after value.");
    emit_byte(c, xx OpCode.POP);
}

compile_block_statement :: (c: *Compiler)
{
    while c.current.type != .RIGHT_BRACE && c.current.type != .EOF
    {
        compile_declaration_statement(c);
    }

    consume(c, .RIGHT_BRACE, "Expect '}' after block.");
}

compile_if_statement :: (c: *Compiler)
{
    consume(c, .LEFT_PAREN, "Expect '(' after 'if'.");
    compile_expression(c);
    consume(c, .RIGHT_PAREN, "Expect ')' after condition.");

    skip_then_jump: = emit_jump(c, .JUMP_IF_FALSE);
    emit_byte(c, xx OpCode.POP);
    compile_statement(c);
    skip_else_jump: = emit_jump(c, .JUMP);
    patch_jump(c, skip_then_jump);
    emit_byte(c, xx OpCode.POP);
    if try_consume(c, .ELSE)
    {
        compile_statement(c);
    }
    patch_jump(c, skip_else_jump);
}

compile_while_statement :: (c: *Compiler)
{
    loop_start: = current_chunk(c).code.count;

    consume(c, .LEFT_PAREN, "Expect '(' after 'while'.");
    compile_expression(c);
    consume(c, .RIGHT_PAREN, "Expect ')' after condition.");

    exit_jump: = emit_jump(c, .JUMP_IF_FALSE);
    emit_byte(c, xx OpCode.POP);

    compile_statement(c);
    emit_loop(c, loop_start);

    patch_jump(c, exit_jump);
    emit_byte(c, xx OpCode.POP);
}

compile_for_statement :: (c: *Compiler)
{
    begin_scope(c);
    defer end_scope(c);

    consume(c, .LEFT_PAREN, "Expect '(' after 'for'.");

    // Initializer clause
    if try_consume(c, .SEMICOLON)
    {
        // No initializer
    }
    else if try_consume(c, .VAR)
    {
        compile_var_statement(c);
    }
    else
    {
        compile_expression_statement(c);
    }

    loop_start: = current_chunk(c).code.count;
    exit_jump: = -1;

    // Condition clause
    if !try_consume(c, .SEMICOLON)
    {
        compile_expression(c);
        consume(c, .SEMICOLON, "Expect ';' after loop condition.");

        exit_jump = emit_jump(c, .JUMP_IF_FALSE);
        emit_byte(c, xx OpCode.POP);
    }

    // Increment clause
    if !try_consume(c, .RIGHT_PAREN)
    {
        body_jump: = emit_jump(c, .JUMP);

        increment_start: = current_chunk(c).code.count;
        compile_expression(c);
        emit_byte(c, xx OpCode.POP);
        consume(c, .RIGHT_PAREN, "Expect ')' after for clauses.");
        emit_loop(c, loop_start);
        loop_start = increment_start;
        
        patch_jump(c, body_jump);
    }

    compile_statement(c);
    emit_loop(c, loop_start);

    if exit_jump != -1
    {
        patch_jump(c, exit_jump);
        emit_byte(c, xx OpCode.POP);
    }

}

compile_expression :: (c: *Compiler)
{
    parse_precedence(c, .ASSIGNMENT);
}

compile_number :: (c: *Compiler, can_assign: bool)
{
    assert(c.previous.type == .NUMBER);

    number: = string_to_float64(c.previous.lexeme);
    emit_constant(c, number_value(number));
}

compile_literal :: (c: *Compiler, can_assign: bool)
{
    if c.previous.type ==
    {
        case .FALSE; emit_byte(c, xx OpCode.FALSE);
        case .NIL; emit_byte(c, xx OpCode.NIL);
        case .TRUE; emit_byte(c, xx OpCode.TRUE);
        case; return; //unreachable
    }
}

compile_string :: (c: *Compiler, can_assign: bool)
{
    assert(c.previous.type == .STRING);

    str: = c.previous.lexeme;
    // strip quotes
    str.data += 1;
    str.count -= 2;

    emit_constant(c, obj_string_value(str));
}

compile_variable :: (c: *Compiler, can_assign: bool)
{
    compile_named_variable(c, c.previous, can_assign);
}

compile_named_variable :: (c: *Compiler, name: Token, can_assign: bool)
{
    get_op, set_op: OpCode;
    arg: = resolve_local(c, name);

    if arg != -1
    {
        get_op = .GET_LOCAL;
        set_op = .SET_LOCAL;
    }
    else
    {
        arg = identifier_constant(c, name);
        get_op = .GET_GLOBAL;
        set_op = .SET_GLOBAL;
    }

    if can_assign && try_consume(c, .EQUAL)
    {
        compile_expression(c);
        emit_bytes(c, xx set_op, xx arg);
    }
    else
    {
        emit_bytes(c, xx get_op, xx arg);
    }
}

compile_grouping :: (c: *Compiler, can_assign: bool)
{
    compile_expression(c);
    consume(c, .RIGHT_PAREN, "Expect ')' after expression.");
}

compile_unary :: (c: *Compiler, can_assign: bool)
{
    operator_type: = c.previous.type;

    parse_precedence(c, .UNARY);

    if operator_type ==
    {
        case .BANG; emit_byte(c, xx OpCode.NOT);
        case .MINUS; emit_byte(c, xx OpCode.NEGATE);
    }
}

compile_binary :: (c: *Compiler, can_assign: bool)
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

compile_and :: (c: *Compiler, can_assign: bool)
{
    end_jump: = emit_jump(c, .JUMP_IF_FALSE);
    emit_byte(c, xx OpCode.POP);

    parse_precedence(c, .AND);

    patch_jump(c, end_jump);
}

compile_or :: (c: *Compiler, can_assign: bool)
{
    else_jump: = emit_jump(c, .JUMP_IF_FALSE);
    end_jump: = emit_jump(c, .JUMP);

    patch_jump(c, else_jump);
    emit_byte(c, xx OpCode.POP);

    parse_precedence(c, .AND);
    patch_jump(c, end_jump);
}

#scope_file

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

ParseFn :: #type (c: *Compiler, can_assign: bool);

ParseRule :: struct
{
    prefix: ParseFn;
    infix: ParseFn;
    precedence: Precedence;
}

Local :: struct
{
    name: Token;
    depth: int;
}

FunctionType :: enum
{
    FUNCTION;
    SCRIPT;
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
    .{compile_variable,  null,               .NONE},         // IDENTIFIER;
    .{compile_string,    null,               .NONE},         // STRING;
    .{compile_number,    null,               .NONE},         // NUMBER;
    .{null,              compile_and,        .AND},          // AND;
    .{null,              null,               .NONE},         // CLASS;
    .{null,              null,               .NONE},         // ELSE;
    .{compile_literal,   null,               .NONE},         // FALSE;
    .{null,              null,               .NONE},         // FOR;
    .{null,              null,               .NONE},         // FUN;
    .{null,              null,               .NONE},         // IF;
    .{compile_literal,   null,               .NONE},         // NIL;
    .{null,              compile_or,         .OR},           // OR;
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

current_chunk :: (c: *Compiler) -> *Chunk
{
    return *c.function.chunk;
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

emit_jump :: (c: *Compiler, instruction: OpCode) -> int
{
    address: = current_chunk(c).code.count;
    emit_byte(c, xx instruction);
    emit_byte(c, 0xff);
    emit_byte(c, 0xff);
    return address;
}

emit_loop :: (c: *Compiler, loop_start: int)
{
    offset: = current_chunk(c).code.count - loop_start + 3;
    emit_byte(c, xx OpCode.LOOP);
    emit_byte(c, 0xff);
    emit_byte(c, 0xff);
    (current_chunk(c).code.data + current_chunk(c).code.count-2).(*u16).* = offset.(u16);
}


patch_jump :: (c: *Compiler, jump_instruction_address: int)
{
    // ip will already be past the jump instruction (3 bytes) when we jump, so we offset the jump size by that.
    jump: = current_chunk(c).code.count - jump_instruction_address - 3;
    if jump > U16_MAX
    {
        error(c, "Too much code to jump over.");
    }

    (current_chunk(c).code.data + jump_instruction_address + 1).(*u16).* = jump.(u16);
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

try_consume :: (c: *Compiler, type: TokenType) -> bool
{
    if c.current.type != type return false;
    advance(c);
    return true;
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
        error(c, "Expect expression.");
        return;
    }

    can_assign: = precedence <= .ASSIGNMENT;
    prefix(c, can_assign);

    while precedence <= get_rule(c.current.type).precedence
    {
        advance(c);
        infix: = get_rule(c.previous.type).infix;
        infix(c, can_assign);
    }

    if can_assign && try_consume(c, .EQUAL)
    {
        error(c, "Invalid assignment target.");
    }
}

parse_variable :: (c: *Compiler, message: string) -> u8
{
    consume(c, .IDENTIFIER, message);

    declare_variable(c);
    if c.scope_depth > 0 return 0;

    return identifier_constant(c, c.previous);
}

define_variable :: (c: *Compiler, global: u8)
{
    if c.scope_depth > 0
    {
        c.locals[c.local_count-1].depth = c.scope_depth;
        return;
    }

    emit_bytes(c, xx OpCode.DEFINE_GLOBAL, global);
}

declare_variable :: (c: *Compiler)
{
    if c.scope_depth == 0 return;

    name: = c.previous;
    for < i: 0..c.local_count-1
    {
        local: = *c.locals[i];
        if local.depth != -1 && local.depth < c.scope_depth break;

        if name.lexeme == local.name.lexeme
        {
            error(c, "Already a variable with this name in this scope.");
        }
    }

    add_local(c, name);
}

identifier_constant :: (c: *Compiler, name: Token) -> u8
{
    return make_constant(c, obj_string_value(name.lexeme));
}

add_local :: (c: *Compiler, name: Token)
{
    if c.local_count == c.locals.count
    {
        error(c, "Too many local variables in function.");
        return;
    }

    local: = *c.locals[c.local_count];
    local.name = name;
    local.depth = -1; // uninitialized by default

    c.local_count += 1;
}

resolve_local :: (c: *Compiler, name: Token) -> int
{
    for < i: 0..c.local_count-1
    {
        local: = *c.locals[i];
        if local.name.lexeme == name.lexeme && local.depth != -1
        {
            return i;
        }
    }
    return -1;
}

begin_scope :: (c: *Compiler)
{
    c.scope_depth += 1;
}

end_scope :: (c: *Compiler)
{
    assert(c.scope_depth > 0);
    c.scope_depth -= 1;

    while c.local_count > 0 && c.locals[c.local_count-1].depth > c.scope_depth
    {
        emit_byte(c, xx OpCode.POP);
        c.local_count -= 1;
    }
}

end_compiler :: (c: *Compiler) -> *ObjFunction
{
    emit_byte(c, xx OpCode.RETURN);

    #if DEBUG_PRINT_CODE
    {
        if !c.had_error
        {
            disassemble_chunk(current_chunk(c), ifx c.function.name != null then c.function.name.str else "<script>");
        }
    }

    return c.function;
}

synchronize :: (c: *Compiler)
{
    c.panic_mode = false;

    while c.current.type != .EOF
    {
        if c.previous.type == .SEMICOLON return;
        if c.current.type ==
        {
            case .CLASS; #through;
            case .FUN; #through;
            case .VAR; #through;
            case .FOR; #through;
            case .IF; #through;
            case .WHILE; #through;
            case .PRINT; #through;
            case .RETURN;
                return;
        }
        advance(c);
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

#import "Math";