Tokenizer :: struct
{
    scanner: Scanner;
    previous: Token;
    current: Token;
}

tokenizer_init :: (t: *Tokenizer, source: string)
{
    scanner_init(*t.scanner, source);
    advance(t);
}

Compiler :: struct
{
    tokenizer: *Tokenizer;
    parent: *Compiler;

    function: *ObjFunction;
    function_type: FunctionType;

    locals: [U8_MAX+1]Local;
    local_count: int;
    upvalues: [U8_MAX+1]Upvalue;
    scope_depth: int;

    panic_mode: bool;
    had_error: bool;
}

compile :: (c: *Compiler, tokenizer: *Tokenizer) -> *ObjFunction
{
    begin_compiler(c, tokenizer, .SCRIPT);

    while !try_consume(c, .EOF)
    {
        compile_declaration_statement(c);
    }

    f: = end_compiler(c);
    
    return ifx !c.had_error then f else null;
}

compile_declaration_statement :: (c: *Compiler)
{
    if try_consume(c, .FUN)
    {
        compile_fun_statement(c);
    }
    else if try_consume(c, .VAR)
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
    else if try_consume(c, .RETURN)
    {
        compile_return_statement(c);
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

compile_fun_statement :: (c: *Compiler)
{
    global: = parse_variable(c, "Expect variable name.");
    mark_local_initialized(c); // allow the function to reference itself

    compile_function(c, .FUNCTION);

    define_variable(c, global);
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

compile_return_statement :: (c: *Compiler)
{
    if c.function_type == .SCRIPT
    {
        error(c, "Can't return from top-level code.");
    }

    if try_consume(c, .SEMICOLON)
    {
        emit_return(c);
    }
    else
    {
        compile_expression(c);
        consume(c, .SEMICOLON, "Expect ';' after return value");
        emit_byte(c, xx OpCode.RETURN);
    }
}

compile_expression_statement :: (c: *Compiler)
{
    compile_expression(c);
    consume(c, .SEMICOLON, "Expect ';' after value.");
    emit_byte(c, xx OpCode.POP);
}

compile_block_statement :: (c: *Compiler)
{
    while current_token(c).type != .RIGHT_BRACE && current_token(c).type != .EOF
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

compile_function :: (parent_compiler: *Compiler, type: FunctionType)
{
    compiler: Compiler;
    c: = *compiler;
    c.parent = parent_compiler;
    begin_compiler(c, parent_compiler.tokenizer, type);
    begin_scope(c); // no need to end scope, compiler will disappear just after

    consume(c, .LEFT_PAREN, "Expect '(' after function name.");
    if current_token(c).type != .RIGHT_PAREN
    {
        while true
        {
            c.function.arity += 1;
            if c.function.arity > 255
            {
                error_at_current(c, "Can't have more than 255 parameters.");
            }
            constant: = parse_variable(c, "Expect parameter name");
            define_variable(c, constant);

            if !try_consume(c, .COMMA) break;
        }

    }
    consume(c, .RIGHT_PAREN, "Expect ')' after parameters.");
    consume(c, .LEFT_BRACE, "Expect '{' before function body.");

    compile_block_statement(c);

    f: = end_compiler(c);

    c = parent_compiler;
    emit_bytes(c, xx OpCode.CLOSURE, make_constant(c, obj_value(f)));

    for i: 0..f.upvalue_count-1
    {
        emit_byte(c, xx ifx compiler.upvalues[i].is_local then 1 else 0);
        emit_byte(c, compiler.upvalues[i].index);
    }
}

compile_expression :: (c: *Compiler)
{
    parse_precedence(c, .ASSIGNMENT);
}

compile_number :: (c: *Compiler, can_assign: bool)
{
    assert(previous_token(c).type == .NUMBER);

    number: = string_to_float64(previous_token(c).lexeme);
    emit_constant(c, number_value(number));
}

compile_literal :: (c: *Compiler, can_assign: bool)
{
    if previous_token(c).type ==
    {
        case .FALSE; emit_byte(c, xx OpCode.FALSE);
        case .NIL; emit_byte(c, xx OpCode.NIL);
        case .TRUE; emit_byte(c, xx OpCode.TRUE);
        case; return; //unreachable
    }
}

compile_string :: (c: *Compiler, can_assign: bool)
{
    assert(previous_token(c).type == .STRING);

    str: = previous_token(c).lexeme;
    // strip quotes
    str.data += 1;
    str.count -= 2;

    emit_constant(c, obj_string_value(str));
}

compile_variable :: (c: *Compiler, can_assign: bool)
{
    compile_named_variable(c, previous_token(c), can_assign);
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
        arg = resolve_upvalue(c, name);
        if arg != -1
        {
            get_op = .GET_UPVALUE;
            set_op = .SET_UPVALUE;
        }
        else
        {
            arg = identifier_constant(c, name);
            get_op = .GET_GLOBAL;
            set_op = .SET_GLOBAL;
        }
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
    operator_type: = previous_token(c).type;

    parse_precedence(c, .UNARY);

    if operator_type ==
    {
        case .BANG; emit_byte(c, xx OpCode.NOT);
        case .MINUS; emit_byte(c, xx OpCode.NEGATE);
    }
}

compile_binary :: (c: *Compiler, can_assign: bool)
{
    operator_type: = previous_token(c).type;
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

compile_call :: (c: *Compiler, can_assign: bool)
{
    arg_count: = argument_list(c);
    emit_bytes(c, xx OpCode.CALL, arg_count);
}

argument_list :: (c: *Compiler) -> u8
{
    arg_count: u8;
    if current_token(c).type != .RIGHT_PAREN
    {
        while true
        {
            compile_expression(c);
            if arg_count == 255
            {
                error(c, "Can't have more than 255 arguments.");
            }

            arg_count += 1;

            if !try_consume(c, .COMMA) break;
        }
    }
    consume(c, .RIGHT_PAREN, "Expect ')' after arguments.");
    return arg_count;
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
    is_captured: bool;
}

Upvalue :: struct
{
    index: u8;
    is_local: bool;
}

FunctionType :: enum
{
    FUNCTION;
    SCRIPT;
}

rules :: ParseRule.[
    .{compile_grouping,  compile_call,       .CALL},         // LEFT_PAREN;
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
    write_chunk(current_chunk(c), byte, previous_token(c).line);
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

emit_return :: (c: *Compiler)
{
    emit_byte(c, xx OpCode.NIL);
    emit_byte(c, xx OpCode.RETURN);
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

advance :: (t: *Tokenizer)
{
    t.previous = t.current;
    t.current = scan_token(*t.scanner);
}

advance :: (c: *Compiler)
{
    advance(c.tokenizer);

    if current_token(c).type == .ERROR
    {
        error_at_current(c);
    }
}

try_consume :: (c: *Compiler, type: TokenType) -> bool
{
    if current_token(c).type != type return false;
    advance(c);
    return true;
}

consume :: (c: *Compiler, type: TokenType, message: string)
{
    if current_token(c).type == type
    {
        advance(c);
        return;
    }

    error_at_current(c, message);
}

current_token :: (c: *Compiler) -> Token
{
    return c.tokenizer.current;
}

previous_token :: (c: *Compiler) -> Token
{
    return c.tokenizer.previous;
}

parse_precedence :: (c: *Compiler, precedence: Precedence)
{
    advance(c);
    prefix: = get_rule(previous_token(c).type).prefix;
    if prefix == null
    {
        error(c, "Expect expression.");
        return;
    }

    can_assign: = precedence <= .ASSIGNMENT;
    prefix(c, can_assign);

    while precedence <= get_rule(current_token(c).type).precedence
    {
        advance(c);
        infix: = get_rule(previous_token(c).type).infix;
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

    return identifier_constant(c, previous_token(c));
}

define_variable :: (c: *Compiler, global: u8)
{
    mark_local_initialized(c);
    if c.scope_depth > 0 return;

    emit_bytes(c, xx OpCode.DEFINE_GLOBAL, global);
}

declare_variable :: (c: *Compiler)
{
    if c.scope_depth == 0 return;

    name: = previous_token(c);
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

mark_local_initialized :: (c: *Compiler)
{
    if c.scope_depth == 0 return;
    c.locals[c.local_count-1].depth = c.scope_depth;
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

add_upvalue :: (c: *Compiler, index: u8, is_local: bool) -> int
{
    upvalue_count: = c.function.upvalue_count;

    for i: 0..upvalue_count-1
    {
        upvalue: = *c.upvalues[i];
        if upvalue.index == index && upvalue.is_local == is_local
            return i;
    }

    if upvalue_count >= c.upvalues.count
    {
        error(c, "Too many closure variables in function.");
        return 0;
    }

    c.upvalues[upvalue_count].is_local = is_local;
    c.upvalues[upvalue_count].index = index;
    c.function.upvalue_count += 1;

    return upvalue_count;
}

resolve_upvalue :: (c: *Compiler, name: Token) -> int
{
    if c.parent == null return -1;

    local: = resolve_local(c.parent, name);
    if local != -1
    {
        c.parent.locals[local].is_captured = true;
        return add_upvalue(c, xx local, true);
    }

    upvalue: = resolve_upvalue(c.parent, name);
    if upvalue != -1
    {
        return add_upvalue(c, xx upvalue, false);
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
        if c.locals[c.local_count-1].is_captured
        {
            emit_byte(c, xx OpCode.CLOSE_UPVALUE);
        }
        else
        {
            emit_byte(c, xx OpCode.POP);
        }
        c.local_count -= 1;
    }
}

begin_compiler :: (c: *Compiler, tokenizer: *Tokenizer, function_type: FunctionType)
{
    c.tokenizer = tokenizer;
    if current_token(c).type == .ERROR
    {
        error_at_current(c);
    }

    c.function = xx obj_function_value().obj;
    c.function_type = function_type;

    local: = *c.locals[c.local_count];
    local.name = .{ type = .IDENTIFIER, lexeme = "" };
    local.depth = 0;
    c.local_count += 1;

    if c.function_type == .FUNCTION
    {
        assert(previous_token(c).type == .IDENTIFIER);
        c.function.name = xx obj_string_value(previous_token(c).lexeme).obj;
    }
}

end_compiler :: (c: *Compiler) -> *ObjFunction
{
    emit_return(c);

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

    while current_token(c).type != .EOF
    {
        if previous_token(c).type == .SEMICOLON return;
        if current_token(c).type ==
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
    error_at(c, previous_token(c), message);
}

error_at_current :: (c: *Compiler)
{
    error_at(c, current_token(c), current_token(c).lexeme);
}

error_at_current :: (c: *Compiler, message: string)
{
    error_at(c, current_token(c), message);
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