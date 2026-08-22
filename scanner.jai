Scanner :: struct
{
    source: string;
    start: *u8;
    current: *u8;
    line: int;
}

Token :: struct
{
    type: TokenType;
    lexeme: string;
    line: int;
}

TokenType :: enum
{
    // Single-character tokens.
    LEFT_PAREN; RIGHT_PAREN;
    LEFT_BRACE; RIGHT_BRACE;
    COMMA; DOT; MINUS; PLUS;
    SEMICOLON; SLASH; STAR;

    // One or two character tokens.
    BANG; BANG_EQUAL;
    EQUAL; EQUAL_EQUAL;
    GREATER; GREATER_EQUAL;
    LESS; LESS_EQUAL;

    // Literals.
    IDENTIFIER; STRING; NUMBER;

    // Keywords.
    AND; CLASS; ELSE; FALSE;
    FOR; FUN; IF; NIL; OR;
    PRINT; RETURN; SUPER; THIS;
    TRUE; VAR; WHILE;

    ERROR; EOF;
}

scanner_init :: (scanner: *Scanner, source: string)
{
    scanner.source = source;
    scanner.start = source.data;
    scanner.current = source.data;
    scanner.line = 1;
}

scan_token :: (scanner: *Scanner) -> Token
{
    skip_whitespace(scanner);
    scanner.start = scanner.current;

    if is_at_end(scanner) return make_token(scanner, .EOF);

    c: = advance(scanner);

    if is_alpha(c) return scan_identifier(scanner);
    if is_digit(c) return scan_number(scanner);

    if c ==
    {
        case "("; return make_token(scanner, .LEFT_PAREN);
        case ")"; return make_token(scanner, .RIGHT_PAREN);
        case "{"; return make_token(scanner, .LEFT_BRACE);
        case "}"; return make_token(scanner, .RIGHT_BRACE);
        case ";"; return make_token(scanner, .SEMICOLON);
        case ","; return make_token(scanner, .COMMA);
        case "."; return make_token(scanner, .DOT);
        case "-"; return make_token(scanner, .MINUS);
        case "+"; return make_token(scanner, .PLUS);
        case "/"; return make_token(scanner, .SLASH);
        case "*"; return make_token(scanner, .STAR);

        case "!"; return make_token(scanner, ifx try_consume(scanner, "=") then .BANG_EQUAL else .BANG);
        case "="; return make_token(scanner, ifx try_consume(scanner, "=") then .EQUAL_EQUAL else .EQUAL);
        case "<"; return make_token(scanner, ifx try_consume(scanner, "=") then .LESS_EQUAL else .LESS);
        case ">"; return make_token(scanner, ifx try_consume(scanner, "=") then .GREATER_EQUAL else .GREATER);

        case "\""; return scan_string(scanner);
    }

    return make_error_token(scanner, "Unexpected character.");
}

scan_string :: (scanner: *Scanner) -> Token
{
    assert(peek(scanner, -1) == "\"");
    while !is_at_end(scanner) && peek(scanner) != "\""
    {
        if peek(scanner) == "\n" scanner.line += 1;
        advance(scanner);
    }

    if is_at_end(scanner) return make_error_token(scanner, "Unterminated string.");

    // consume closing quote
    advance(scanner);

    return make_token(scanner, .STRING);
}

scan_number :: (scanner: *Scanner) -> Token
{
    while is_digit(peek(scanner)) advance(scanner);

    if peek(scanner) == "." && is_digit(peek(scanner, 1))
    {
        advance(scanner);
        while is_digit(peek(scanner)) advance(scanner);
    }

    return make_token(scanner, .NUMBER);
}

scan_identifier :: (scanner: *Scanner) -> Token
{
    check_keyword :: (scanner: *Scanner, start_offset: int, rest: string, type: TokenType) -> TokenType
    {
        if (scanner.current - scanner.start != start_offset + rest.count) || memcmp(scanner.start + start_offset, rest.data, rest.count) != 0 
            return .IDENTIFIER;
        return type;
    }

    while is_alnum(peek(scanner)) advance(scanner);

    type: TokenType = .IDENTIFIER;
    if scanner.start[0] ==
    {
        case "a"; type = check_keyword(scanner, 1, "nd", .AND);
        case "c"; type = check_keyword(scanner, 1, "lass", .CLASS);
        case "e"; type = check_keyword(scanner, 1, "lse", .ELSE);
        case "f";
        {
            if scanner.current - scanner.start > 1
            {
                if scanner.start[1] ==
                {
                    case "a"; type = check_keyword(scanner, 2, "lse", .FALSE);
                    case "o"; type = check_keyword(scanner, 2, "r", .FOR);
                    case "u"; type = check_keyword(scanner, 2, "n", .FUN);
                }
            }
        }
        case "i"; type = check_keyword(scanner, 1, "f", .IF);
        case "n"; type = check_keyword(scanner, 1, "il", .NIL);
        case "o"; type = check_keyword(scanner, 1, "r", .OR);
        case "p"; type = check_keyword(scanner, 1, "rint", .PRINT);
        case "r"; type = check_keyword(scanner, 1, "eturn", .RETURN);
        case "s"; type = check_keyword(scanner, 1, "uper", .SUPER);
        case "t";
        {
            if scanner.current - scanner.start > 1
            {
                if scanner.start[1] ==
                {
                    case "h"; type = check_keyword(scanner, 2, "is", .THIS);
                    case "r"; type = check_keyword(scanner, 2, "ue", .TRUE);
                }
            }
        }
        case "v"; type = check_keyword(scanner, 1, "ar", .VAR);
        case "w"; type = check_keyword(scanner, 1, "hile", .WHILE);
    }

    return make_token(scanner, type);
}

is_at_end :: (scanner: *Scanner) -> bool
{
    return scanner.current >= scanner.source.data + scanner.source.count;
}

advance :: (scanner: *Scanner) -> u8
{
    scanner.current += 1;
    return scanner.current[-1];
}

skip_whitespace :: (scanner: *Scanner)
{
    while true
    {
        c: = peek(scanner);

        if c ==
        {
            case " "; #through;
            case "\r"; #through;
            case "\t"; advance(scanner);

            case "\n"; scanner.line += 1; advance(scanner);

            case "/";
            {
                if peek(scanner, 1) == "/"
                {
                    scanner.current += 2;
                    while !is_at_end(scanner) && peek(scanner) != "\n" { advance(scanner); }
                }
                else
                {
                    return;
                }
            }

            case; return;
        }
    }
}

try_consume :: (scanner: *Scanner, expected: u8) -> bool
{
    if is_at_end(scanner) return false;
    if scanner.current.* != expected return false;
    scanner.current += 1;
    return true;
}

peek :: (scanner: *Scanner, offset: int = 0) -> u8
{
    index: = scanner.current - scanner.source.data + offset;
    if index < 0 || index >= scanner.source.count return "\0";
    return scanner.source.data[index];
}

make_token :: (scanner: *Scanner, type: TokenType) -> Token
{
    return .{
        type,
        .{scanner.current - scanner.start, scanner.start },
        scanner.line
    };
}

make_error_token :: (scanner: *Scanner, message: string) -> Token
{
    return .{
        .ERROR,
        message,
        scanner.line
    };
}