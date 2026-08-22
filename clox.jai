#import "Basic";

#load "chunk.jai";
#load "vm.jai";
#load "compiler.jai";
#load "scanner.jai";
#load "tools.jai";

DEBUG_PRINT_CODE :: true;
DEBUG_TRACE_EXECUTION :: true;

main :: ()
{
    args: = get_command_line_arguments();
    if args.count ==
    {
        case 1;
        {
            repl();
        }
        case 2;
        {
            result: = run_file(args[1]);
            if result ==
            {
                case .COMPILE_ERROR; exit(65);
                case .RUNTIME_ERROR; exit(70);
            }
        }
        case;
        {
            print("Usage: clox [script]");
        }
    }
}

repl :: ()
{
    // interpreter: Interpreter;
    // initialize(*interpreter);
    // defer shutdown(*interpreter);

    while true
    {
        print("> ");
        line: = read_line();
        
        if (line.count == 1 && line[0] == 0x4) || line == "exit" break;

        result: = interpret(line);
        // run(*interpreter, line);

        // if !interpreter.has_error && interpreter.has_last_statement_value
        // {
        //     log("%", to_string(interpreter.last_statement_value));
        // }

        // interpreter.has_error = false;
    }
    
}

run_file :: (path: string) -> InterpretResult
{
    data, success: = read_entire_file(path);
    defer free(data);

    if !success
    {
        log_error("Failed to read source file \"%\"", path);
        exit(74);
        return .COMPILE_ERROR;
    }

    return interpret(data);
}

interpret :: (source: string) -> InterpretResult
{
    // Compile
    chunk: Chunk;
    init(*chunk);
    defer shutdown(*chunk);

    if !compile(source, *chunk)
    {
        return .COMPILE_ERROR;
    }

    // Run
    vm: VM;
    init(*vm);
    defer shutdown(*vm);

    return interpret(*vm, *chunk);
}

#scope_file

#import "File";