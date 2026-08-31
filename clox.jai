#import "Basic"()(MEMORY_DEBUGGER = DEBUG_MEMORY);

#load "chunk.jai";
#load "object.jai";
#load "vm.jai";
#load "compiler.jai";
#load "scanner.jai";
#load "tools.jai";

DEBUG_MEMORY :: true;
DEBUG_PRINT_CODE :: true;
DEBUG_TRACE_EXECUTION :: true;

main :: ()
{
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

        array_free(args);
        free_objects();
        deinit(*strings);
    }

    // Memory leak check
    #if DEBUG_MEMORY
    {
        report: = make_leak_report();
        if report.sorted_summaries.count
        {
            log_error("\n=========\nMemory Leak!!\n=========");
            log_leak_report(report);
            assert(false);
        }
    }
}

repl :: ()
{
    vm: VM;
    init(*vm);
    defer shutdown(*vm);

    while true
    {
        print("> ");
        line: = read_line();
        
        if (line.count == 1 && line[0] == 0x4) || line == "exit" break;

        compiler: Compiler;
        f: = compile(*compiler, line);
        if f == null
        {
            continue;
        }

        interpret(*vm, f);
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

    // Compile
    compiler: Compiler;
    f: = compile(*compiler, data);
    if f == null
    {
        return .COMPILE_ERROR;
    }

    // Run
    vm: VM;
    init(*vm);
    defer shutdown(*vm);
    return interpret(*vm, f);
}

#scope_file

#import "File";