#import "Basic"()(MEMORY_DEBUGGER = DEBUG_MEMORY);

#load "chunk.jai";
#load "object.jai";
#load "vm.jai";
#load "compiler.jai";
#load "scanner.jai";
#load "tools.jai";
#load "debug.jai";

DEBUG_MEMORY :: true;
DEBUG_PRINT_CODE :: true;
DEBUG_TRACE_EXECUTION :: true;
DEBUG_STRESS_GC :: false;
DEBUG_LOG_GC :: true;

main :: ()
{
    {
        args: = get_command_line_arguments();
        defer array_free(args);

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

    ctx := context;
    ctx.allocator.proc = clox_allocator_proc;
    ctx.allocator.data = *vm;
    push_context ctx
    {
        init(*vm);
        defer shutdown(*vm);

        while true
        {
            print("> ");
            line: = read_line();
            
            if (line.count == 1 && line[0] == 0x4) || line == "exit" break;

            tokenizer: Tokenizer;
            tokenizer_init(*tokenizer, line);

            compiler: Compiler;
            f: = compile(*compiler, *tokenizer, *vm);
            if f == null
            {
                continue;
            }

            interpret(*vm, f);
        }
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

    vm: VM;

    ctx := context;
    ctx.allocator.proc = clox_allocator_proc;
    ctx.allocator.data = *vm;
    push_context ctx
    {
        init(*vm);
        defer shutdown(*vm);

        // Compile
        tokenizer: Tokenizer;
        tokenizer_init(*tokenizer, data);

        compiler: Compiler;
        f: = compile(*compiler, *tokenizer, *vm);
        if f == null
        {
            return .COMPILE_ERROR;
        }

        // Run
        return interpret(*vm, f);
    }
}

#scope_file

#import "File";