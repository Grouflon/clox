ObjType :: enum
{
    STRING;
    FUNCTION;
    CLOSURE;
    NATIVE;
    UPVALUE;
}

Obj :: struct
{
    type: ObjType;
    next: *Obj;
    is_marked: bool;
}

ObjString :: struct
{
    #as using base: Obj;
    type = .STRING;

    str: string;
    hash: u32;

}

ObjFunction :: struct
{
    #as using base: Obj;
    type = .FUNCTION;

    arity: int;
    upvalue_count: int;
    chunk: Chunk;
    name: *ObjString;
}

ObjClosure :: struct
{
    #as using base: Obj;
    type = .CLOSURE;

    function: *ObjFunction;
    upvalues: []*ObjUpvalue;
}

ObjUpvalue :: struct
{
    #as using base: Obj;
    type = .UPVALUE;

    location: *Value;
    closed: Value;
    next_upvalue: *ObjUpvalue;
}

NativeFn :: #type (arg_count: int, args: *Value) -> Value;

ObjNative :: struct
{
    #as using base: Obj;
    type = .NATIVE;

    function: NativeFn;
}

free_object :: (vm: *VM, obj: *Obj)
{
    #if DEBUG_LOG_GC
    {
        log("% free type %", obj, obj.type);
    }

    if obj.type ==
    {
        case .STRING; free(obj.(*ObjString).str);
        case .FUNCTION; shutdown(*obj.(*ObjFunction).chunk);
        case .CLOSURE; array_free(obj.(*ObjClosure).upvalues);
    }
    free(obj);
}

object_to_string :: (obj: *Obj) -> string
{
    if #complete obj.type ==
    {
        case .STRING; return obj.(*ObjString).str;
        case .FUNCTION; return ifx obj.(*ObjFunction).name != null then tprint("<fn %>",  obj.(*ObjFunction).name.str) else "<script>";
        case .CLOSURE; return object_to_string(obj.(*ObjClosure).function);
        case .NATIVE; return tprint("<native fn %>", obj.(*ObjNative).function);
        case .UPVALUE; return "upvalue";
    }
    return "";
}

objects_equal :: (a: *Obj, b: *Obj) -> bool
{
    return a == b;
    // if a == b return true;
    // if a == null || b == null return false;
    // if a.type != b.type return false;

    // return false;
}

is_string :: (value: Value) -> bool
{
    if value.type != .OBJ return false;
    if value.obj == null return false;
    return value.obj.type == .STRING;
}

is_function :: (value: Value) -> bool
{
    if value.type != .OBJ return false;
    if value.obj == null return false;
    return value.obj.type == .FUNCTION;
}

is_closure :: (value: Value) -> bool
{
    if value.type != .OBJ return false;
    if value.obj == null return false;
    return value.obj.type == .CLOSURE;
}

is_native :: (value: Value) -> bool
{
    if value.type != .OBJ return false;
    if value.obj == null return false;
    return value.obj.type == .NATIVE;
}

new_object :: (vm: *VM, $T: Type) -> *T
{
    obj: = New(T);
    push(vm, obj_value(obj));
    defer pop(vm);

    obj.base.next = vm.objects;
    vm.objects = obj;

    #if DEBUG_LOG_GC
    {
        log("% allocate % for %", obj, size_of(T), obj.type);
    }

    return obj;
}

new_obj_closure :: (vm: *VM, function: *ObjFunction) -> *ObjClosure
{
    closure: = new_object(vm, ObjClosure);
    push(vm, obj_value(closure));
    defer pop(vm);

    closure.function = function;

    size: = size_of(*ObjUpvalue) * function.upvalue_count;
    closure.upvalues.data = alloc(size);
    memset(closure.upvalues.data, 0, size);
    closure.upvalues.count = function.upvalue_count;
    return closure;
}

obj_value :: (obj: *Obj) -> Value
{
    return .{
        type = .OBJ,
        obj = obj
    };
}

obj_string_value :: (vm: *VM, str: string) -> Value
{
    hash: = get_hash(str);
    found, obj: = table_find(*vm.strings, hash);
    if !found
    {
        obj = new_object(vm, ObjString);
        push(vm, obj_value(obj));
        defer pop(vm);

        obj.str = copy_string(str);
        obj.hash = hash;

        table_add(*vm.strings, hash, obj);
    }
    else
    {
        assert(compare(str, obj.str) == 0, "Hash collision !"); 
    }

    return obj_value(obj);
}

obj_function_value :: (vm: *VM) -> Value
{
    obj: = new_object(vm, ObjFunction);
    return obj_value(obj);
}

obj_native_value :: (vm: *VM, function: NativeFn) -> Value
{
    obj: = new_object(vm, ObjNative);
    obj.function = function;
    return obj_value(obj);
}

as_obj_string :: (value: Value) -> *ObjString
{
    assert(is_string(value));
    return value.obj.(*ObjString);
}

as_obj_function :: (value: Value) -> *ObjFunction
{
    assert(is_function(value));
    return value.obj.(*ObjFunction);
}

as_obj_closure :: (value: Value) -> *ObjClosure
{
    assert(is_closure(value));
    return value.obj.(*ObjClosure);
}

as_obj_native :: (value: Value) -> *ObjNative
{
    assert(is_native(value));
    return value.obj.(*ObjNative);
}

#scope_file

#import "Hash_Table";
#import "Hash";
#import "String";