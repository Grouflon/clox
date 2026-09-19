objects: *Obj;
strings: Table(u32, *ObjString);

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

shutdown :: (obj :*Obj)
{
    if obj.type ==
    {
        case .STRING; free(obj.(*ObjString).str);
        case .FUNCTION; shutdown(*obj.(*ObjFunction).chunk);
        case .CLOSURE; free(obj.(*ObjClosure).upvalues.data);
    }
}

free_objects :: ()
{
    obj: = objects;
    while obj != null
    {
        next: = obj.next;

        shutdown(obj);
        free(obj);

        obj = next;
    }
    objects = null;
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

new_object :: ($T: Type) -> *T
{
    obj: = New(T);
    obj.base.next = objects;
    objects = obj;
    return obj;
}

new_obj_closure :: (function: *ObjFunction) -> *ObjClosure
{
    closure: = new_object(ObjClosure);
    closure.function = function;
    closure.upvalues.count = function.upvalue_count;
    size: = size_of(*ObjUpvalue) * function.upvalue_count;
    closure.upvalues.data = alloc(size);
    memset(closure.upvalues.data, 0, size);
    return closure;
}

obj_value :: (obj: *Obj) -> Value
{
    return .{
        type = .OBJ,
        obj = obj
    };
}

obj_string_value :: (str: string) -> Value
{
    hash: = get_hash(str);
    found, obj: = table_find(*strings, hash);
    if !found
    {
        obj = new_object(ObjString);
        obj.str = copy_string(str);
        obj.hash = hash;

        table_add(*strings, hash, obj);
    }
    else
    {
        assert(compare(str, obj.str) == 0, "Hash collision !"); 
    }

    return obj_value(obj);
}

obj_function_value :: () -> Value
{
    obj: = new_object(ObjFunction);
    return obj_value(obj);
}

obj_native_value :: (function: NativeFn) -> Value
{
    obj: = new_object(ObjNative);
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