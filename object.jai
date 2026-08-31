objects: *Obj;
strings: Table(u32, *ObjString);

ObjType :: enum
{
    STRING;
    FUNCTION;
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
    chunk: Chunk;
    name: *ObjString;

}

shutdown :: (obj :*Obj)
{
    if #complete obj.type ==
    {
        case .STRING; free(obj.(*ObjString).str);
        case .FUNCTION; shutdown(*obj.(*ObjFunction).chunk);
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

        obj = obj.next;
    }
    objects = null;
}

object_to_string :: (obj: *Obj) -> string
{
    if #complete obj.type ==
    {
        case .STRING; return obj.(*ObjString).str;
        case .FUNCTION; return ifx obj.(*ObjFunction).name != null then tprint("<fn %>",  obj.(*ObjFunction).name.str) else "<script>";
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

new_object :: ($T: Type) -> *T
{
    obj: = New(T);
    obj.next = objects;
    objects = obj;
    return obj;
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

#scope_file

#import "Hash_Table";
#import "Hash";
#import "String";