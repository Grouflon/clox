objects: *Obj;
strings: Table(u32, *ObjString);

ObjType :: enum
{
    STRING;
}

Obj :: struct
{
    type: ObjType;
    next: *Obj;
}

ObjString :: struct
{
    #as using base: Obj;
    str: string;
    hash: u32;
}

shutdown :: (obj :*Obj)
{
    if #complete obj.type ==
    {
        case .STRING; free(obj.(*ObjString).str);
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
    }
    return "";
}

objects_equal :: (a: *Obj, b: *Obj) -> bool
{
    if a == b return true;
    if a == null || b == null return false;
    if a.type != b.type return false;

    if #complete a.type ==
    {
        case .STRING; return a.(*ObjString).hash == b.(*ObjString).hash;
    }
}

is_string :: (value: Value) -> bool
{
    if value.type != .OBJ return false;
    if value.obj == null return false;
    return value.obj.type == .STRING;
}

obj_string_value :: (str: string) -> Value
{
    hash: = get_hash(str);
    found, obj: = table_find(*strings, hash);
    if !found
    {
        obj = New(ObjString);
        obj.type = .STRING;
        obj.str = copy_string(str);
        obj.hash = hash;
        obj.next = objects;
        objects = obj;

        table_add(*strings, hash, obj);
    }
    else
    {
        assert(compare(str, obj.str) == 0, "Hash collision !"); 
    }

    return .{
        type = .OBJ,
        obj = obj
    };
}

as_obj_string :: (value: Value) -> *ObjString
{
    assert(is_string(value));
    return value.obj.(*ObjString);
}

#scope_file

#import "Hash_Table";
#import "Hash";
#import "String";