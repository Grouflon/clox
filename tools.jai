ExpandSide :: enum
{
    LEFT;
    RIGHT;
}

tprint_fixed_length_value :: (value: $T, length: int, expand_side: ExpandSide = .RIGHT) -> string
{
    return tprint_fixed_length_string(tprint("%", value), length, expand_side);
}

tprint_fixed_length_string :: (str: string, length: int, expand_side: ExpandSide = .RIGHT) -> string
{
    buf: *u8;
    buf = talloc(length);

    empty_chars: = length - str.count;

    if expand_side ==
    {
        case .LEFT;
        {
            for i: 0..length-1
            {
                char: u8 = #char " ";
                if i == (length-1) && length < str.count
                {
                    char = #char "~";
                }
                else if i < str.count
                {
                    char = str[str.count-1 - i];
                }
                buf[length-1 - i] = char; 
            }
        }

        case .RIGHT;
        {
            for i: 0..length-1
            {
                char: u8 = #char " ";
                if i == (length-1) && length < str.count
                {
                    char = #char "~";
                }
                else if i < str.count
                {
                    char = str[i];
                }
                buf[i] = char; 
            }
        }
    }

    return .{length, buf};
}

read_line :: () -> string
{
    BUFFER_SIZE :: 1024;
    buffer: [BUFFER_SIZE] u8;

    stdin := GetStdHandle(STD_INPUT_HANDLE);
    bytes_read: s32;
    if !ReadConsoleA(stdin, buffer.data, xx buffer.count, *bytes_read) || bytes_read == 0
    {
        return "";
    }

    result: string;
    result.data = talloc(bytes_read-2);
    result.count = bytes_read-2;
    memcpy(result.data, buffer.data, bytes_read-2);
    return result;
}

kernel32 :: #system_library "kernel32";
ReadConsoleA  :: (
    hConsoleHandle: HANDLE, 
    buff : *u8, 
    chars_to_read : s32,  
    chars_read : *s32, 
    lpInputControl := *void 
) -> bool #foreign kernel32;

#scope_file

#import "Windows";