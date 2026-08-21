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
