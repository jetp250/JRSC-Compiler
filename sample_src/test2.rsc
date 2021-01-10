
void someOtherFunc(char* str, int length) {
    
}

void processString(char* str, int length) {
    for int i in 0..length {
        char ch = str[i]; // get current char without advancing iterator

        if (...) {
            i += 2; // Skip two letters
            continue;
        }
        /*else if (...) {
            // get the rest of the characters starting at position 'i' (inclusive!)
            someOtherFunc(str + i, length - i - 1);
        }*/
    }
}

/*
8      16      32    64
bool
byte   short   int   long
ubyte  ushort  uint  ulong
               float double
*/
                    