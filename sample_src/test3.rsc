import std::Console;
import std::Optional;
import std::Optional;
import std::Vector;

struct MyStruct {
    pub int aPublicInt;
    int aPrivateInt;
}

fn MyStruct::getPrivateInt() -> int {
    return self->aPrivateInt;
}

pub fn main() {
    println("Hello World");

    let test_float = 0.987654321;
    let test_int = 0xFFFF_FFFFU;
    let test_bin = 0b1111_1111u;

    let int = requestInt("Please enter an int:");
    println("You entered ${int}!");
}

fn requestInt(msg: String) -> int {
    println(msg);

    loop {
        let optInt = Console::readLine()->toIntOpt();
        if optInt {
            return optInt->unwrap();
        }

        println("Not an integer!");
        println(msg);
    }
}