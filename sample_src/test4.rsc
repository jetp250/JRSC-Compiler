fn main() {
    println("Hello World");

    let test_float = 0.987654321;
    let test_char = 'Q';

    let test_exp = (3 - test_float) * 5 + requestInt('P');
    test_exp /= 2;
    test_exp = test_exp * 2;

    println("test_exp is ${test_exp}");

    let int = requestInt("Please enter an int:");
    println("You entered ${int}!");
}