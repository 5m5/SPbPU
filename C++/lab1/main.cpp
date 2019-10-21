#include <iostream>

int main() {
    char string1[] = {'H', 'e', 'l', 'l', 'o'};
    char string2[] = {' ', 'w', 'o', 'r', 'l', 'd'};
    std::cout << strncat(string1, string2, 6);

    return 0;
}