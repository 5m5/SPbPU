#include <iostream>

unsigned getByte(unsigned long x, int n) {
    return x >> n;
}

int main() {
    std::cout << std::hex << getByte(0x12345678,1) << std::endl;
    return 0;
}