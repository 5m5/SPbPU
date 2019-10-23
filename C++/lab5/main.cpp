#include <iostream>

unsigned getByte(unsigned long x, int n) {
    x = x >> 8 * n;
    x = x << 8 * 3;
    return x;
}

int main() {
    std::cout << std::hex << getByte(0x12345678,1) << std::endl;
    return 0;
}
