#include <iostream>

char *customStrncat(char *dst, const char *src, size_t n) {
    if (n != 0) {
        char *d = dst;
        const char *s = src;

        while(*d != 0)
            d++;
        do {
            if((*d = *s++) == 0)
                break;
            d++;
        } while(--n != 0);
        *d = 0;
    }
    return dst;
}

int main() {
    char string1[] = {'H', 'e', 'l', 'l', 'o'};
    char string2[] = {' ', 'w', 'o', 'r', 'l', 'd'};
    std::cout << strncat(string1, string2, 10);
    //std::cout << customStrncat(string1, string2, 10) << std::endl;

    return 0;
}
