#include <iostream>

char *customStrncat(char * __restrict dst, const char * __restrict src, size_t n){
    if (n != 0) {
        char *d = dst;
        const char *s = src;

        while (*d != 0)
            d++;
        do {
            if ((*d = *s++) == 0)
                break;
            d++;
        } while (--n != 0);
        *d = 0;
    }
    return (dst);
}

int main(int argc, char *argv[]) {
    int n = atoi(argv[3]);
    std::cout << customStrncat(argv[1], argv[2], n) << std::endl;

    return 0;
}
