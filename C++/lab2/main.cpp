#include <iostream>

int sum(int *array, int dim, int string)
{
    int sum = 0;
    for(int i = string; i < dim; i++)
        sum += array[i];

    return sum;
}

int main() {
    int array[3][3];
    for(int i = 0; i < 3; i++)
        for(int j = 0; j < 3; j++) {
            std::cout << "array[" << i << "][" << j << "]: ";
            std::cin >> array[i][j];
        }

    for(int i = 0; i < 3; i++) {
        for(int j = 0; j < 3; j++)
            std::cout << array[i][j] << " ";
        std::cout << std::endl;
    }

    std::cout << sum(*array, 3, 0);

    return 0;
}