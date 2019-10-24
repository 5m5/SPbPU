#include <iostream>

template<class T>
T sum(T *array, int dim, int stringNumber)
{
    T sum = 0;
    for(int i = 0; i < dim; i++)
        sum += array[i];

    return sum;
}

int main() {
    float array[3][3];
    
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
    
    int stringNumber;
    std::cout << "Enter the number of string" << std::endl;
    std::cin >> stringNumber;
    
    if(stringNumber >= 3 && stringNumber < 0) {
        std::cout << "Wrong string number" << std::endl;
        return 0;
    }

    std::cout << sum(array[stringNumber], 3, stringNumber) << std::endl;

    return 0;
}
