#include <iostream>
#include <vector>
#include <iterator>
#include <fstream>

using namespace std;

/**
 * Напишите алгоритм сортировки (любой простейший) содержимого вектора
 * целых чисел, используя оператор operator[].
 */
template <typename T>
void insertionSort(vector<T> &v)
{
    for(int i = 1; i < v.size(); ++i)
        for(int j = i; j > 0 && v[j-1] > v[j]; --j)
            swap(v[j-1], v[j]);
}

/**
 * Напишите алгоритм сортировки (любой простейший) содержимого вектора
 * целых чисел, используя метод at().
 */
template <typename T>
void insertionSortUsingAt(vector<T> &v)
{
    for(int i = 1; i < v.size(); ++i)
        for(int j = i - 1; j >= 0 && v.at(j) > v.at(i); --j)
            swap(v.at(j), v.at(j + 1));
}

/**
 * Напишите алгоритм сортировки (любой простейший) содержимого вектора
 * целых чисел, используя для доступа к содержимому вектора только итераторы.
 * Для работы с итераторами допустимо использовать только операторы получения
 * текущего элемента и перехода в следующему
 */
template <typename T>
void insertionSortUsingIterator(vector<T> &v)
{
    typename vector<T>::iterator iterFirst = v.begin();
    typename vector<T>::iterator iterLast = v.end();

    if(!(iterFirst < iterLast))
        return;

    for(vector<int>::iterator i = iterFirst + 1; i != iterLast; ++i)
        for(vector<int>::iterator j = i; j != iterFirst && *j < *(j - 1); --j)
            iter_swap(j - 1, j);
}

/**
 * Прочитайте во встроенный массив С содержимое текстового файла,
 * скопируйте данные в вектор одной строкой кода (без циклов и алгоритмов STL)
 */
template <typename T>
void readFromFile(vector<T> &v)
{
    ifstream inputFile("data.txt");

    if(!inputFile.good())
        return;

    T current_number = 0;
    while(inputFile >> current_number)
        v.push_back(current_number);

    inputFile.close();
}

/**
 * Напишите программу, сохраняющую в векторе числа, полученные из стандартного
 * ввода (окончанием ввода является число 0). Удалите все элементы,
 * которые делятся на 2 (не используете стандартные алгоритмы STL),
 * если последнее число 1. Если последнее число 2,
 * добавьте после каждого числа которое делится на 3 три единицы.
 */
template <typename T>
void readVectorFromKeyboard(vector<T> &v)
{
    T number;
    while(number != 0)
    {
        cin >> number;
        v.push_back(number);
    }
    v.pop_back();

    if(v.back() == 1) {
        for(auto itr = v.begin(); itr != v.end(); ++itr)
            if(*itr % 2 == 0)
                v.erase(itr);
    }
    else if(v.back() == 2)
        for(auto itr = v.begin(); itr != v.end(); ++itr)
            if(*itr % 3 == 0)
                v.insert(itr + 1, 3, 1);
}

/**
 * Напишите функцию void fillRandom(double* array, int size) заполняющую массив
 * случайными значениями в интервале от -1.0 до 1.0. Заполните с помощью
 * заданной функции вектора размером 5,10,25,50,100 и отсортируйте его
 * содержимое (с помощью любого разработанного ранее алгоритма модифицированного
 * для сортировки действительных чисел) 
 */
template <typename T>
void fillRandom(T *array, int size)
{
    srand(time_t(0));

    for(int i = 0; i < size; i++)
        array[i] = static_cast<T>(rand()) / RAND_MAX * (1 + 1) - 1;
}

/**
 * Вывод на экран
 */
template <typename T>
void printVector(vector<T> &v)
{
    for(T number : v)
        cout << number << " ";
    cout << endl;
}

int main()
{
    vector<int> vectorFromFile;
    vector<int> v = vectorFromFile;
    readFromFile(vectorFromFile);

    cout << "Вектор из файла:" << endl;
    printVector(vectorFromFile);
    cout << endl;

    v = vectorFromFile;
    cout << "Сортировка вставками, используя operator[]" << endl;
    double startTime = clock();
    insertionSort(v);
    double endTime = clock();
    printVector(v);
    cout << "Время выполнения: " << endTime - startTime << " мсек" << endl << endl;

    v = vectorFromFile;
    cout << "Сортировка вставками, используя at()" << endl;
    startTime = clock();
    insertionSortUsingAt(v);
    endTime = clock();
    printVector(v);
    cout << "Время выполнения: " << endTime - startTime << " мсек" << endl << endl;

    v = vectorFromFile;
    cout << "Сортировка вставками, используя итераторы" << endl;
    startTime = clock();
    insertionSortUsingIterator(v);
    endTime = clock();
    printVector(v);
    cout << "Время выполнения: " << endTime - startTime << " мсек" << endl << endl;

    vector<int> vectorFromKeyboard;
    readVectorFromKeyboard(vectorFromKeyboard);
    cout << "Ввод с клавиатуры:" << endl;
    printVector(vectorFromKeyboard);

    double array[25];
    fillRandom(array, 25);
    cout << "Вектор случайных чисел:" << endl;
    auto vec = vector<double>(array, array + sizeof(array) / sizeof(array[0]));
    printVector(vec);
    insertionSortUsingAt(vec);
    cout << "Отсортированный вектор случайных чисел:" << endl;
    printVector(vec);
}
