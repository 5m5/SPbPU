#include <iostream>
#include <list>
#include "QueueWithPriority.h"

using namespace std;

using T = int;

void putElementToQueue(QueueWithPriority<T> &queue, T value, ElementPriority priority = ElementPriority::NORMAL) {
    const auto element = QueueElement<T>(value, priority);
    queue.putElementToQueue(element);
}

void printElement(const QueueElement<T> &element) {
    string priority;

    switch(element.priority) {
        case ElementPriority::HIGH:
            priority = "высоким";
            break;
        case ElementPriority::NORMAL:
            priority = "нормальным";
            break;
        case ElementPriority::LOW:
            priority = "низким";
            break;
        default:
            priority = "неизвестным";
            cout << "[WARNING]: в функции " << __FUNCTION__ << " получен неизвестный приоритет" << endl;
            break;
    }

    cout << "Элемент: " << element.value << " с " << priority << " приоритетом" << endl;
}

void fillQueue(QueueWithPriority<T> &queue) {
    putElementToQueue(queue, 3, ElementPriority::HIGH);
    putElementToQueue(queue, 2, ElementPriority::NORMAL);
    putElementToQueue(queue, 1, ElementPriority::LOW);
    putElementToQueue(queue, 4, ElementPriority::HIGH);
    putElementToQueue(queue, 5, ElementPriority::NORMAL);
    putElementToQueue(queue, 6, ElementPriority::LOW);
}

///Вывести на экран все элементы очереди
void printQueue(QueueWithPriority<T> &queue) {
    while(!queue.isEmpty())
        printElement(queue.getElementFromQueue());
}

void task1() {
    cout << "Задание 1" << endl;

    auto queue = QueueWithPriority<T>();

    fillQueue(queue);
    printQueue(queue);

    cout << endl;

    fillQueue(queue);
    queue.accelerate();
    printQueue(queue);
}

void task2() {
    cout << "Задание 2" << endl;

    srand(unsigned(time(0)));

    pair<int, int> range(1, 20);

    list<int> randomList;

    for(int i = 0; i < 15; ++i) {
        int randomValue = range.first + (rand() % (range.second - range.first + 1));
        randomList.push_back(randomValue);
    }

    cout << "List: ";
    for(const auto item : randomList)
        cout << item << " ";
    cout << endl;

    cout << "Вывод содержимого списка в следующем порядке: первый элемент, последний элемент, второй элемент, предпоследний элемент и т.д." << endl;

    list<int>::iterator i = randomList.begin();
    list<int>::iterator j = randomList.end();

    --j;
    while (i != j) {
        cout << *i << " " << *j << " ";
        ++i;
        --j;
    }
    --j;
    cout << *j;
}

int main() {
    task1();

    task2();

    return 0;
}
