//
// Created by Mikhail Andreev on 15.03.2020.
//

#ifndef LAB3_QUEUEWITHPRIORITY_H
#define LAB3_QUEUEWITHPRIORITY_H

#include <deque>

typedef enum
{
    LOW,
    NORMAL,
    HIGH
} ElementPriority;

template <typename T>
struct QueueElement
{
    T value;
    ElementPriority priority;

    QueueElement(T value, ElementPriority priority) {
        this->value = value;
        this->priority = priority;
    }
};

template <typename T>
class QueueWithPriority {

public:
    QueueWithPriority() = default;

    /// Добавить в очередь элемент element с приоритетом priority
    void putElementToQueue(const QueueElement<T> &element);

    /**
     * Получить элемент из очереди
     * метод должен возвращать элемент с наибольшим приоритетом, который был
     * добавлен в очередь раньше других
     */
    QueueElement<T> getElementFromQueue();

    /// Выполнить акселерацию
    void accelerate();

    ///Возвращает true, если все три очереди пустые, иначе - false
    bool isEmpty() const;

private:
    std::deque<QueueElement<T>> m_lowPriorityDeque;
    std::deque<QueueElement<T>> m_normalPriorityDeque;
    std::deque<QueueElement<T>> m_highPriorityDeque;
};

template <typename T>
void QueueWithPriority<T>::putElementToQueue(const QueueElement<T> &element) {
    switch(element.priority) {
        case LOW:
            m_lowPriorityDeque.push_back(element);
            break;
        case HIGH:
            m_highPriorityDeque.push_back(element);
            break;
        case NORMAL:
        default:
            m_normalPriorityDeque.push_back(element);
            break;
    }
}

template <typename T>
QueueElement<T> QueueWithPriority<T>::getElementFromQueue() {
    for(auto element : m_highPriorityDeque) {
        m_highPriorityDeque.pop_front();
        return element;
    }

    for(auto element : m_normalPriorityDeque) {
        m_normalPriorityDeque.pop_front();
        return element;
    }

    for(auto element : m_lowPriorityDeque) {
        m_lowPriorityDeque.pop_front();
        return element;
    }
}

template <typename T>
void QueueWithPriority<T>::accelerate() {
    for(auto element : m_lowPriorityDeque) {
        element.priority = ElementPriority::HIGH;
        m_highPriorityDeque.push_back(element);
        m_lowPriorityDeque.pop_back();
    }
}

template <typename T>
bool QueueWithPriority<T>::isEmpty() const {
    return m_lowPriorityDeque.empty() && m_normalPriorityDeque.empty() && m_highPriorityDeque.empty();
}

#endif //LAB3_QUEUEWITHPRIORITY_H
