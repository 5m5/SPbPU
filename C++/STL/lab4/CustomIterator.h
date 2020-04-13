//
// Created by Mikhail Andreev on 13.04.2020.
//

#ifndef LAB4_CUSTOMITERATOR_H
#define LAB4_CUSTOMITERATOR_H

#include <iostream>

class Container;

template<typename T>
class CustomIterator : public std::iterator<std::input_iterator_tag, T> {

    friend class Container;

public:
    CustomIterator(const CustomIterator &it);

    bool operator!=(CustomIterator const &other) const;
    bool operator==(CustomIterator const &other) const;
    typename CustomIterator::reference operator*() const;
    CustomIterator &operator++();
    CustomIterator &operator--();

private:
    CustomIterator(T *p);
    T *p;
};

template<typename T>
CustomIterator<T>::CustomIterator(T *p) : p(p)
{

}

template<typename T>
CustomIterator<T>::CustomIterator(const CustomIterator &it) : p(it.p)
{

}

template<typename T>
bool CustomIterator<T>::operator!=(CustomIterator const &other) const
{
    return p != other.p;
}

template<typename T>
bool CustomIterator<T>::operator==(CustomIterator const &other) const
{
    return p == other.p;
}

template<typename T>
typename CustomIterator<T>::reference CustomIterator<T>::operator*() const
{
    return *p;
}

template<typename T>
CustomIterator<T> &CustomIterator<T>::operator++()
{
    ++p;
    return *this;
}

template<typename T>
CustomIterator<T> &CustomIterator<T>::operator--()
{
    --p;
    return *this;
}

#endif //LAB4_CUSTOMITERATOR_H
