//
// Created by Mikhail Andreev on 13.04.2020.
//

#ifndef LAB4_CONTAINER_H
#define LAB4_CONTAINER_H

#include <iostream>
#include <memory>
#include "customiterator.h"

class Container {

public:
    Container();

    typedef CustomIterator<int> iterator;
    typedef CustomIterator<const int> const_iterator;

    iterator begin()
    {
        return m_items.get();
    }

    iterator end()
    {
        return m_items.get() + m_size - 1;
    }

    const_iterator begin() const
    {
        return m_items.get();
    }

    const_iterator end() const
    {
        return m_items.get() + m_size;
    }

private:
    void fillItems();
    int factorial(int n);

private:
    const size_t m_size = 10;
    std::unique_ptr<int[]> m_items;
};

#endif //LAB4_CONTAINER_H
