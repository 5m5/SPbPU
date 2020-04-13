//
// Created by Mikhail Andreev on 13.04.2020.
//

#include "container.h"

Container::Container() :
        m_items(new int[m_size])
{
    fillItems();
}

void Container::fillItems() {
    for(int i = 1; i <= m_size; ++i)
        m_items[i-1] = factorial(i);
}

int Container::factorial(int n) {
    return (n == 1) ? 1 : factorial(n - 1) * n;
}
