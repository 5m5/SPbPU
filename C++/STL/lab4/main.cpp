#include <iostream>
#include "container.h"

using namespace std;

int main() {
    auto factorials = Container();
    auto iter = factorials.begin();

    ++iter; // 2! = 2
    cout << *iter << endl;

    ++iter; // 3! = 6
    ++iter; // 4! = 24
    ++iter; // 5! = 120
    cout << *iter << endl;

    auto iterEnd = factorials.end(); // 10! = 3_628_800
    cout << *iterEnd << endl;

    --iterEnd; // 9! = 362_880
    --iterEnd; // 8! = 40_320
    cout << *iterEnd << endl;

    return 0;
}
