#include <iostream>
#include "Book.h"

int main() {
    std::string bookName = "Гарри Поттер и узник Азкабана";
    std::string authorName = "Дж. К. Роулинг";
    int releaseYear = 2017;
    int pagesCount = 508;

    Book *book;
    book = new Book(bookName, authorName, releaseYear, pagesCount, BookTheme::Fantasy);
    std::cout << *book << std::endl;

    return 0;
}