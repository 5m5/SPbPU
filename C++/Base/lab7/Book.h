//
// Created by Михаил Андреев on 27.10.2019.
//

#ifndef LAB7_BOOK_H
#define LAB7_BOOK_H

#include "iostream"

enum BookTheme {
    Detective,
    Novel,
    Fantasy,
    Thriller,
    Historic
};

class Book {
public:
    Book(std::string author, std::string name, int releaseYear, int pageCount, BookTheme theme);
    Book(const Book &book);

    void setAuthor(const std::string &author);
    void setName(const std::string &name);
    void setReleaseYear(int releaseYear);
    void setPageCount(int pageCount);
    void setTheme(BookTheme theme);

    std::string author() const;
    std::string name() const;
    int releaseYear() const;
    int pageCount() const;
    BookTheme theme() const;

    friend std::ostream& operator<<(std::ostream &out, const Book &book);
    Book &operator=(const Book &book);

private:
    std::string m_author;
    std::string m_name;
    int m_releaseYear;
    int m_pageCount;
    BookTheme m_theme;
};

#endif //LAB7_BOOK_H
