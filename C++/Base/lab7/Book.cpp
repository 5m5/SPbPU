//
// Created by Михаил Андреев on 27.10.2019.
//

#include "Book.h"

Book::Book(std::string author, std::string name, int releaseYear, int pageCount, BookTheme theme)
: m_author(author)
, m_name(name)
, m_releaseYear(releaseYear)
, m_pageCount(pageCount)
, m_theme(theme)
{
}

Book::Book(const Book &book) {
    setAuthor(book.author());
    setName(book.name());
    setReleaseYear(book.releaseYear());
    setPageCount(book.pageCount());
    setTheme(book.theme());
};

void Book::setAuthor(const std::string &author) {
    m_author = author;
}

void Book::setName(const std::string &name) {
    m_name = name;
}

void Book::setReleaseYear(int releaseYear) {
    m_releaseYear = releaseYear;
}

void Book::setPageCount(int pageCount) {
    m_pageCount = pageCount;
}

void Book::setTheme(BookTheme theme) {
    m_theme = theme;
}

std::string Book::author() const {
    return m_author;
}

std::string Book::name() const {
    return m_name;
}

int Book::releaseYear() const {
    return m_releaseYear;
}

int Book::pageCount() const {
    return m_pageCount;
}

BookTheme Book::theme() const {
    return m_theme;
}

std::ostream &operator<<(std::ostream &out, const Book &book) {
    out << book.author() << " - " << book.name() << ", " << book.releaseYear() << " y., "
    << book.pageCount() << " p.";
    return out;
}

Book &Book::operator=(const Book &book) {
    if(this == &book)
        return *this;

    setAuthor(book.author());
    setName(book.name());
    setReleaseYear(book.releaseYear());
    setPageCount(book.pageCount());
    setTheme(book.theme());
    return *this;
}
