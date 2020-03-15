#include <iostream>
#include <algorithm>

using namespace std;

template <class T> class Link;
template <class T> class List_iterator;

template <class T>
class List
{
public:
   typedef List_iterator<T> iterator;

   List();
   List(const List<T> & l);
   ~List();

   bool empty() const;
   unsigned int size() const;
   T & back() const;
   T & front() const;
   void push_front(const T & x);
   void push_back(const T & x);
   void pop_front();
   void pop_back();
   iterator begin() const;
   iterator end() const;
   void insert(iterator pos, const T & x);
   void erase(iterator & pos);
   List<T> & operator=(const List<T> & l);

protected:
   Link<T> * first_link;
   Link<T> * last_link;
   unsigned int my_size;
};

template <class T>
List<T>::List()
{
        first_link = 0;
        last_link = 0;
        my_size = 0;
}

template <class T>
List<T>::List(const List & l)
{
        first_link = 0;
        last_link = 0;
        my_size = 0;
        for (Link<T> * current = l.first_link; current != 0; current = current -> next_link)
                push_back(current -> value);
}

template <class T>
typename List<T>::iterator List<T>::begin() const
{
        return iterator(first_link);
}

template <class T>
class Link
{
private:
   Link(const T & x): value(x), next_link(0), prev_link(0) {}//pg. 204

   T value;
   Link<T> * next_link;
   Link<T> * prev_link;

   friend class List<T>;
   friend class List_iterator<T>;
};

template <class T> class List_iterator//pg.207
{
public:
   typedef List_iterator<T> iterator;

   List_iterator(Link<T> * source_link): current_link(source_link) { }
   List_iterator(): current_link(0) { }
   List_iterator(List_iterator<T> * source_iterator): current_link(source_iterator.current_link) { }

   T & operator*();  // dereferencing operator
   iterator & operator=(const iterator & rhs);
   bool operator==(const iterator & rhs) const;
   bool operator!=(const iterator & rhs) const;
   iterator & operator++();
   iterator operator++(int);
   iterator & operator--();
   iterator operator--(int);

protected:
   Link<T> * current_link;

   friend class List<T>;
};

template <class T>
T & List_iterator<T>::operator*()
{
        return current_link -> value;
}

template <class T>
List_iterator<T> & List_iterator<T>::operator++()
{
        current_link = current_link -> next_link;
        return *this;
}

template <class T>
void List<T>::push_back(const T & x)
{
    Link<T> * new_link = new Link<T> (x);
    if (first_link == 0)
    first_link = last_link = new_link;
    else
    {
    new_link->prev_link = last_link;
        last_link->next_link = new_link;
        last_link = new_link;
    }
    my_size++;
}

template <class T>
typename List<T>::iterator List<T>::end() const
{
        return iterator(last_link);
}

template <class T>
List <T>::~List()
{
    Link <T> * first = first_link;
    while (first != 0)
    {
    Link <T> * next = first->next_link;
        delete first;
    first = next;
    }
}

template<class T>
bool List_iterator<T>::operator==(const iterator & rhs) const
{
    return ( this->current_link == rhs.current_link );
}

template <class T>
bool List_iterator<T>::operator!=(const iterator & rhs) const
{
    return !( *this == rhs );
}

int main()
{
   List<int> l;

   l.push_back(44);  // list = 44
   l.push_back(33);  // list = 44, 33
   l.push_back(11);  // list = 44, 33, 11
   l.push_back(22);  // list = 44, 33, 11, 22

   List<int> m(l);

   List<int>::iterator itr(m.begin());
   while (itr != m.end()) {
        cout << *itr << endl;
        ++itr;
   }
}
