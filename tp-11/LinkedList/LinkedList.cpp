#include <iostream>
#include "LinkedList.h"
using namespace std;

struct NodeList
// IE: las punteros internos NO se comparten (liberar, puede borrar toda la memoria sin problema porque no se esta compartiendo la memoria con nadie).
{
    int value;
    NodeList *next;
};

struct LinkedListSt
// IR: * first == NULL si y solo si last == NULL.
//     * size es la cantidad de nodos a recorrer hasta llegar a un NULL desde first.
//     * si last != NULL, last->next == NULL.
{
    int size;
    NodeList *first;
    NodeList *last;
};
typedef LinkedListSt *LinkedList;
// IR: * el puntero no es NULL.

struct IteratorSt
{
    NodeList *current;
};
typedef IteratorSt *ListIterator;
// IR: * el puntero no es NULL.

LinkedList nil()
{
    LinkedListSt *xs = new LinkedListSt;
    l->size = 0;
    l->first = NULL;
    l->last = NULL;
    return xs;
}

bool isEmpty(LinkedList xs)
{
    return xs->size == 0;
}

int head(LinkedList xs)
// Precondición: La lista no esta vacia.
{
    if (xs->size == 0)
    {
        throw invalid_argument("Error using head: Is empty LinkedList");
    }
    return l->first->value;
}

void Tail(LinkedList xs) 
// Precondición: La lista no esta vacia.
{
    NodeList *temp = xs->first; 
    xs->first = xs->first->next;
    if (xs->first == NULL) 
    {
        xs->last == NULL
    }
    xs->size--;
    delete temp; 
}

int length(LinkedList xs)
{
    return xs->size;
}

void Cons(int x, LinkedList xs)
{
    NodeList *n = new NodeList; 
    n->value = x;
    n->next = xs->first;

    xs->first = n;

    if (xs->last == NULL) 
    {
        xs->last = n
    }
    xs->size++;
}

void Snoc(int x, LinkedList xs)
{
    NodeList *n = new NodeList;
    n->value = x;
    n->next = NULL;

    if (xs->last == NULL) 
    {
        xs->first = n;
    }
    else
    {
        xs->last->next = n;
    }
    xs->last = n;
    xs->size++;
}

int liberar(LinkedList xs)
{
    NodeList *temp = xs->first;
    while (xs->first != NULL)
    {
        xs->first = xs->first->next;
        delete temp;
        temp = xs->first;
    }
    delete xs;
}

int last(LinkedList xs)
{ 
    if (xs->first == NULL)
    {
        throw invalid_argument("Error using last: There is no last");
    }
    return xs->last->;
}

int append(LinkedList xs, LinkedList ys)
{ 
    xs->ultimo->next = ys->primero;
    delete ys;
}

void DestroyL(LinkedList xs)
{
    NodeList *temp = xs->first;
    while (xs->first != NULL)
    {
        xs->first = xs->first->next;
        delete temp;
        temp = xs->first;
    }
    delete xs;
}

ListIterator getIterator(LinkedList xs)
{
    IteratorSt *it = new IteratorSt;
    it->current = xs->first;
    return it;
}

int current(ListIterator it)
{
    return it->current->value;
}

void SetCurrent(int x, ListIterator it)
// Precondición: no esta al final del recorrido.
{
    it->current->value = x;
}

void Next(ListIterator it)
// Precondición: no esta al final del recorrido.
{
    it->current = it->current->next;
}

bool atEnd(ListIterator it)
{
    return it->current == NULL;
}

void DisposeIterator(ListIterator it)
{
    delete it;
}