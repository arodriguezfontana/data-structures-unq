#include <iostream>
#include "LinkedList.h"
using namespace std;

int sumatoriaSinIterador(LinkedList xs)
{
    int total = 0;
    NodeList *current = xs->first;
    while (current != NULL)
    {
        total += current->value;
        current = current->next;
    }
    return total;
}

int sumatoria(LinkedList xs) // O(n).
{
    int sumatoria = 0;
    ListIterator it = getIterator(xs);
    while (!atEnd(it))
    {
        total += current(it);
        Next(it)
    }
    DisposeIterator(it);
    return sumatoria;
}

void Sucesores(LinkedList xs) // M
{
    ListIterator it = getIterator(xs);
    while (!atEnd(it))
    {
        SetCurrent(current(it) + 1, it);
        Next(it)
    }
    DisposeIterator(it);
}

bool pertenece(int x, LinkedList xs)
{
    ListIterator it = getIterator(xs);
    while (!atEnd(it) && current(ixs) != x)
    {
        Next(it)
    }
    bool found = !atEnd(it) && current(it) == x;
    DisposeIterator(it);
    return found;
}

int apariciones(int x, LinkedList xs)
{
    int apariciones = 0;
    ListIterator it = getIterator(xs);
    while (!atEnd(it))
    {
        if (current(it) == x)
        {
            apariciones++;
        }
        Next(it)
    }
    DisposeIterator(it);
    return apariciones;
}

int minimo(LinkedList xs)
{
    ListIterator it = getIterator(xs);
    int minimo = current(it);
    while (!atEnd(it))
    {
        minimo = min(minimo, current(it));
        Next(it);
    }
    DisposeIterator(it);
    return minimo;
}

LinkedList copy(LinkedList xs)
{
    LinkedList newLL = nil();
    ListIterator it = getIterator(xs);
    while (!atEnd(it))
    {
        Snoc(current(it), newLL);
        Next(it);
    }
    DisposeIterator(it);
    return newLL;
}

void Append(LinkedList xs, LinkedList ys)
{
    while (!isEmpty(ys)) 
    {
        Snoc(head(ys), xs);
        Tail(ys);
    }
    DestroyL(ys);
}