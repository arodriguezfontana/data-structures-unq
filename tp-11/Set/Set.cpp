#include <iostream>
#include "Set.h"
using namespace std;

struct NodoSet
{
    int value;
    NodoS *next;
};

struct SetSt
{
    int size;
    NodoS *first;
};
typedef SetSt *Set;

Set emptyS()
{
    SetSt *s = new SetSt;
    s->cantidad = 0;
    s->primero = NULL;
    return s;
}

bool isEmptyS(Set s)
{
    return s->size == 0;
}

bool belongsS(int x, Set s)
{
    NodeS *temp = s->first;
    while (temp != NULL && temp->value != x)
    {
        temp = temp->next;
    }
    return temp != NULL && temp->value == x;
}

void AddS(int x, Set s)
{
    if (!belongsS(x, s))
    {
        NodeS *newN = new NodeS;
        newN->value = x;
        newN->next = s->first;

        s->first = newN;
        s->size++;
    }
}

void RemoveS(int x, Set s)
{
    NodeS *temp = s->fst;
    if (temp != NULL && temp->value != x)
    {
        NodeS *next = temp->next;
        while (next != NULL && next->value != x)
        {
            temp = temp->next;
            next = next->next;
        }
        if (next != NULL)
        {
            temp->next = next->next;
            delete next;
            s->size--;
        }
    }
    else if (temp->value == x)
    {
        s->fst = temp->next;
        delete temp;
        s->size--;
    }
}

int sizeS(Set s)
{
    return s->size;
}

LinkedList setToList(Set s)
{
    LinkedList xs = nil();
    NodeS *temp = s->first;
    while (temp != NULL)
    {
        Cons(temp->value, xs);
        temp = temp->next;
    }
    return xs;
}

void DestroyS(Set s)
{
    NodeSet *temp = s->first;
    while (s->first != NULL)
    {
        s->first = temp->next;
        delete temp;
        temp = s->first;
    }
    delete s;
}