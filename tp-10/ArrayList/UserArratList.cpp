#include <iostream>
#include "ArrayList.h"
using namespace std;

int sumatoria(ArrayList xs)
{
    int c = 0;
    for (int i = 0; i < lengthAl(xs); i++)
    {
        c += get(i, xs);
    }
    return c;
}

void sucesores(ArrayList xs)
{
    for (int i = 0; i < lengthAl(xs); i++)
    {
        set(i, get(i, xs) + 1, xs)
    }
}

bool pertenece(int x, ArrayList xs)
{
    int i = 0;
    while (i < lengthAL(xs) && get(i, xs) != x)
    {
        i++;
    }
    return i != lengthAL(xs) && get(i, xs) == x;
}

int apariciones(int x, ArrayList xs)
{
    int a = 0;
    for (int i = 0; i < lengthAL(xs); i++)
    {
        if (x == get(i, xs))
        {
            a++
        }
    }
    return a;
}

ArrayList append(ArrayList xs, ArrayList ys)
{
    ArrayList res = newArrayListWith(lengthAL(xs) + lengthAL(ys));
    for (int i = 0; i < lengthAL(xs); i++)
    {
        add(get(i, xs), res);
    }
    for (int i = 0; i < lengthAL(ys); i++)
    {
        add(get(i, ys), res);
    }
    return res;
}

int minimo(ArrayList xs)
{
    int min = get(0, xs);
    for (int i = 1; i < lengthAl(xs); i++)
    {
        min = minimoEntre(min, get(1, xs))
    }
    return min;
}

int minimoEntre(int n, int m)
{
    if (n <= m)
    {
        return n;
    }
    else
    {
        return m;
    }
}
