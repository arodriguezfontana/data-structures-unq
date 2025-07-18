#include <iostream>
#include "ArrayList.h"
using namespace std;

struct ArrayListSt
{
    int cantidad;
    int *elementos;
    int capacidad;
};

typedef ArrayListSt *ArrayList;

ArrayList newArrayList()
{
    ArrayListSt *xs = new ArrayListSt;
    int *elementos = new int[16];
    xs->cantidad = 0;
    xs->elementos = elementos;
    xs->capacidad = 16;
    return xs;
}

ArrayList newArrayListWith(int capacidad)
{
    ArrayListSt *xs = new ArrayListSt;
    int *elementos = new int[capacidad];
    xs->cantidad = 0;
    xs->elementos = elementos;
    xs->capacidad = capacidad;
    return xs;
}

int lengthAL(ArrayList xs)
{
    return xs->cantidad;
}

int get(int i, ArrayList xs)
{
    return xs->elementos[i];
}

void set(int i, int x, ArrayList xs)
{
    xs->elementos[i] = x;
}

void resize(int capacidad, ArrayList xs)
{
    if (capacidad >= xs->cantidad)
    {
        xs->capacidad = capacidad;
    }
    cambiarTamanio(capacidad, xs, xs->cantidad);
}

void cambiarTamanio(int menorCapacidad, ArrayList xs, int cantidad)
{
    int *temp = new int[menorCapacidad];
    for (int i = 0; i < menorCapacidad; i++)
    {
        temp[i] = xs->elementos[i]
    }
    delete xs->elementos; 
    xs->cantidad = menorCapacidad;
    xs->capacidad = menorCapacidad;
    xs->elementos = temp;
}

void add(int x, ArrayList xs)
{
    if (xs->cantidad == xs->capacidad)
    {
        resize(xs->capacidad * 2, xs)
    }
    xs->elementos[xs->cantidad] = x;
    xs->cantidad++;
}

void remove(ArrayList xs)
{
    if (xs->cantidad > 0)
    {
        xs->cantidad--;
    }
}