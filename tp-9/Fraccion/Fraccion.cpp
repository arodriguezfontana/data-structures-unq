#include <iostream>
#include "Fraccion.h"
using namespace std;

struct Fraccion
{
    int numerador;
    int denominador;
};

Fraccion consFraccion(int numerador, int denominador)
{
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return f;
}

int numerador(Fraccion f)
{
    return f.numerador;
}

int denominador(Fraccion f)
{
    return f.denominador;
}

float division(Fraccion f)
{
    return f.numerador / f.denominador;
}

Fraccion multF(Fraccion f1, Fraccion f2)
{
    Fraccion newF;
    newF.numerador = f1.numerador * f2.denominador;
    newF.denominador = f1.denominador * f2.numerador;
    return newF;
}

int mdc(int n, int m)
{
    int i = 2;
    while (n % i != 0 || m % i != 0)
    {
        i++;
    }
    return i;
}

Fraccion simplificada(Fraccion p)
{
    Fraccion newF;
    int numP = p.numerador;
    int denP = p.denominador;
    int mdc = mdc(numP, denP);

    while (denP != 1 && (denP % mdc == 0 && numP % mdc == 0))
    {
        numP = numP / mdc;
        denP = denP / mdc;
    }

    newF.numerador = numP;
    newF.denominador = denP;

    return newF;
}

int mcm(int n, int m)
{
    int i = 2;
    while (not((i % n == 0) && (i % m == 0)))
    {
        i++;
    }
    return i;
}

Fraccion sumF(Fraccion f1, Fraccion f2)
{
    Fraccion newF;
    if (f1.denominador == f2.denominador)
    {
        newF.numerador = f1.numerador + f2.numerador;
        newF.denominador = f1.denominador;
        return newF;
    }
    int mcm = mcm(f1.denominador, f2.denominador);
    newF.denominador = mcm;
    newF.numerador = ((mcm / f1.denominador) * f1.numerador) + ((mcm / f2.denominador) * f2.numerador);
    return newF;
}

void ShowFraccion(Fraccion f)
{
    cout << "Fraccion {" << endl;
    cout << " nominador -> " << f->nominador << endl;
    cout << " denominador -> " << p->denominador << endl;
    cout << "}" << endl;
}