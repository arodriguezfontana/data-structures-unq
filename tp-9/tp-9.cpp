#include <iostream>
#include <string>
#include "Par.h"
#include "Fraccion.h"

using namespace std;

    // Ejercicios 1 y 2 en papel.

    // Ejercicio 3.

    Par consPar(int x, int y)
    {
        Par p;
        p.x = x;
        p.y = y;
        return p;
    }

    int fst(Par p)
    {
        return p.x;
    }

    int snd(Par p)
    {
        return p.y;
    }

    int maxDelPar(Par p)
    {
        if (p.x > p.y)
        {
            return p.x;
        }
        else
        {
            return p.y;
        }
    }

    Par swap(Par p)
    {
        int fst = p.x;
        p.x = p.y;
        p.y = fst;
        return p;
    }

    Par divisionYResto(int n, int m)
    {
        Par p;
        p.x = n / m;
        p.y = n % m;
        return p;
    }

    // Ejercicio 4.

    void iPrintN(int n, string s)
    {
        while (n > 0)
        {
            cout << s << " ";
            n--;
        }
    };

    void rPrintN(int n, string s)
    {
        if (n > 0)
        {
            cout << s << " ";
            rPrintN((n - 1), s);
        };
    };

    void iCuentaRegresiva(int n)
    {
        while (n >= 0)
        {
            cout << n << endl;
            n--;
        }
    }

    void rCuentaRegresiva(int n)
    {
        if (n >= 0)
        {
            cout << n << endl;
            rCuentaRegresiva(n - 1);
        }
    }

    void iDesdeCeroHastaN(int n)
    {
        for (int i = 0; i <= n; i++)
        {
            cout << i << endl;
        }
    }

    void rDesdeCeroHastaN(int n)
    {
        if (n >= 0)
        {
            rDesdeCeroHastaN(n - 1);
            cout << n << endl;
        }
    }

    int iMult(int n, int m)
    {
        int i = 0;
        while (m > 0)
        {
            i += n;
            m--;
        }
        return i;
    }

    int rMult(int n, int m)
    {
        int i = 0;
        if (m > 0)
        {
            i = n + rMult(n, m - 1);
        }
        return i;
    }

    void iPrimerosN(int n, string s)
    {
        for (int i = 0; i < n; i++)
        {
            cout << s[i] << endl;
        }
    }

    void rPrimerosN(int n, string s)
    {
        if (n > 0)
        {
            rPrimerosN(n - 1, s)
                    cout
                << s[n - 1] << endl;
        }
    }

    bool iPertenece(char c, string s)
    {
        int i = 0;
        while (s[i] != c && s[i] != '\0')
        {
            i++;
        }
        return s[i] == c;
    }

    bool rPertenece(char c, string s)
    {
        if (s[0] == '\0')
        {
            return (s[0] == c);
        }
        else
        {
            return (s[0] == c || rPertenece(c, s.substr(1)));
        }
    }

    int iApariciones(char c, string s)
    {
        int i = 0;
        for (int i = 0; s[i] != '\0'; i++)
        {
            if (s[i] == c)
            {
                i++;
            }
        }
        return i;
    }

    int rApariciones(char c, string s)
    {
        if (s[0] == '\0')
        {
            return (0);
        }
        else
        {
            if (s[0] == c)
            {
                return (1 + rApariciones(c, s.substr(1)));
            }
            else
            {
                return (rApariciones(c, s.substr(1)));
            }
        }
    }

    // Ejercicio 5.

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