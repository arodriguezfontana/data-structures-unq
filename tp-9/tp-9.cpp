#include <iostream>
#include <string>
using namespace std;

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