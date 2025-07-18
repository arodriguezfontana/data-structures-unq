#include <iostream>
#include "Arbol.h"
using namespace std;

int sumarT(Tree t) 
{
    if (isEmptyT(t))
    {
        return 0;
    }
    else
    {
        return root(t) + sumarT(left(t)) + sumarT(right(t));
    }
}

int rlSumarT(Tree t)
{
    int res = 0;
    TreeLinkedList porProcesar = emptyL();
    cons(t, porProcesar);

    if (!isEmptyT(t))
    {
        cons(t, faltanProcesar);
    }

    while (!isEmptyL(porProcesar))
    {
        Tree actual = head(porProcesar);
        res = res + root(actual);

        tail(porProcesar);

        if (!isEmptyT(right(actual)))
        {
            cons(right(actual), porProcesar);
        }
        if (!isEmptyT(left(actual))) 
        {
            cons(left(actual), porProcesar);
        }
    }
    liberarL(porProcesar)
    return res;
}

int rlNodosT(Tree t)
{
    int res = 0;
    TreeLinkedList porProcesar = emptyL();
    cons(t, porProcesar);

    if (!isEmptyT(t))
    {
        cons(t, faltanProcesar);
    }

    while (!isEmptyL(porProcesar))
    {
        Tree actual = head(porProcesar);
        res = 1 + root(actual);

        tail(porProcesar);

        if (!isEmptyT(right(actual)))
        {
            cons(right(actual), porProcesar); 
        }
        if (!isEmptyT(left(actual))) 
        {
            cons(left(actual), porProcesar);
        }
    }
    liberarL(porProcesar)
    return res;
}

void succesoresT(Tree t)
{
    if (!isEmptyT(t))
    {
        t->value++;
        succesoresT(t->left);
        succesoresT(t->right);
    }
}

int sizeT(Tree t)
{
    if (isEmptyT(t))
    {
        return 0;
    }
    else
    {
        return 1 + sizeT(t->left) || sizeT(t->right));
    }
}

bool perteneceT(int e, Tree t)
{
    if (isEmptyT(t))
    {
        return false;
    }
    else
    {
        return e == t->value || perteneceT(t->left) || perteneceT(t->right));
    }
}

int aparicionesT(int e, Tree t)
{
    if (isEmptyT(t))
    {
        return 0;
    }
    else
    {
        return unoSi(e == t->value) + aparicionesT(t->left) + aparicionesT(t->right));
    }
}

int unoSi(bool b) {
    if (b)
    {
        return 1;
    } else {
        return 0;
    }
}

int heightT(Tree t)
{
    if (isEmptyT(t))
    {
        return 0;
    }
    else
    {
        return 1 + max(heightT(t->left), heightT(t->right));
    }
}

ArrayList toList(Tree t)
{
}

ArrayList leaves(Tree t)
{
}

ArrayList levelN(int n, Tree t)
{
}

List preorderT(Tree t) {}
    if (isEmptyT(t))
    {
        return emptyList()
    } else {
        List xs = preorderT(t->left);
        List ys = preorderT(t->right);
        append(xs, ys);
        cons(t->value, x);
        return xs;
    }
}

void agregarPreorderEn(Tree t, List xs) { 
    if (!isEmptyT(t)) {
        agregarPreorderEn(t->right, xs); 
        agregarPreorderEn(t->left, xs);
        cons(t->value, xs)
    }
}

List preorderTMejorado(Tree t) {
    List vistosHastaAhora = emptyList();
    agregarPreorderEn(y, vistosHastaAhora); 
    return vistosHastaAhora;
}

void LiberarTree(Tree t)
{ 
    if (!isEmptyT(t))
    {
        LiberarTree(t->left);
        LiberarTree(t->right);
        delete t;
    }
}