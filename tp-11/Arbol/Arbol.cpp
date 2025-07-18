#include <iostream>
#include "Arbol.h"
using namespace std;

struct NodeT
{
    int value;
    NodeT* left;
    NodeT* right;
};
typedef NodeT* Tree;

Tree emptyT() {
    return NULL;
}

Tree nodeT(int value, Tree left, Tree right) {
    NodeT* t = new NodeT;
    t->value = value;
    t->left = left;
    t->right = right;
    return t;
}

bool isEmptyT(Tree t) {
    return t == NULL;
}

int rootT(Tree t) {
    return t->value;
}

Tree left(Tree t) {
    return t->left;
}

Tree right(Tree t) {
    return t->right;
}

void liberarT(Tree t) {
    if (t == NULL) return;
    liberarT(left(t))
    liberarT(right(t));
    delete t;
}