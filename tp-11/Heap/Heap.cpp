struct NodeT {
    int* elementos; // IR.: la longitud de values[] = capacity.
    int capacidad; // IR.: capacity >= 1.
    int tamanio; // IR.: size <= capacity.
}

Tree emptyT() {
    TreeSt* t = new TreeSt;
    t->elementos = int[16];
    t->capacidad = 16;
    t->tamanio = 0;
}

void addT(int n, Tree t) {
    t->elementos[t->tamanio] = n;
    t->tamanio++;
    // Me fijo si tengo que agrandar.
}

bool isEmptyT(Tree t) {
    return t->tamanio == 0;
}

int left(int i) {
    return i * 2;
}

int right(int i) {
    return i * 2 + 1;
}

int parent(int i) {
    // Precondición: i > 1;
    return i / 2;
}

int rootT(Tree t) {
    return t->elementos[1];
}

int getAt(int i, Tree t) {
    return t->elementos[i];
}

int sizeT(Tree t) {
    return t->tamanio;
}

void liberarT(Tree t) {
    delete[] t->elementos;
    delete t;
}

void setAt(int i, int elemento, Tree t) {
    t->elementos[i] = elemento;
}

int sumT(Tree t) {
    int res = 0;
    for (int i = 0; i < sizeT(t); i++)
    {
        res = res + getAt(i, t);
    }
    return res;
}

int sumTLeft(Tree t) {
    int res = 0;
    int i = 0;
    while (i < sizeT(t))
    {
        res = res + getAt(i, t);
        i = left(i);
    }
    return res;
}