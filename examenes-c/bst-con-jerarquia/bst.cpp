struct JBSTNodeStr
{
    string name;
    JBSTNodeStr* left;
    JBSTNodeStr* right;
    JBSTNodeStr* parent
};

struct JBSTStr
{
    JBSTNodeStr* root;
};

typedef JBSTStr* JerarquiaBST;

// Inv. Rep.:
// * No existen dos nodos con el mismo name.
// * Un arbol vacio es un JBSTStr con root == NULL.
// * Todos los nodos menos el root tienen un parent != NULL.
// * El nodo root tiene un parent == NULL.
// * Para todos los nodos se cumple que: left->name < name y right->name > name (en el caso que tengan nodo left y right).
// * Si un nodo A tiene como parent a un nodo B, entonces el nodo B no puede tener como parent al nodo A.
// * Si un nodo B tiene como parent a un nodo A, entonces el nodo A no puede tener como parent al nodo B.
// * Todos los nodos del arbol, menos el root, deben tener como parent al root, directamente o indirectamente.

JerarquiaBST fundar(string p)
{
    JBSTStr* bst = new JBSTStr;
    JBSTNodeStr* n = new JBSTNodeStr;
    
    n->name = p;
    n->left = NULL;
    n->right = NULL;
    n->parent = NULL;

    bst->root = n;
    
    return bst;
}

// Precondiciones:
// * Existe una persona en la jerarquia con el nombre del superior dado.
// * No existe una persona en la jerarquia con el nombre nuevo.
void Insertar(string nuevo, string superior, JerarquiaBST t)
{
    JBSTNodeStr* node = new JBSTNodeStr;
    node->name = nuevo;
    node->left = NULL;
    node->right = NULL;
    node->parent = find(superior, t->root);
    JBSTNodeStr* actual = t->root;
    JBSTNodeStr* padre = NULL;

    while(actual != NULL) {
        padre = actual;
        if (actual->name > nuevo)
        {
            actual = actual->left;
        } else {
            actual = actual->right;
        }
    }

    if (padre->name > nuevo)
    {
        padre->left = node;
    } else {
        padre->right = node;
    }
}

// Precondiciones:
// * Existe una persona en la jerarquia con el nombre del empleado dado.
// * Existe una persona en la jerarquia con el nombre del superior dado.
bool esSubordinadoDe(string empleado, string superior, JerarquiaBST bst)
{
    JBSTNodeStr* actual = find(empleado, bst);

    while (actual != NULL && actual->name != superior)
    {
        actual = actual->parent;
    }
    return actual != NULL;
}

JBSTNodeStr* find(string nombre, JBSTNodeStr* t) {
    JBSTNodeStr* actual = t;

    while (actual != NULL && actual->name != nombre)
    {
        if (actual->name > nombre)
        {
            actual = actual->left;
        } else {
            actual = actual->right;
        }
    }
    return actual;
}