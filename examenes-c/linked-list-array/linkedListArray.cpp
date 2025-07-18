struct LLANode
{
    int capacidad;
    ELEM* values;
    LLANode* next;
};

struct LLAHeader
{
    int total;
    LLANode* first;
};

typedef LLAHeader* LLArray;
typedef int ELEM;


LLArray emptyLLArray()
{
}

LLArray mkLLArray()
{
    LLAHeader* newLLA = new LLAHeader;
    newLLA->total = 0;
    LLANode* n = new LLANode;
    n->capacidad = 1;
    n->values = new ELEM[n->capacidad];
    n->next = NULL;
    newLLA->first = n;

    return newLLA;
}

ELEM get(int index, LLArray lla)
{
    LLANode* current = lla->first;

    while (index >= current->capacidad)
    {
        index -= current->capacidad;
        current = current->next;
    }

    return current->values[index];
}

void add(ELEM value, LLArray lla)
{
    int cantidadElementosPorVer = lla->total;
    LLANode* current = lla->first;

    while (cantidadElementosPorVer > current->capacidad)
    {
        cantidadElementosPorVer -= current->capacidad;
        current = current->next;
    }

    if (cantidadElementosPorVer == current->capacidad)
    {
        LLANode* nuevoLLA = new LLANode;
        nuevoLLA->capacidad = current->capacidad*  2;
        nuevoLLA->values = new ELEM[nuevoLLA->capacidad];
        nuevoLLA->next = NULL;
        nuevoLLA->values[0] = value;
        current->next = nuevoLLA;
    }
    else
    {
        current->values[cantidadElementosPorVer] = value;
    }

    lla->total++;
}

int totalLLA(LLArray lla)
{
    return lla->total;
}