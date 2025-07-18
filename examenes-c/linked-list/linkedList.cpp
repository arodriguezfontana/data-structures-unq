struct NodeStr
{
    int value;
    NodeStr* next;
};

struct LinkedlistStr
{
    int size;
    NodeStr* first;
    NodeStr* last;
};

int* ascendente(int values[], int size){
    if(size == 0) return new int[0];
    LinkedList xs = emptyLL(); 
    int maxVisto = values[0]; 
    Snoc(xs, values[0]); 
    for(int i = 1; i < size; i++){
        if(values[i] > maxVisto){
            Snoc(xs, values[i]);
            maxVisto = values[i];
        }
    }
    int* res = toArray(xs);
    DestroyLL(xs);
    return res; 
}

int[] toArray(LinkedList xs){
    int* res = new int[xs->size]; 
    NodeStr* n = xs->first; 
    for (int i = 0; i < xs->size; i++){
        res[i] = n->value; 
        n = n->next; 
    }
    return res; 
}


void reverse(LinkedList xs) {
    if (xs->size > 1) {
        NodeStr* current = xs->first; 
        NodeStr* next = current->next; 
        xs->last = current; 
        current->next = NULL;
        while (next != NULL) {
            NodeStr* temp = next->next; 
            next->next = current; 
            current = next; 
            next = temp; 
        }
        xs->first = current;
    }
}

LinkedList from(int* values, int size){
    LinkedListStr* res = new LinkedListStr; 
    res->size = size; 
    if (size > 0) {
        NodeStr* p = new NodeStr; 
        res->first = p; 
        p->value = values[0];
        for(int i = 1; i < size; i++){
            NodeStr* n = new NodeStr; 
            n->value = values[i];
            p->next = n; 
            p = n; 
        }
        res->last = p; 
        p->next = NULL; 
    } else {
        res->first = res->last = NULL;
    }
    return res; 
}