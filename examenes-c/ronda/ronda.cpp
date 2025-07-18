// En una ronda:
// * Next y prev no pueden ser null.
// * Ronda no puede ser NULL.
// * Para cualqueir nodo existe una secuencia de nodos que permite alcanzar el nodo incial a travez de los atributos "next".
// * Para cualqueir nodo existe una secuencia de nodos que permite alcanzar el nodo incial a travez de los atributos "prev".
// * Todo nodo es previo a su siguiente.
// * Todo nodo es siguiente de su previo.

void insert( int value, Ronda ronda) { // O(1) en eficiencia del codigo y memoria.
    RondaNode node = new RondaNode;
    node->value= value;
    if(ronda->current == NULL) {
        node->next = node;
        node->prev = node;
    }
    else {
        RondaNode* current = ronda->current;
        RondaNode* next = current->next;
        node->next = next;
        node->prve = current;
        current->next = node;
        next->prev = node;
    }
    ronda->current = node;
}

void remove(Ronda ronda) { // O(1) en eficiencia del codigo y memoria.
    RondaNode* current = ronda->current;
    if(current != NULL) {
      if(ronda->next == current) {
        ronda->current = NULL;
      }
      else {
        current->next->prev = current->prev;
        current->prev->next = current->next;
        ronda->current = current->prev; 
      }
    delete current;
    }
}

int length(Ronda ronda) { // O(N) en eficiencia del codigo y O(1) en memoria.
    int l = 0;
    RondaNode* current = ronda->current;
    if(current != NULL) {
        l++;
        current = current->next;
        while(current != ronda->current) {
            l++;
            current = current->next;
        }
    }
    return l;
}

int* toArray(Ronda ronda) { // O(N) en eficiencia del codigo y O(1) en memoria.
    int l = length(ronda);
    int* r = NULL;
    int* r = new int[l];
    if( l> 0 ) {
        for(int i = 0; i < l; i++) {
            r[i] = current(ronda);
            move(1,ronda);
        }
    }
    return r;
}

Ronda fromArray(int len , int* arr) { // O(N) en eficiencia del codigo y O(1) en memoria.
    Ronda r = mkRonda();
    for(int i = 0; i < len;i++) {
        insert(arr[i],r);
    }
    move(1,r);
    return r;
}