struct NodeT;
typedef NodeT* Tree;

Tree emptyT();
Tree nodeT(int elem, Tree left, Tree right);
bool isEmptyT(Tree t);
int rootT(Tree t);
Tree left(Tree t);
Tree right(Tree t);
int getAt(int i);
int sizeT(Tree t);
void addT(Tree t);
void liberarT(Tree t);