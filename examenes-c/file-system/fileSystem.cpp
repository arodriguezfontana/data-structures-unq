struct FSNode
{
    string nombre;
    bool isFile;
    FSNode **children;
    int childCount;
};

struct FSStr
{
    FSNode *current;
    Stack path;
    int capacidad;
};

/* INV REP:
FSStr:
- MiniFS no es NULL
- current no es NULL, es un puntero a un nodo valido
- capacidad tiene que ser mayor que 0
- si path tiene elementos:
    - es un stack en el cual cada nodo es hijo del que esta debajo
    - el nodo actual es hijo del tope del stack
    - la base del stack es el nodo raiz
- si path esta vacio, current es la raiz del filesystem

FSNode:
- si isFile es true:
    - children es NULL
    - childCount es 0
- si isFile es false:
    - children es un array de tamaño capacidad
    - childCount es menor a capacidad
- dos nodos no pueden ser acnestros entre si (no puede haber ciclos)
*/

FSNode* createNode(string name, bool isFile, int capacidad) {
    FSNode *node = new FSNode;
    node -> nombre = name;
    node -> isFile = isFile;
    node -> childCount = 0;
    if (!isFile && capacidad > 0) {
        node -> children = new FSNode *[capacidad];
    } else {
        node -> children = NULL;
    }
    return node;
}

bool hayNombre(FSNode *node, string name) { // Precondición: el nodo tiene children.
    int i = 0
    while (i < node->childCount && node->children[i]->nombre != name)
    {
        i++;
    }
    return i <= node->childCount;
}

// Agregar un nuevo directorio como hijo al nodo actual si hay espacio y no hay otro archivo o directorio con el nombre.
// Costo lineal sobre el array de los children.
// Stack: constante.
void mkdir(string name, MiniFS fs) {
    if(! fs->current->isFile && 
        fs->current->childCount < fs->capacidad && !hayNombre(fs->current, name)) {
            fs->current->children[fs->current->childCount++] = createNode(name,false, fs->capacidad);
    }
}

// Agregar un nuevo archivo como hijo al nodo actual si hay espacio y no hay otro archivo o directorio con el nombre.
// Costo lineal sobre el array de los children.
// Stack: constante.
void touch(string name, MiniFS fs){
    if(! fs->current->isFile && 
        fs->current->childCount < fs->capacidad && !hayNombre(fs->current, name)) {
            fs->current->children[fs->current->childCount++] = createNode(name,true, fs->capacidad);
    }
}

// Se mueve al hijo index del directorio actual.
// Costo constante, memoria constante.
void cd(int index, MiniFs fs) {
    if(! fs->current->isFile && index >= 0 && index < fs->current->childCount) {
        push(fs->current, index, fs->path);
        fs->current = fs->current->children[index]; 
    } 
}

// Sube un nivel en el sistema, si esta en la raiz no hace nada.
// Costo constante, memoria constante.
void cdUp(MiniFs fs) {
    if (!isEmpty(fs->path)) {
        fs->current = (FSNode *) topNode(fs->path);
        pop(fs->path);
    }
}

// Costo constante, memoria constante.
void print(string content) {
    cout << content << endl;
}

// Imprime los nombres de los hijos del nodo actual si el nodo actual es un directorio.
// Costo lineal sobre los hijos, memoria constante.
void ls(MiniFs fs) {
    for (int i = 0; i < fs->current->childCount; i++) // Por invariante no hace falta chequear que sea directorio.
    {
        print(fs->current->children[i]);
    }
}

// Elimina el archivo en la posicion dada si existe en el directorio actual, y mueve los demas hijos.
// Costo lineal sobre los hijos, memoria constante.
void rm(int index, MiniFs fs) {
    if(index >= 0 && index < fs->current->childCount && fs->current->children[index]->isFile) {
        delete fs->current->children[index];
        for (int i = index; i < fs->current->childCount; i++)
        {
            fs->current->children[i] = fs->current->children[i++];
        }
        fs->current->childCount--;
    }
}

// Devuelve la cantidad de archivos y directorios del nodo actual.
int childCount(MiniFs fs) {
    return fs->current->childCount;
}

// Devuelve el nombre del archivo/directorio actual.
string currentName(MiniFs fs) {
    return fs->current->nombre;
}

// Indica si el nodo actual es un archivo.
bool isFile(MiniFs fs) {
    return fs->current->isFile;
}

// Recursivo.
// Costo lineal sobre los hijos de fs.
// Costo en memoria lineal por la recursion sobre la profundidad de fs de entrada.
int countFiles(MiniFs fs) {
    int count = 0;
    if(isFile(fs)) {
        count ++;
    } else {
        for (int i = 0; i < childCount(fs); i++)
        {
            cd(i, fs);
            count += countFiles(fs);
            cdUp(fs);
        }  
    }
    return count;
}

// Precondición: la ruta es valida a partir de la posicion actual por lo tanto, se que la posicion actual es un directorio.
void cdPath(MiniFs fs, string* path, int len) {
    for (int i = 0; i < len; i++)
    {
        string next = path[i];
        int j = 0;
        cd(j, fs);
        while(currentName(fs) != next) {
            cdUp(fs);
            cd(++j,fs);
        }
    }
}