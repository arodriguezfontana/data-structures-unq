struct MascotaSt {
    string nombre;
    string especie;
};
typedef MascotaSt* Mascota;


Mascota consMascota(string nombre, string especie) {
    MascotaSt* m = new MascotaSt;
    m->nombre = nombre;
    m->especie = especie;
    return m;
}

string nombreMascota(Mascota m) {
    return m->nombre;
}

string especieMascota(Mascota m) {
    return m->especie;
}

void cambiarNombreMascota(Mascota m, string nombre) {
    m->nombre = nombre;
}
void liberarMascota(Mascota m) {
    delete m;
}