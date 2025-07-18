struct MascotaSt;
typedef MascotaSt* Mascota;

Mascota consMascota(string Name, string especie);
string nombreMascota(Mascota m);
string especieMascota(Mascota m);
void cambiarNombreMascota(Mascota m, string nombre);
void liberarMascota(Mascota m);