#include <iostream>
#include "Pokemon.h"
using namespace std;

struct EntrenadorSt;
typdef EntrenadorSt* Entrenador;

Entrenador consEntrenador(string nombre, int cantidad, Pokemon *pokemon);
string nombreDeEntrenador(Entrenador e);
int cantidadDePokemon(Entrenador e);
int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e);
Pokemon pokemonNro(int i, Entrenador e);
bool leGanaATodos(Entrenador e1, Entrenador e2);
void ShowEntrenador(Entrenador e);