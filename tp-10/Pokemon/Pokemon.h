#include <iostream>
using namespace std;

typedef string TipoDePokemon;
struct PokemonSt;
typedef PokemonSt* Pokemon;

Pokemon consPokemon(TipoDePokemon tipo);
TipoDePokemon tipoDePokemon(Pokemon p);
int energia(Pokemon p);
void perderEnergia(int energia, Pokemon p);
bool superaA(Pokemon p1, Pokemon p2);
void ShowPokemon(Pokemon p);