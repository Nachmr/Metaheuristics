#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <stdlib.h>
#include <ctime>
#include <climits>
#include <vector>
#include <algorithm>
#include <math.h>
#include <time.h>
#include "timer.h"
#include "random_ppio.h"

using namespace std;

/*Generador de números aleatorios*/

unsigned long Seed = 0L;

#define MASK 2147483647
#define PRIME 65539
#define SCALE 0.4656612875e-9

void Set_random (unsigned long x)
/* Inicializa la semilla al valor x.
   Solo debe llamarse a esta funcion una vez en todo el programa */
{
    Seed = (unsigned long) x;
}

unsigned long Get_random (void)
/* Devuelve el valor actual de la semilla */
{
    return Seed;
}

float Rand()
/* Genera un numero aleatorio real en el intervalo [0,1[
   (incluyendo el 0 pero sin incluir el 1) */
{
    return (( Seed = ( (Seed * PRIME) & MASK) ) * SCALE );
}

int Randint(int low, int high)
/* Genera un numero aleatorio entero en {low,...,high} */
{
    return (int) (low + (high-(low)+1) * Rand());
}

/*Contador tiempo*/

clock_t start_time;

double elapsed;


void start_timers()
/*
      FUNCTION:       virtual and real time of day are computed and stored to
                      allow at later time the computation of the elapsed time
		      (virtual or real)
      INPUT:          none
      OUTPUT:         none
      (SIDE)EFFECTS:  virtual and real time are computed
*/
{
    start_time = clock();
}



double elapsed_time()
/*
      FUNCTION:       return the time used in seconds (virtual or real, depending on type)
      INPUT:          TIMER_TYPE (virtual or real time)
      OUTPUT:         seconds since last call to start_timers (virtual or real)
      (SIDE)EFFECTS:  none
*/
{
    elapsed = clock()- start_time;
    return elapsed / CLOCKS_PER_SEC;
}
//FIN CONTADOR DE TIEMPOS
////////////////////////////////////////////////////////////////////////////////////

//Parámetros propios del problema
int tam;
int **flujo;
int **distancia;

/*Lee el archivo con la instancia en el formato estandar de QAPLIB generando
 la matriz de distancia y flujo.*/
void leer(char *ent) {
    ifstream entrada;
    entrada.open(ent);
    string linea;
    entrada >> linea;
    tam = atoi(linea.c_str());
    flujo = (int**) malloc(sizeof (int*) *tam);
    distancia = (int**) malloc(sizeof (int*) *tam);
    for (int i = 0; i < tam; i++) {
        distancia[i] = new int[tam];
        for (int j = 0; j < tam; j++) {
            entrada >> linea;
            distancia[i][j] = atoi(linea.c_str());
        }
    }
    for (int i = 0; i < tam; i++) {
        flujo[i] = new int[tam];
        for (int j = 0; j < tam; j++) {
            entrada >> linea;
            flujo[i][j] = atoi(linea.c_str());
        }
    }
}

/*Representa una solucion para el problema*/
typedef struct estado {
    int *sol = new int [tam];
    long costo;
} Estado;

/*Funcion destructora de estados*/
void destructorEstado(Estado *e) {
    delete [] e->sol;
    delete e;
}

typedef struct ord {
    long valor;
    int elem;
    int elem2;
} Ord;

typedef struct movimiento {
    int i;
    int j;
    int posi;
    int posj;
} Mov;

/*Dado un estado, se le calcula el costo*/
void setCosto(Estado* e) {
    e->costo = 0;
    for (int i = 0; i < tam; i++) {
        for (int j = 0; j < tam; j++) {
            e->costo += flujo[i][j] * distancia[e->sol[i]][e->sol[j]];
        }
    }
}

/*Dado un puntero a un estado, devuelve un puntero a un estado idéntico*/
Estado* clonar(Estado*e) {
	Estado *ret = (Estado*) malloc(sizeof (Estado));
	ret->sol = (int*) malloc(sizeof (int) *tam);
	ret->costo = e->costo;
	for (int i = 0; i < tam; i++) {
		ret->sol[i] = e->sol[i];
	}
	return ret;
}

//Devuelve el movimiento hecho entre la solucion actual y su vecino
Mov movimientoHecho(Estado* actual, Estado* vecino){
    Mov mov;
    int cambiosencontrados=0;
    for(int k=0; k<tam && cambiosencontrados <2; k++){
        if(actual->sol[k] != vecino->sol[k])
            switch(cambiosencontrados){
                case 0:
                    mov.i = vecino->sol[k];
                    mov.posi = k;
                    cambiosencontrados=1;
                break;

                case 1:
                    mov.j = vecino->sol[k];
                    mov.posj= k;
                    cambiosencontrados=2;
                break;
        }
    }
    return mov;
}

// Dado un estado y las dos localizaciones a intercambiar, se calcula el costo factorizado del vecino.
int CostoFactorizado(Estado* e, int r, int s){
    int deltaC=0;

    for(int k=0; k<tam; k++){
        if(k!=r && k!= s)
            deltaC +=
               flujo[r][k] * (distancia[e->sol[s]][e->sol[k]] - distancia[e->sol[r]][e->sol[k]])
             + flujo[s][k] * (distancia[e->sol[r]][e->sol[k]] - distancia[e->sol[s]][e->sol[k]])
             + flujo[k][r] * (distancia[e->sol[k]][e->sol[s]] - distancia[e->sol[k]][e->sol[r]])
             + flujo[k][s] * (distancia[e->sol[k]][e->sol[r]] - distancia[e->sol[k]][e->sol[s]]);
    }

     return deltaC;
}

/*En el estado "e" se intercambian las localidades entre las instalaciones
  "a" y "b"*/
Estado* intercambialocalizaciones(Estado* e, int a, int b) {
    Estado* ret = new Estado;
    ret->sol = new int [tam];
    for (int i = 0; i < tam; i++) {
        ret->sol[i] = e->sol[i];
    }
    int aux = ret->sol[a];
    ret->sol[a] = ret->sol[b];
    ret->sol[b] = aux;
    ret->costo = e->costo + CostoFactorizado(e, a, b);
    return ret;
}



/*Imprime por stdout el estado dado*/
void printEstado(Estado* e) {
    cout << e->costo << endl;
}

/* Implementacion del operador de intercambio para la generacion de
   vecinos. Se generan dos posiciones aleatorias en el vector que
   representa la solucion actual y se intercambian sus valores para
   obtener un vecino aleatorio. */
Estado* Genera_Vecino (Estado* actual){
    Estado* vecino;
  int localizacion1 = Randint(0, tam-1);
  int localizacion2 = Randint(0, tam-1);

  while(localizacion1==localizacion2)
    localizacion2 = Randint(0, tam-1);

    vecino = intercambialocalizaciones(actual, localizacion1, localizacion2);
  return vecino;
 }

/*Dado un estado, se devuelve un vecindario aleatorio.*/
Estado** getVecinos(Estado* e) {
    Estado **vecindarioale = new Estado* [tam * (tam - 1) / 2];
    Estado *aux;
    int tamvecindario = (tam * (tam - 1) )/ 2;
    int cont = 0;
    for (int i = 0; i < tamvecindario; i++) {
        for (int j = i + 1; j < tamvecindario; j++) {
            destructorEstado(aux);
            aux = Genera_Vecino(e);
            vecindarioale[cont] = aux;
            cont++;
        }
    }
    destructorEstado(aux);
    return vecindarioale;
}


/*Dado un estado, devuelve el todos sus vecinos. El vecindario viene dado
 por todos los estados conseguidos al intercambiar la posicion de dos instalaciones
 cualquiera*/
Estado** getVecindario(Estado* e) {
    Estado **ret = new Estado* [tam * (tam - 1) / 2];
    Estado *aux = new Estado;
    int cont = 0;
    for (int i = 0; i < tam; i++) {
        for (int j = i + 1; j < tam; j++) {
            aux = intercambialocalizaciones(e,i,j);
            ret[cont] = aux;
            cont++;
        }
    }

    return ret;
}

//Devuelve un estado identico a que se le pasa
Estado* copiar(Estado*e) {
    Estado *copia = (Estado*) malloc(sizeof (Estado));
    copia->sol = (int*) malloc(sizeof (int) *tam);
    copia->costo = e->costo;
    for (int i = 0; i < tam; i++) {
        copia->sol[i] = e->sol[i];
    }
    return copia;
}

/*Devuelve una solucion aleatoria*/
Estado* getSolAleatoria() {
    vector <int> a;
    for (int i = 0; i < tam; i++) {
        a.push_back(i);
    }
    Estado *ret = (Estado*) malloc(sizeof (Estado));
    ret->sol = (int*) malloc(sizeof (int) *tam);
    for (int i = 0; i < tam; i++) {
        int aux = random() % (tam - i);
        ret->sol[i] = a[aux];
        a.erase(a.begin() + aux);
    }
    setCosto(ret);

    return ret;
}


/*Funcion para ordenamiento de Mayor a menor*/
bool ordenMam(Ord *a, Ord* b) {
    return (a->valor > b->valor);
}

/*Funcion para ordenamiento de menor a Mayor*/
bool ordenmaM(Ord *a, Ord* b) {
    return (a->valor < b->valor);
}

/*Devuelve un vector<Ord *> con las instalaciones ordenadas por flujo*/
vector<Ord *> ordenarXflujo() {
    vector<Ord *> orden(tam);

    for (int i = 0; i < tam; i++) {
        orden[i] = (Ord*) malloc(sizeof (Ord));
        orden[i]->elem = i;
        for (int j = 0; j < tam; j++)
            orden[i]->valor += flujo[i][j];
    }

    sort(orden.begin(), orden.end(), ordenMam);

    return orden;
}

/*Devuelve un vector<Ord *> con las instalaciones ordenadas por distancia*/
vector<Ord *> ordenarXdist() {
    vector<Ord *> orden(tam);

    for (int i = 0; i < tam; i++) {
        orden[i] = (Ord*) malloc(sizeof (Ord));
        orden[i]->elem = i;
        for (int j = 0; j < tam; j++)
            orden[i]->valor += distancia[i][j];
    }

    sort(orden.begin(), orden.end(), ordenmaM);

    return orden;
}

//Solución Greedy que asigna unidades con mayor flujo en localizaciones más centricas.
Estado *getSolGreedy() {

    Estado *greedy =new Estado;
    greedy->sol = new int[tam];

    vector<Ord*> orden_flujo = ordenarXflujo();
    vector<Ord*> orden_dist = ordenarXdist();

    for (int i = 0; i < tam; i++)
        greedy->sol[orden_flujo[i]->elem] = orden_dist[i]->elem;

    setCosto(greedy);

    return greedy;
}


//Busqueda Local con Don't look bits
Estado* busquedaLocal(Estado* actual) {

    bool improve_flag=true;
    bool dlb[tam];
	
    for(int i = 0; i < tam; i++)
       dlb[i] = 0;

    while (improve_flag){
    improve_flag = false;
        for (int i = 0; i < tam && !improve_flag; i++){
            if (dlb[i] == 0){

                improve_flag = false;
                for (int j = 0; j < tam && !improve_flag; j++){
                    if (CostoFactorizado(actual, i, j) < 0){
                        actual = intercambialocalizaciones(actual, i, j);
                        dlb[i] = 0;
                        dlb[j] = 0;
                        improve_flag = true;
                    }
                }
            }
            if (!improve_flag)
                dlb[i] = 1;
        }
    }

    return actual;
}

Estado* BMB(){
	Estado *e[25];
	//Genero las soluciones aleatorias iniciales
	for(int i = 0; i<25; ++i){
		e[i] = getSolAleatoria();
	}
	//Aplico BL a cada una
	for(int i = 0; i<25; ++i){
		e[i] = busquedaLocal(e[i]);
	}
	//Selecciono la mejor solucion encontrada
	Estado *mejorsol = e[1];
	for(int i = 0; i<25; ++i){
		if(mejorsol->costo > e[i]->costo)
			mejorsol = e[i];
	}
	
	return mejorsol;
}

//Implementación Greedy aleatorio
Estado *getGreedyAleatorio() {
	Estado *greedy = (Estado *) malloc(sizeof (Estado));
	greedy->sol = (int *) malloc(sizeof (int) *tam);
	
	int inst = rand() % tam;
	vector<Ord*> orden(tam);
	vector<Ord*> orden_dist = ordenarXdist();
	
	for (int i = 0; i < tam; i++) {
		orden[i] = (Ord*) malloc(sizeof (Ord));
		orden[i]->valor = flujo[inst][i] + flujo[i][inst];
		orden[i]->elem = i;
	}
	
	sort(orden.begin(), orden.end(), ordenMam);
	
	for (int i = 0; i < tam; i++)
		greedy->sol[orden[i]->elem] = orden_dist[i]->elem;
	
	setCosto(greedy);
	return greedy;
}

/*Implementación de la metaheurí­stica GRASP*/
Estado *GRASP() {
	Estado *best = (Estado*) malloc(sizeof (Estado));
	Estado *actual;
	int i;
	best->costo = LONG_MAX;
	
	while (i != 25) {
		actual = getGreedyAleatorio();
		actual = busquedaLocal(actual);
		if (best->costo > actual->costo) {
			best = clonar(actual);
			i = -1;
		}
		i++;
	}
	return best;
}

Estado *mutarILS(Estado *e){
	Estado* mutar = copiar(e);
	int t = tam/4;
	int pos = Randint(0, tam -1 - tam/4);
	
	for(int i=0; i<tam; i++){
		int aux, ale1 = Randint(pos, pos+t), ale2 = Randint(pos, pos+t);
		while(ale1 == ale2){
			ale1 = Randint(pos, pos+t);
		}
		
		aux = mutar->sol[ale1];
		mutar -> sol[ale1] = mutar -> sol[ale2];
		mutar -> sol[ale2] = aux;
		
	}

	setCosto(mutar);
	
	return mutar;
}

Estado * ILS() {
	Estado *actual = getSolAleatoria();
	Estado *mejor = clonar(actual);

	
	busquedaLocal(actual);

	for(int i = 0; i < tam; ++i){
		
		actual = mutarILS(actual);
		actual = busquedaLocal(actual);
		
		if(actual->costo < mejor->costo)
			mejor = clonar(actual);
	}

	return mejor;
}

int main(int argc, char **argv) {
	int Semilla = atoi(argv[3]);
    Set_random(Semilla);

    string uso = "./qap <Metaheuristica> <archivo de entrada>\n\n"
            "1) Solución Greedy\n"
			"2) Busqueda Multiarranque Basica\n"
			"3) GRASP\n"
			"4) ILS\n";

    if (argc < 3) {
        cout << uso << endl;
        return EXIT_FAILURE;
    }
    Estado* e;
    int metaheuristica = atoi(argv[1]);

    //Se lee la instancia de entrada
    leer(argv[2]);

    start_timers();


    //Ejecuto la metaheurÃ­stica dada por el identificador
    switch (metaheuristica) {
        case 1:{
            e = getSolGreedy();
            break;
		}
		case 2:{
			e = BMB();
			break;
		}
		case 3:{
			e = GRASP();
			break;
		}
		case 4:{
			e = ILS();
			break;
		}
        default:
            cout << "Identificador de metaheuristica invalido" << endl;
            return EXIT_FAILURE;
    }

    double segundos = elapsed_time();

    cout  << e->costo << endl;
	cout << segundos <<endl;

    return EXIT_SUCCESS;
}
