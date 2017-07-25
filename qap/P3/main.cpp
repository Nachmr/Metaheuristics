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
    cout <<"Costo: " << e->costo << endl;
    cout << "Solución:"<<endl;
    for (int i = 0; i < tam; i++) {
        cout << e->sol[i] << " ";
    }
    cout << endl;
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

//Solución Greedy. Unidades con mayor flujo en localizaciones más centricas.
Estado *getSolGreedy() {

    Estado *greedy =new Estado;// (Estado *) malloc(sizeof (Estado));
    greedy->sol = new int[tam];//(int *) malloc(sizeof (int) *tam);

    vector<Ord*> orden_flujo = ordenarXflujo();
    vector<Ord*> orden_dist = ordenarXdist();

    for (int i = 0; i < tam; i++)
        greedy->sol[orden_flujo[i]->elem] = orden_dist[i]->elem;

    setCosto(greedy);

    return greedy;
}



/*Dados los padres (padre1, padre2) se hace el cruce (aleatorio) de un solo
 * punto entre dichos dos padres. Los individuos resultantes se almacenan en el
 * arreglo de estados "hijos"*/
void cruce1(Estado **poblacion, Estado **hijos, int *padres, int i, int padre1, int padre2) {
	/* Seleccionamos un k para el crossover */
	int k = random() % tam;
	if (k == 0)
		k += tam / 2;
	else if (k == tam - 1)
		k -= tam / 2;
	/* Reservamos espacio para la solucion de los hijos */
	hijos[i] = (Estado*) malloc(sizeof (Estado));
	hijos[i + 1] = (Estado*) malloc(sizeof (Estado));
	hijos[i]->sol = (int*) malloc(sizeof (int) *tam);
	hijos[i + 1]->sol = (int*) malloc(sizeof (int) *tam);
	/* Guardamos los dos hijos del crossover */
	for (int j = 0; j < k; j++) {
		hijos[i]->sol[j] = poblacion[padre1]->sol[j];
		hijos[i + 1]->sol[j] = poblacion[padre2]->sol[j];
	}
	int asig = k, asig1 = k;
	for (int j = 0; j < tam; j++) {
		bool esta = false, esta1 = false;
		for (int j1 = 0; j1 < asig && !esta; j1++) {
			if (poblacion[padre2]->sol[j] == hijos[i]->sol[j1]) {
				esta = true;
			}
		}
		if (!esta) {
			hijos[i]->sol[asig] = poblacion[padre2]->sol[j];
			asig++;
		}
		for (int j1 = 0; j1 < asig1 && !esta1; j1++) {
			if (poblacion[padre1]->sol[j] == hijos[i + 1]->sol[j1]) {
				esta1 = true;
			}
		}
		if (!esta1) {
			hijos[i + 1]->sol[asig1] = poblacion[padre1]->sol[j];
			asig1++;
		}
	}
	/* Calculamos el costo de los nuevos hijos*/
	setCosto(hijos[i]);
	setCosto(hijos[i + 1]);
	padres[i] = padre1;
	padres[i + 1] = padre2;
	
}

void printEstadoSol(Estado *e) {
	for (int i = 0; i < tam; i++) {
		cout << e->sol[i] << " ";
	}
	cout << endl;
}

/*Dados los padres (padre1, padre2) se hace el cruce (aleatorio) de dos puntos
 * entre dichos dos padres. Los individuos resultantes se almacenan en el
 * arreglo de estados "hijos"*/
void cruce(Estado **poblacion, Estado **hijos, int *padres, int i, int padre1, int padre2) {
	int k1, k2;
	//Seleccionamos puntos de corte
	while (true) {
		k1 = random() % tam;
		k2 = random() % tam;
		if (k1 < k2) {
			break;
		} else if (k2 < k1) {
			int aux = k2;
			k2 = k1;
			k1 = aux;
			break;
		}
	}

	/* Reservamos espacio para la solucion de los hijos */
	hijos[i] = (Estado*) malloc(sizeof (Estado));
	hijos[i + 1] = (Estado*) malloc(sizeof (Estado));
	hijos[i]->sol = (int*) calloc(sizeof (int), tam);
	hijos[i + 1]->sol = (int*) calloc(sizeof (int), tam);
	/* Guardamos los dos hijos del crossover */
	for (int j = 0; j < k1 + 1; j++) {
		hijos[i]->sol[j] = poblacion[padre1]->sol[j];
		hijos[i + 1]->sol[j] = poblacion[padre2]->sol[j];
	}
	/* Guardamos los dos hijos del crossover */
	for (int j = k2; j < tam; j++) {
		hijos[i]->sol[j] = poblacion[padre1]->sol[j];
		hijos[i + 1]->sol[j] = poblacion[padre2]->sol[j];
	}
	int asig = k1 + 1, asig1 = k1 + 1;

	for (int j = 0; j < tam; j++) {
		bool esta = false, esta1 = false;
		for (int j1 = 0; j1 < tam && !esta && asig < k2; j1++) {
			if (poblacion[padre2]->sol[j] == hijos[i]->sol[j1]) {
				esta = true;
			} else if (j1 == asig - 1) {
				j1 = k2 - 1;
			}
		}
		if (!esta && asig < k2) {
			//    cout << " No encontre a " << poblacion[padre1]->sol[j] << ", asig " << asig << " aqui: " << endl;
			//    printEstadoSol(hijos[i]);
			hijos[i]->sol[asig] = poblacion[padre2]->sol[j];
			asig++;
		}
		for (int j1 = 0; j1 < tam && !esta1 && asig1 < k2; j1++) {
			if (poblacion[padre1]->sol[j] == hijos[i + 1]->sol[j1]) {
				esta1 = true;
			} else if (j1 == asig1 - 1) {
				j1 = k2 - 1;
			}
		}
		if (!esta1 && asig1 < k2) {
			//    cout << " No encontre a " << poblacion[padre1]->sol[j] << " aqui: " << endl;
			//    printEstadoSol(hijos[i+1]);
			hijos[i + 1]->sol[asig1] = poblacion[padre1]->sol[j];
			asig1++;
		}
	}

	/* Calculamos el costo de los nuevos hijos*/
	setCosto(hijos[i]);
	setCosto(hijos[i + 1]);

	padres[i] = padre1;
	padres[i + 1] = padre2;
}

/*Hace el cruce seleccionando los padres de forma aleatoria*/
void crossoverRandom(Estado **poblacion, Estado **hijos, int *padres, int nCortes, bool intensificar) {
	/* Hacemos los crossover necesarios */
	for (int i = 0; i < ceil(CROSSOVER * CANTPOBLADORES * 2); i += 2) {
		/* Seleccionamos a los dos pobladores a cruzar */
		int padre1, padre2;
		do {
			padre1 = random() % CANTPOBLADORES;
			padre2 = random() % CANTPOBLADORES;
		} while (padre1 != padre2);
		
		//Genero hijos
		switch (nCortes) {
			case 1:
				cruce1(poblacion, hijos, padres, i, padre1, padre2);
				break;
			case 2:
				
				cruce2(poblacion, hijos, padres, i, padre1, padre2);
				break;
		}
	}
	if (intensificar && random() % 2) {
		for (int i = 0; i < ceil(CROSSOVER * CANTPOBLADORES * 2); i++) {
			hijos[i] = busquedaLocal(hijos[i], false);
		}
	}
}

/*Hace el cruce seleccionando los padres con probabilidades proporcionales al fitness
 * de los mismos*/
void crossoverRuleta(Estado **poblacion, Estado **hijos, int *padres, int nCortes, bool intensificar) {
	long sumFitness = 0;
	for (int i = 0; i < CANTPOBLADORES; i++) {
		sumFitness += poblacion[i]->costo;
	}
	/* Hacemos los crossover necesarios */
	for (int i = 0; i < ceil(CROSSOVER * CANTPOBLADORES * 2); i += 2) {
		double k1 = (double) random() / RAND_MAX, k2 = (double) random() / RAND_MAX, aux = 0.0;
		bool p1 = false, p2 = false;
		int padre1, padre2;
		//Selecciono padres
		for (int j = 0; j < CANTPOBLADORES; j++) {
			aux += (double) poblacion[j]->costo / sumFitness;
			if (!p1 && k1 < aux) {
				p1 = true;
				padre1 = j;
			}
			if (!p2 && k2 < aux) {
				p2 = true;
				padre2 = j;
			}
			if (p1 && p2 && p1 != p2)
				break;
		}
		//Genero hijos
		cruce(poblacion, hijos, padres, i, padre1, padre2);

	}
	if (intensificar && random() % 2) {
		for (int i = 0; i < ceil(CROSSOVER * CANTPOBLADORES * 2); i++) {
			hijos[i] = busquedaLocal(hijos[i], false);
		}
	}
}

}
}

/*Reemplaza los elementos de la población por los hijos (nuevos individuos). Los
 *individuos a eliminar son los padres que generaron los respectivos hijos*/
void hijosPorPadres(Estado **poblacion, Estado **hijos, int *padres) {
	/* Cambiamos la generacion
	 * Este cambio seria de los padres por los hijos.
	 */
	for (int i = 0; i < ceil(CROSSOVER * CANTPOBLADORES * 2); i += 2) {
		destructorEstado(poblacion[padres[i]]);
		if (padres[i] != padres[i + 1])
			destructorEstado(poblacion[padres[i + 1]]);
		poblacion[padres[i]] = clonar(hijos[i]);
		poblacion[padres[i + 1]] = clonar(hijos[i + 1]);
	}
}

/*Dados los valores de las variables globales "SELPADRES" y "SELELIM", se llaman
 *a las funciones de selecciÃ³n de padres y de reemplazo correspondientes*/
void nuevaPoblacion(Estado **poblacion, int nCortes, bool intensificar) {
	Estado **hijos = (Estado**) malloc(sizeof (Estado*) * ceil(CROSSOVER * CANTPOBLADORES * 2));
	int *padres = (int*) malloc(sizeof (int) *ceil(CROSSOVER * CANTPOBLADORES * 2));
	/* Realiza el cruce*/
	crossoverRuleta(poblacion, hijos, padres, nCortes, intensificar);

	/* Realiza la sustitucion*/
	hijosPorPadres(poblacion, hijos, padres);

}

/*Devuelve el individuo de la poblaciÃ³n con menor costo*/
Estado *mejorPoblador(Estado **poblacion) {
	Estado *e;
	int ret = 0;
	for (int i = 1; i < CANTPOBLADORES; i++) {
		if (poblacion[i]->costo < poblacion[ret]->costo)
			ret = i;
	}
	e = clonar(poblacion[ret]);
	return e;
}

/*Operador de "MUTACION"*/
void mutation(Estado **poblacion) {
	for (int i = 0; i < CANTPOBLADORES; i++) {
		if (float(random()) / RAND_MAX <= MUTATION) {
			int j = random() % tam, k = random() % tam;
			int aux = poblacion[i]->sol[j];
			poblacion[i]->sol[j] = poblacion[i]->sol[k];
			poblacion[i]->sol[k] = aux;
		}
	}
}

/*ImplementaciÃ³n de la metaheurí­stica Algoritmo GenÃ©tico*/
Estado * Genetico(int nCortes, bool intensificar) {
	Estado **poblacion = (Estado**) malloc(sizeof (Estado*) * CANTPOBLADORES);
	Estado *ret;
	Estado *aux;
	ret = (Estado*) malloc(sizeof (Estado));
	aux = (Estado*) malloc(sizeof (Estado));
	ret->sol = (int*) malloc(sizeof (int) *tam);
	aux->sol = (int*) malloc(sizeof (int) *tam);
	for (int i = 0; i < CANTPOBLADORES; i++) {
		if (intensificar)
			poblacion[i] = busquedaLocal(getSolInicial(), false);
		else
			poblacion[i] = getSolInicial();
	}
	
	for (int i = 0; i < EPOCAS; i++) {
		nuevaPoblacion(poblacion, nCortes, intensificar);
		mutation(poblacion);
	}
	ret = mejorPoblador(poblacion);
	cout << EPOCAS << endl;
	return ret;
}


int main(int argc, char **argv) {
    int Semilla = 583269;
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
    cout << "Mejor solucion encontrada: "<<endl;
    printEstado(e);

    cout <<"Se han tardado " << segundos << " segundos." <<endl;

    return EXIT_SUCCESS;
}
