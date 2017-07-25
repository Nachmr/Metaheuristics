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
 *   Solo debe llamarse a esta funcion una vez en todo el programa */
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
 *   (incluyendo el 0 pero sin incluir el 1) */
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
 *      FUNCTION:       virtual and real time of day are computed and stored to
 *                      allow at later time the computation of the elapsed time
 *		      (virtual or real)
 *      INPUT:          none
 *      OUTPUT:         none
 *      (SIDE)EFFECTS:  virtual and real time are computed
 */
{
	start_time = clock();
}



double elapsed_time()
/*
 *      FUNCTION:       return the time used in seconds (virtual or real, depending on type)
 *      INPUT:          TIMER_TYPE (virtual or real time)
 *      OUTPUT:         seconds since last call to start_timers (virtual or real)
 *      (SIDE)EFFECTS:  none
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

unsigned long MAX_EVALUACIONES = 25000;
int evaluaciones = 0;

int n_hormigas = 10;
int tamPoblacion = n_hormigas;

/*Lee el archivo con la instancia en el formato estandar de QAPLIB generando
 * la matriz de distancia y flujo.*/
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
	unsigned int costo;
	bool optimizado = 0;
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
	float heuristica;
	float num;
	float den;
} Ord;

/*Dado un estado, se le calcula el costo*/
void setCosto(Estado* &e) {
	e->costo = 0;
	for (int i = 0; i < tam; i++) {
		for (int j = 0; j < tam; j++) {
			e->costo += flujo[i][j] * distancia[e->sol[i]][e->sol[j]];
		}
	}
	
	evaluaciones++;
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
	
	evaluaciones++;
	return deltaC;
}

/*En el estado "e" se intercambian las localidades entre las instalaciones
 *  "a" y "b"*/
Estado* intercambialocalizaciones(Estado* &e, int a, int b) {
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
		orden[i]->valor = 0;
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
		orden[i]->valor = 0;
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
	
	for (unsigned int i = 0; i < tam; i++)
		greedy->sol[orden_flujo[i]->elem] = orden_dist[i]->elem;
	
	setCosto(greedy);
	
	return greedy;
}

//Busqueda Local con Don't look bits
Estado* busquedaLocal(Estado* actual) {
	
	bool improve_flag=true;
	bool dlb[tam];
	
	for(unsigned int i = 0; i < tam; i++)
		dlb[i] = 0;
	
	while (improve_flag){
		improve_flag = false;
		for (unsigned int i = 0; i < tam && !improve_flag; i++){
			if (dlb[i] == 0){
				
				improve_flag = false;
				for (unsigned int j = 0; j < tam && !improve_flag; j++){
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


void avanzar(Estado* &hormiga, const int paso, const vector<vector<float> > feromona, vector<Ord*> pflujo, const int alpha, const int beta, const float q0){
	
	bool localizaciones[tam];
	
	for(unsigned int i = 0; i < tam; i++)
		localizaciones[i] = 0;
	
	
	for(unsigned int i = 0; i < paso; i++)
		localizaciones[hormiga->sol[pflujo[i]->elem]]= 1;
	
	
	vector<Ord*> candidatos;
	for(unsigned int j = 0; j < tam; j++){
		if(localizaciones[j] == 0){
			Ord* nuevocandidato = new Ord;
			nuevocandidato->elem = j;
			nuevocandidato->heuristica = 0;
			candidatos.push_back(nuevocandidato);
		}
	}
	
	
	//Calculo la heuristica de cada candidato
	for(unsigned int j = 0; j < candidatos.size(); j++){
		float HeuristicaC = 0.0;
		for (unsigned int k = 0; k < candidatos.size(); k++)
			HeuristicaC += distancia[candidatos[j]->elem][candidatos[k]->elem] + distancia[candidatos[k]->elem][candidatos[j]->elem];
		if(HeuristicaC==0.0){
			HeuristicaC = pow(10.0,-6.0);
		}
		candidatos[j]->heuristica = 1.0/HeuristicaC;
	}
	
	
	for(unsigned int j = 0; j < candidatos.size(); j++){
		candidatos[j]->num = pow(candidatos[j]->heuristica, beta) * pow(feromona[pflujo[paso]->elem][candidatos[j]->elem], alpha);

	}
	
	float aleatorio = Rand();
	int localizacion;
	if(aleatorio <= q0){
		int mejor = 0;
		for(unsigned int j = 0; j < candidatos.size(); j++){
			if(candidatos[j]->num > candidatos[mejor]->num){
				mejor = j;
			}
		}
		
		localizacion = candidatos[mejor]->elem;
		
	}else{
		
		//Hago la ruleta para los candidatos
		
		float den=0.0;
		for(unsigned int i = 0; i < candidatos.size(); i++ )
			den += candidatos[i]->num;
		
		for(unsigned int i = 0; i < candidatos.size(); i++ )
			candidatos[i]->den = candidatos[i]->num/den;
		
		
		//Ruleta
		float ruleta[candidatos.size()];
		for(unsigned int j = 0; j < candidatos.size(); j++ )
			(j == 0)?ruleta[j] = candidatos[j]->den : ruleta[j] = ruleta[j-1] + candidatos[j]->den;
			
		
		
		//Lanzo aleatorio entre 0 y 1
		float eleccion = Rand();
		
		int j = 0;
		if(candidatos.size() > 1){
			while(ruleta[j]<eleccion){
				j++;
			}
			localizacion = candidatos[j]->elem;
		}else{
			localizacion = candidatos[0]->elem;
		}
	}
	
	
	hormiga->sol[pflujo[paso]->elem] = localizacion;
	
}

int buscarMejorSolucion(Estado** poblacion){
	int mejorcosto = poblacion[0]->costo;
	int posMejor = 0;
	for(unsigned int i = 1; i < tamPoblacion; i++){
		if(poblacion[i]->costo < mejorcosto){
			mejorcosto = poblacion[i]->costo;
			posMejor = i;
		}
	}
	return posMejor;
}


Estado* SCH(){

	float feromona_ini = pow(10,-6);
	int alpha = 1;
	int beta = 0;
	float evaporacion_global = 0.2;
	float evaporacion_local = 0.2;
	float q0 = 0.9;
	
	vector<Ord*> pflujo = ordenarXflujo();
	
	Estado* mejorencontrada = new Estado;
	mejorencontrada->costo = LONG_MAX;

	
	//Inicializo la matriz de feromona
	vector<vector<float> > feromona(tam);
	
	for(unsigned int i = 0; i<tam; i++)
		feromona[i].resize(tam);
	
	for(unsigned int i = 0; i < tam; i++){
		for(unsigned int j = 0; j < tam; j++){
			feromona[i][j] = feromona_ini;
		}
	}
	
	//Algoritmo
	
	Estado** caminos;	
	while (evaluaciones < MAX_EVALUACIONES){
		caminos = new Estado*[n_hormigas];
		
		for(unsigned int i = 0; i < n_hormigas; i++){
			caminos[i] = new Estado;
			for(unsigned int j = 0; j < tam; j++){
				caminos[i]->sol[j] = -1;
				caminos[i]->optimizado = 0;
			}
		}
		
		for(unsigned int i = 0; i < tam; i++){
			for(unsigned int j = 0; j < n_hormigas; j++){
				avanzar(caminos[j], i, feromona, pflujo, alpha, beta, q0);
				feromona[pflujo[i]->elem][caminos[j]->sol[pflujo[i]->elem]] = (1.0-evaporacion_local)*feromona[pflujo[i]->elem][caminos[j]->sol[pflujo[i]->elem]] + evaporacion_local * feromona_ini;
			}
		}
				
		for(unsigned int i = 0; i < n_hormigas; i++)
			setCosto(caminos[i]);
		
		
		int idx = buscarMejorSolucion(caminos);
		busquedaLocal(caminos[idx]);
		
		if(caminos[idx]->costo < mejorencontrada->costo){
			destructorEstado(mejorencontrada);
			mejorencontrada = copiar(caminos[idx]);
		}
		
		for(unsigned int i = 0; i < tam; i++){
			feromona[i][mejorencontrada->sol[i]] = ((1.0-evaporacion_global)*feromona[i][mejorencontrada->sol[i]]) + (evaporacion_global / mejorencontrada->costo);
		}
		
	}
	
	return mejorencontrada;
}

int buscarPeorSolucion(Estado** poblacion){
	int peorcosto = poblacion[0]->costo;
	int posPeor = 0;
	for(unsigned int i = 1; i < tamPoblacion; i++){
		if(poblacion[i]->costo > peorcosto){
			peorcosto = poblacion[i]->costo;
			posPeor = i;
		}
	}
	return posPeor;
}

Estado* SHMM(){
	Estado* inicializa = getSolAleatoria();
	vector<Ord*> pflujo = ordenarXflujo();
	
	float q0 = 0.9;
	float evaporacion_global = 0.2;
	int alpha = 1;
	int beta = 0;
	float t_max = 1.0/(evaporacion_global * inicializa->costo);
	float t_min = t_max/500.0;
	
	int sin_mejorar = 0;
	
	Estado* mejorencontrada = new Estado;
	mejorencontrada = copiar(inicializa);
	Estado** caminos;
	
	//matriz de feromona
	vector<vector<float> > feromona(tam);
	
	for(unsigned int i = 0; i<tam; i++)
		feromona[i].resize(tam);
	

	for(unsigned int i = 0; i < tam; i++){
		for(unsigned int j = 0; j < tam; j++){
			feromona[i][j] = t_max;
		}
	}
	
	//Algoritmo
	
	while (evaluaciones < MAX_EVALUACIONES){
		caminos = new Estado*[n_hormigas];
		
		for(unsigned int i = 0; i < n_hormigas; i++){
			caminos[i] = new Estado;
		}
		
		for(unsigned int paso = 0; paso < tam; paso++){
			for(int hormiga = 0; hormiga < n_hormigas; hormiga++){
				avanzar(caminos[hormiga], paso, feromona, pflujo, alpha, beta, q0);
			}
		}
		
		for(unsigned int hormiga = 0; hormiga < n_hormigas; hormiga++){
			setCosto(caminos[hormiga]);
		}
		

		int idx = buscarMejorSolucion(caminos);
		busquedaLocal(caminos[idx]);
		
		if(caminos[idx]->costo < mejorencontrada->costo){
			destructorEstado(mejorencontrada);
			mejorencontrada = copiar(caminos[idx]);
			
			t_max = 1.0/(evaporacion_global * mejorencontrada->costo);
			t_min = t_max/500.0;
			
		}
		
		
		//Se evaporan las feromonas
		for(unsigned int i = 0; i < tam; i++){
			for(int j = 0; j < tam; j++){
				feromona[i][j] = feromona[i][j] *(1.0 - evaporacion_global);
			}
		}
		
		int idx_peor = buscarPeorSolucion(caminos);
		
		for(unsigned int i = 0; i < tam; i++){
			feromona[i][caminos[idx]->sol[i]] = ((1.0-evaporacion_global)*feromona[i][caminos[idx]->sol[i]]) + (1.0 / caminos[idx]->costo);
		}
		
		//Truncar si se supera el maximpo por los decimales
		for( unsigned int i = 0; i < tam; i++ ){
			for( unsigned int j = 0; j < tam; j++ ){
				
				if(feromona[i][j] < t_min){
					feromona[i][j] = t_min;
				}
				
				if(feromona[i][j] > t_max){
					feromona[i][j] = t_max;
				}
			}
		}
		
		
	}
	
	return mejorencontrada;
}



int main(int argc, char **argv) {
	int Semilla = atoi(argv[3]);
	Set_random(Semilla);
	
	string uso = "./practica4 <Metaheuristica> <archivo de entrada>\n\n"
	"1) Solucion Greedy\n"
	"2) SCH\n"
	"3) SHMM\n";
	
	if (argc < 3) {
		cout << uso << endl;
		return EXIT_FAILURE;
	}
	
	Estado* e;
	int metaheuristica = atoi(argv[1]);
	
	leer(argv[2]);
	start_timers();
	
	//Ejecuto la metaheuri­stica dada por el identificador
	switch (metaheuristica) {
		case 1:
			e = getSolGreedy();
			break;
		case 2:
			e = SCH();
			break;
		case 3:
			e = SHMM();
			break;
		default:
			cout << "Identificador de metaheuristica invalido" << endl;
			return EXIT_FAILURE;
	}
	
	double segundos = elapsed_time();
	
	
	cout  << e->costo << endl;
	cout << segundos <<endl;
	
	return EXIT_SUCCESS;
}
