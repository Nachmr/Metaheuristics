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
#include "random_ppio.h"
#include "timer.h"

using namespace std;

#define TAM_LISTA_TABU 20
#define MAX_ITERACIONES_SIN_CAMBIO 1000

/*Clase para la gestión de las Matrices*/
class Matriz{
public:
    unsigned dim;
    int ** datos;
    Matriz(unsigned d){
        dim = d;
        this->datos = new int*[dim];
        this->datos[0] = new int[dim*dim];
        for(unsigned i = 1; i<dim ; ++i){
            this->datos[i] = &this->datos[0][dim*i];
        }
    }

    ~Matriz(){
        delete[] datos;
    }
};

/*Representa una solución para el problema
 */
typedef struct estado {
    int *sol;
    long costo;
} Estado;

/*Estructura destinada a facilitar el ordenamiento en las metaheurísticas greedy*/
typedef struct ord {
    long valor;
    int elem;
    int elem2;
} Ord;

/*Función destructora de estados*/
void destructorEstado(Estado *e) {
    free(e->sol);
    free(e);
}

/*Declaración de matrices de flujo y distancia*/
unsigned dim;
Matriz * flujo;
Matriz * distancia;

/*Función destructora de vector de estados*/
void destructorVecindad(Estado **e) {
    unsigned size = dim * (dim - 1) / 2;
    for (unsigned i = 0; i < size; ++i)
        destructorEstado(e[i]);
    free(e);
}

/*Dado un estado, se le calcula el estado y se le asigna en su parametro "costo"*/
void setCosto(Estado* e) {
    e->costo = 0;
    for (unsigned i = 0; i < dim; ++i) {
        for (unsigned j = 0; j < dim; ++j) {
            e->costo += flujo->datos[i][j] * distancia->datos[e->sol[i]][e->sol[j]];
        }
    }
}

/*Calcula el costo factorizado entre dos localizaciones de un estado e*/
int setCostoFactorizado(Estado* e, int l, int r){
    int deltaC=0;

    for(int i=0; i<dim; ++i){
        if(i!=l && i!=r){
            deltaC +=
                flujo->datos[l][i] * (distancia->datos[e->sol[r]][e->sol[i]] - distancia->datos[e->sol[l]][e->sol[i]])
                + flujo->datos[r][i] * (distancia->datos[e->sol[l]][e->sol[i]] - distancia->datos[e->sol[r]][e->sol[i]])
                + flujo->datos[i][l] * (distancia->datos[e->sol[i]][e->sol[r]] - distancia->datos[e->sol[i]][e->sol[l]])
                + flujo->datos[i][r] * (distancia->datos[e->sol[i]][e->sol[l]] - distancia->datos[e->sol[i]][e->sol[r]]);
        }
    }
}


/*Devuelve una solución aleatoria*/
Estado* getSolInicial() {
    vector <int> a;
    for (int i = 0; i < dim; i++) {
        a.push_back(i);
    }
    Estado *ret = (Estado*) malloc(sizeof (Estado));
    ret->sol = (int*) malloc(sizeof (int) *dim);
    for (int i = 0; i < dim; i++) {
        int aux = rand() % (dim - i);
        ret->sol[i] = a[aux];
        a.erase(a.begin() + aux);
    }
    setCosto(ret);
    return ret;
}

/*Dado un puntero a un estado, devuelve un puntero a un estado idéntico*/
Estado* clonar(Estado*e) {
    Estado *ret = (Estado*) malloc(sizeof (Estado));
    ret->sol = (int*) malloc(sizeof (int) *dim);
    ret->costo = e->costo;
    for (unsigned i = 0; i < dim; ++i) {
        ret->sol[i] = e->sol[i];
    }
    return ret;
}

/*Copiar un estado en otro estado*/
void copiar(Estado* src, Estado *dst) {
    dst->costo = src->costo;
    for (unsigned i = 0; i < dim; ++i)
        dst->sol[i] = src->sol[i];
}

void imprimirMatriz() {
    cout << "Matriz flujo:\n";
    for (unsigned i = 0; i < flujo->dim; ++i){
        for(unsigned k = 0; k < flujo->dim; ++k){
            cout << flujo->datos[i][k] << " ";
        }
        cout << endl;
    }

    cout << "Matriz distancia:\n";
    for (unsigned i = 0; i < distancia->dim; ++i){
        for(unsigned k = 0; k < distancia->dim; ++k){
            cout << distancia->datos[i][k] << " ";
        }
        cout << endl;
    }
}

void imprimirMatriz(Matriz *flujo1, Matriz *distancia1) {
    cout << " FLUJO " << endl;
    for (unsigned i = 0; i < dim; ++i) {
        for (unsigned j = 0; j < dim; ++j) {
            cout << flujo1->datos[i][j] << " ";
        }
        cout << endl;
    }
    cout << endl << " DISTANCIA" << endl;
    for (unsigned i = 0; i < dim; ++i) {
        for (unsigned j = 0; j < dim; ++j) {
            cout << distancia1->datos[i][j] << " ";
        }
        cout << endl;
    }
}

/*Para comprobar si se han cargado bien las matrices*/
bool EscribirImagenPGM (const char nombre[], const unsigned char datos[], int filas, int columnas)
{
  ofstream f(nombre);
  bool res= true;

  if (f) {
    f << "P5" << endl;
    f << columnas << ' ' << filas << endl;
    f << 255 << endl;
    f.write(reinterpret_cast<const char *>(datos),filas*columnas);
    if (!f) res=false;
  }
  return res;
}

void LecturaFicheroDAT(Matriz * m, ifstream &archivodat){
    string line;
    getline(archivodat , line); //extrae la linea del stream y ya está no se hace nada mas con ella.
    //ahora te haces un for que itere dim veces, y vas cogiendo los datos;
    for(unsigned i = 0; i < m->dim; ++i){                                // este bucle itera cada fila de la matriz
        unsigned k=0;
        size_t next_pos, esp_pos;
        int valor;
        getline(archivodat, line);                              //elimina los posibles espacios en blanco al principio, garantiza que el primer char es un numero, no un espacio
        //cout << "Linea : |" << line << endl;
        line = line.substr(line.find_first_of("0123456789"));   //se sustituye la cadena, por ella misma a partir de la posición del primer número(se eliminan los espacios)

        for(k = 0; k < m->dim; ++k){                                //este bucle itera cada valor de la fila
            //cout << "k = " << k << ":\t|" << line << endl;
            esp_pos = line.find(" ");                               // posicion del siguiente espacio
            if(esp_pos == -1) esp_pos = 0;                          //comprobación: si no se encuentra ningun espacio se toma como referencia la primera posicion del string
            valor = atoi( line.substr(0, esp_pos ).c_str() );       //el valor es la subcadena entre la primera posicion y la posición del próximo espacio
            m->datos[i][k] = valor;

            next_pos = line.find_first_of("0123456789", esp_pos);   //posición del siguiente número, buscando a partir del próximo espacio
            if(next_pos == -1) next_pos = line.length()-1;          //comprobación: si no encuentra ningun número, la posicion será la última de la cadena

            line = line.substr( next_pos );                         //se sustituye la linea, se elimina el valor leído y los espacios que le sucedan.
        }
    }
}

/*Imprime por stdout el estado dado*/
void printEstado(Estado* e) {
    cout << "Costo de la solucion: " << e->costo << endl << endl;
    cout << "Vector solucion: " << endl;
    for (unsigned i = 0; i < dim; i++) {
        cout << e->sol[i] << " ";
    }
    cout << endl;
}

/*En el estado "e" se intercambian las localidades entre las instalaciones
 * "a" y "b"*/
Estado* swap(Estado* e, int a, int b) {
    Estado *ret = (Estado*) malloc(sizeof (Estado));
    ret->sol = (int*) malloc(sizeof (int) *dim);
    for (unsigned i = 0; i < dim; i++) {
        ret->sol[i] = e->sol[i];
    }
    int aux = ret->sol[a];
    ret->sol[a] = ret->sol[b];
    ret->sol[b] = aux;
    ret->costo = e->costo + setCostoFactorizado(e,a,b) ;
    return ret;
}

/*Dado un estado, se devuelve la vecindad del mismo. La vecindad viene dada
 por todos los estados conseguidos al intercambiar la posición de dos instalaciones
 cualquiera*/
Estado** getVecindades(Estado* e) {
    Estado **ret = (Estado**) malloc(sizeof (Estado) * dim * (dim - 1) / 2);
    Estado *aux;
    unsigned cont = 0;
    for (unsigned i = 0; i < dim; i++) {
        for (unsigned j = i + 1; j < dim; j++) {
            aux = swap(e, i, j);
            ret[cont] = aux;
            cont++;
        }
    }
    return ret;
}



/*BusquedaLocal con la técnica de Don't look bits.*/
Estado* busquedaLocal(Estado* e) {
    //Estado** vecindades;

    /*Creación de la máscara Don't look bits*/
    Estado *vecino;
    vector<int> DLB;
    for(unsigned i=0; i<dim; ++i){
        DLB.push_back(0);
    }
    for(unsigned i=0; i<dim;++i){
        if(DLB.at(i) == 0){ //si lo puedo mirar
            for(unsigned j=0;j<dim;++j){
                vecino = swap(e,i,j);
                if(vecino->costo < e->costo){
                    copiar(vecino,e);
                }
                DLB.at(i) = DLB.at(j) = 0;
            }
        }
    }

    /*Algoritmo busqueda local*/
    Estado *inter;
    for(unsigned i=0;i<dim;++i){

        if(DLB.at(i)==0){
            for(unsigned j=0;j<dim;++j){
                inter = swap(e,i,j);
                if(inter->costo < e->costo)
                    e->costo = inter->costo;
                    e->sol = inter ->sol;
                    DLB.at(i) = DLB.at(j) = 0;
            }
            if(e->costo != inter->costo)
                DLB.at(i)=1;
        }
    }

    return e;
}

/*Devuelve verdadero si dos estados son idénticos y falso en caso contrario*/
bool estadosIguales(Estado *a, Estado *b) {
    bool iguales = 1;
    if (a == NULL || a->sol == NULL) {
        cout << "Estados nulos." << endl;
        exit(0);
    }
    if (b == NULL)
        return 0;
    for (unsigned i = 0; i < dim; i++) {
        if (a->sol[i] != b->sol[i]) {
            iguales = 0;
            break;
        }
    }
    return iguales;
}

/*Implementación de la metaheurística Búsqueda Tabú*/
Estado* tabu(Estado *ret) {
    int i = 0, j = 1;
    Estado **lTabu = (Estado **) malloc(sizeof (Estado*) * TAM_LISTA_TABU);
    Estado** vecindades;
    Estado *current = ret;
    int mejor = ret->costo, iteraciones = 0;
    lTabu[0] = ret;
    //Inicializo lTabu
    for (unsigned k = 0; k < TAM_LISTA_TABU; k++)
        lTabu[k] = NULL;
    //Mientras no se cumpla la condicion de parada
    while (i < dim*10) {
        iteraciones++;
        vecindades = getVecindades(current);
        //Le coloco un costo muy alto a los estados que se encuentran en la lista Tabu.
        for (unsigned a = 0; a < dim * (dim - 1) / 2; a++) {
            for (unsigned b = 0; b < TAM_LISTA_TABU; b++) {
                if (estadosIguales(vecindades[a], lTabu[b])) {
                    vecindades[a]->costo = LONG_MAX;
                }
            }
        }
        //Busco la mejor solucion de la vecindad que no este en lTabu
        for (unsigned k = 0; k < dim * (dim - 1) / 2; k++) {
            if (k == 0 || vecindades[k]->costo < current->costo)
                current = vecindades[k];
        }

        //Libero la memoria del objeto anterior de la lista tabu
        if (lTabu[j] != NULL)
            destructorEstado(lTabu[j]);
        lTabu[j] = current;

        if (current->costo < mejor) {
            free(ret);
            ret = clonar(current);
            mejor = ret->costo;
            i = -1;
        }

        i++;
        j++;
        j = j % TAM_LISTA_TABU;
    }
    return ret;
}

float setTemperaturaInicial(Estado *e){
    float Tempin =0;
    Tempin = (0.3 * e->costo)/(-log(0.3));
    return Tempin;
}


/*Implementación de la metaheurística Enfriamiento Simulado*/
Estado *SA() {
    Estado *current = getSolInicial();
    Estado *best = clonar(current);
    float ro = 1.05, delta = 0.86, T = setTemperaturaInicial(current), K = 7 * dim;
    cout << T << endl;
    int A = 2 * dim, iteraciones = 0;
    while (iteraciones < 10*dim and T>0.001) {
            //cout << T << endl;
        int a = 0, k = 0;
        while (k < K && a < A) {

            Estado ** vecindad = getVecindades(current);
            for (unsigned i = 0; i < dim * (dim - 1) / 2 && k < K && a < A; i++) {
                Estado *aux = vecindad[i];
                float cDif = aux->costo - current->costo;
                float random = Randfloat(0,RAND_MAX);
                float expo = exp(-cDif / T);
                k++;
                if (cDif < 0 || random < expo) {
                    free(current);
                    current = clonar(aux);
                    a++;
                    if (aux->costo < best->costo) {
                        free(best);
                        best = clonar(aux);
                        iteraciones = -1;
                    }
                    break;
                }
            }
            //Limpio la vecindad actual
            for (unsigned i = 0; i < dim * (dim - 1) / 2; i++) {
                Estado * aux = vecindad[i];
                if (aux != current && aux != best)
                    free(vecindad[i]);
            }
            free(vecindad);
        }
        T = delta*T;
        K = ro*K;
        iteraciones++;
    }
    setCosto(best);
    return best;
}

/*Función para ordenamiento de Mayor a menor*/
bool ordenMam(Ord *a, Ord* b) {
    return (a->valor > b->valor);
}

/*Función para ordenamiento de menor a Mayor*/
bool ordenmaM(Ord *a, Ord* b) {
    return (a->valor < b->valor);
}

/*Devuelve un vector<Ord *> con las instalaciones ordenadas por flujo*/
vector<Ord *> ordenarXflujo() {
    vector<Ord *> orden(dim);

    for (unsigned i = 0; i < dim; i++) {
        orden[i] = (Ord*) malloc(sizeof (Ord));
        orden[i]->elem = i;
        for (unsigned j = 0; j < dim; j++)
            orden[i]->valor += flujo->datos[i][j];
    }

    sort(orden.begin(), orden.end(), ordenMam);

    return orden;
}

/*Devuelve un vector<Ord *> con las instalaciones ordenadas por distancia*/
vector<Ord *> ordenarXdist() {
    vector<Ord *> orden(dim);

    for (unsigned i = 0; i < dim; i++) {
        orden[i] = (Ord*) malloc(sizeof (Ord));
        orden[i]->elem = i;
        for (unsigned j = 0; j < dim; j++)
            orden[i]->valor += distancia->datos[i][j];
    }

    sort(orden.begin(), orden.end(), ordenmaM);

    return orden;
}

/*Implementación de la solución greedy*/
Estado *getSolGreedy() {
    Estado *greedy = (Estado *) malloc(sizeof (Estado));
    greedy->sol = (int *) malloc(sizeof (int) *dim);

    vector<Ord*> orden_flujo = ordenarXflujo();
    vector<Ord*> orden_dist = ordenarXdist();
    for (unsigned i = 0; i < dim; i++)
        greedy->sol[orden_flujo[i]->elem] = orden_dist[i]->elem;
    setCosto(greedy);
    return greedy;
}




int main(int argc, char **argv) {
    Set_random(atoi(argv[3]));
    string uso = "./qap <Metaheuristica> <archivo de entrada> <semilla>\n\n"
            "1) Greedy\n"
            "2) Busqueda Local\n"
            "3) Enfriamiento Simulado\n"
            "4) Tabú\n";

    if (argc < 4) {
        cout << uso << endl;
        return EXIT_FAILURE;
    }

    Estado* e;
    int metaheuristica = atoi(argv[1]);

    //Se lee la instancia de entrada
    ifstream archivodat;
    archivodat.open(argv[2],ifstream::in);
    string line;
    getline(archivodat, line);
    dim = atoi(line.c_str());
    flujo = new Matriz(dim);
    distancia = new Matriz(dim);
    LecturaFicheroDAT(flujo, archivodat);
    LecturaFicheroDAT(distancia, archivodat);

    //comprobación de que se ha leido bien el fichero DAT (si la imagen es negra entera esta bien cargada)
    EscribirImagenPGM("flujo.pgm",reinterpret_cast<unsigned char *>(flujo->datos[0]), dim, dim);
    EscribirImagenPGM("distancia.pgm",reinterpret_cast<unsigned char *>(distancia->datos[0]), dim, dim);

	double tiempo = 0;
    //Ejecuto la metaheurística dada por el identificador
    switch (metaheuristica) {
        case 1:
			start_timers();
            e = getSolGreedy();
			tiempo = elapsed_time();
            cout << "Tiempo: " << tiempo << endl;
			break;
        case 2:
			start_timers();
            e = getSolInicial();
            e = busquedaLocal(e);
			tiempo = elapsed_time();
            cout << "Tiempo: " << tiempo << endl;
			break;
		case 3:
			start_timers();
            e = SA();
			tiempo = elapsed_time();
            cout << "Tiempo: " << tiempo << endl;
            break;
        case 4:
			start_timers();
            e = getSolInicial();
            e = tabu(e);
			tiempo = elapsed_time();
            cout << "Tiempo: " << tiempo << endl;
            break;
        default:
            cout << "Identificador de metaheurística inválido" << endl;
            return EXIT_FAILURE;
    }

    printEstado(e);
    return EXIT_SUCCESS;
}
