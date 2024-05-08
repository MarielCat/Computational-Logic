*Lógica Computacional 2024-2 Practica 4*

Integrantes:
- Del Monte Ortega Maryam Michelle
- Monroy Romero Sahara Mariel

Explicación de ejercicios:

** esVariable
Función que nos dice si un término es una variable.

** variables
Función que dado un término, regresa su lista de variables. Aquí usamos una función auxiliar llamada elim       
    ** elim 
        Función que elimina las variables repetidas de una lista de variables

** variablesEnLista
Función que regresa la lista de variables dada una lista de términos.

** epsilon
Función que representa a la sustitución identidad.

** dominio
Función que dada una sustitución, obtenemos su dominio. Aquí usamos una función auxiliar llamada dominioAux:
    ** dominioAux 
        Función auxiliar que regresa una lista con la primer entrada de la pareja

** aplicaVar 
Función que dada una sustitución y una variable, regresa la aplicación de la sustitución a la variable.

** aplicaT
Función que dada una sustitución y un término, regresa la aplicación de la sustitución al término.

** reduce 
Función que regresa la sustitución obtenida, eliminando los pares cuyos elementos son iguales. Aquí usamos una función auxiliar llamada reduceAux:
    **reduceAux 
    Funcion auxiliar que regresa una lista vacía si la primer entrada de la pareja es igual a la segunda.

** composicion 
Función que dadas dos sustituciones, regresa su composición.

** unifica

Función que dados dos términos, regresa la lista formada por el unificador más general de ambos términos. Si no son unificables, regresa la lista vacía.
Esta función toma dos términos y realiza diferentes comparaciones para determinar si los términos pueden unificarse.
Si ambos términos son variables entonces compara sus nombres, si son iguales, devuelve la sustitución de identidad ([epsilon]),si no devuelve una sustitución que iguala las variables.
Si uno de los términos es una variable y el otro es un término más complejo, se verifica si la variable no está presente en la lista de variables del otro término. En ese caso, devuelve una sustitución que iguala la variable con el término, si no devuelve una lista vacía [].
Si ambos términos son términos de función, la función verifica si los nombres de las funciones son iguales. Si son iguales, procede a unificar las listas de términos (l1 y l2) utilizando la función unificaListas.

** unificaListas
Función que regresa la lista formada por el unificador más general de las listas de términos.
Toma dos listas de términos como entrada y busca el unificador más general.

Si ambas listas están vacías, la función devuelve una única sustitución que representa la identidad ([epsilon]), indicando que las listas vacías son unificables de manera trivial.

Si una de las listas es vacía y la otra no, entonces las listas no pueden unificarse y la función devuelve una lista vacía, indicando que no hay unificador más general posible entre las dos listas.

Para listas no vacías, la función toma el primer término de cada lista y utiliza la función unifica para encontrar todas las posibles sustituciones que pueden unificar estos dos términos. Luego, para cada sustitución obtenida, la función aplica esta sustitución a los términos restantes en ambas listas utilizando la función aplicaT. Finalmente, compone estas sustituciones resultantes con la sustitución original mediante la función composicion, generando así una lista de sustituciones que representa el unificador más general de las listas originales.