
# Práctica 2 - Lógica Proposicional

## Integrantes del equipo:
Del Monte Ortega Maryam Michelle
Monroy Romero Sahara Mariel 


## Descripción de la implementación de los ejercicios:

### 1. Definición de tipos y operadores
En este ejercicio, definimos los tipos `Atom` y `State` para representar variables proposicionales y estados, respectivamente. También definimos el tipo de dato `Prop` para representar fórmulas proposicionales, junto con su instancia de la clase `Show`. Asimismo, implementamos la instancia de la clase `Operadores` para `Prop`, que define los operadores lógicos básicos: negación, conjunción, disyunción, implicación y equivalencia.

### 2. Función para obtener las variables de una fórmula
La función `vars` recibe una fórmula proposicional y devuelve la lista de variables que aparecen en ella. Se utiliza tanto en la eliminación de equivalencias como en el cálculo de modelos y posibles interpretaciones.

### 3. Función de interpretación de fórmulas
La función `interp` recibe un estado y una fórmula proposicional, y devuelve `True` si la fórmula es verdadera en el estado dado, y `False` en caso contrario. Esta función es fundamental para verificar modelos de fórmulas y es utilizada en la implementación de las funciones `esModelo` y `todosModelos`.

### 4. Eliminación de equivalencias e implicaciones
Las funciones `elimEquiv` y `elimImpl` se encargan de eliminar las equivalencias e implicaciones respectivamente, utilizando reglas de transformación adecuadas. Estas funciones son aplicadas antes de realizar otras operaciones sobre las fórmulas.

### 5. Cálculo de todas las posibles interpretaciones
La función `posiblesInterp` recibe una fórmula y devuelve una lista con todas las posibles interpretaciones que podrían satisfacerla. Esta función utiliza la función `vars` para obtener las variables presentes en la fórmula.

### 6. Verificación de modelos y satisfacibilidad
Las funciones `esModelo`, `todosModelos`, `esSatisfacible`, `esInsatisfacible`, `esTautologia` y `esContradiccion` permiten verificar diferentes propiedades de las fórmulas proposicionales, como la satisfacibilidad, tautología y contradicción. Estas funciones hacen uso de las funciones anteriores para calcular los modelos y posibles interpretaciones.

### 7. Función auxiliar para calcular la potencia de un conjunto
La función `potencia` calcula la potencia de un conjunto dado, lo cual es utilizado en `posiblesInterp` para generar todas las posibles combinaciones de valores de verdad para las variables de una fórmula.
