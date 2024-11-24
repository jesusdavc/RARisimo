#### Universidad Simón Bolívar
#### Departamento de Computación y Tecnología de la Información
#### CI-3661 – Laboratorio de Lenguajes de Programación
#### Septiembre–Diciembre 2024

# Proyecto I: RARísimo

### Estudiantes:

#### Sergio Carrillo - Carnet: 14-11315
#### Jesús  Cuéllar  - Carnet: 15-10345
#### Néstor Herrera  - Carnet: 18-10796

## **Descripción del Proyecto**

El proyecto "RARísimo" consiste en desarrollar un sistema de compresión y descompresión de archivos utilizando algoritmos basados en árboles de Hoffman implementados en Haskell. Este sistema tiene como objetivo operar en un entorno limitado, mostrando el funcionamiento básico y emulando este sistema de compresión/descompresión.

## **Estructura**

### **1. Modulos Principales**

1. **`Frecuencia.hs`**  
   Este módulo define el tipo de datos `Frecuencia` para contar las ocurrencias de caracteres en una cadena. Incluye:
   - Funciones para inicializar y contar frecuencias.
   - Acceso al valor y la frecuencia.
   - Instancias de las clases `Show` y `Ord` para manejar comparaciones y representaciones.

2. **`Hoffman.hs`**  
   Contiene la implementación del tipo de datos `Hoffman` (árbol de Hoffman) y las funciones asociadas:
   - Construcción: `nuevoHoffman` y `fusionHoffman`.
   - Acceso: `obtenerCaracter`, `arbolIzquierdo` y `arbolDerecho`.
   - Transformación: `codificacion`, para generar códigos binarios a partir del árbol.
   - En la instancia `Read`, en lugar de cumplirse que `arbol = read (show arbol)`, se cumple que `arbol = read (show arbol) :: Hoffman`, ya que es necesario especificar que nos referimos a la instancia `read` de Hoffman al hacer esta comparación.

3. **`RARisimo.hs`**  
   Implementa el algoritmo de construcción del árbol de Hoffman basado en frecuencias, incluyendo:
   - `frecuencias`: Genera las frecuencias a partir de una cadena.
   - `ganadores`: Encuentra las dos frecuencias más pequeñas.
   - `hoffman`: Construye el árbol de Hoffman.
   - `rarisimo`: Genera la codificación asociada a cada carácter.

4. **`Cliente.hs`**  
   Implementa la interfaz de usuario y el flujo principal del programa (`main`).  
   Incluye las siguientes funcionalidades:
   - **Codificar**: Convierte un archivo en uno comprimido con extensión `.raro`. Puede leer rutas relativas y absolutas.
   - **Decodificar**: Restaura un archivo `.raro` a su estado original. Puede leer rutas relativas y absolutas.
   - **Analizar**: Muestra el tamaño original, el tamaño comprimido y el porcentaje de ganancia.
   - **Salir**: Termina la ejecución del programa.

   Con respecto al formato de archivos, se escogio el uso de archivos de formato `.txt` mayormente por la facilidad de manejo al ser archivos de texto plano, lo cual es suficiente para probar la funcionalidad del programa en la compresion de datos en diferentes archivo, el programa tambien funciona con archivos sin extensión.

## Detalles de Implementacion

### Comandos antes de iniciar ejecucion

Para cumplir el requerimiento de que el programa pueda ser ejecutado con el comando `./rarisimo`, se agregó un archivo `Makefile`, el cual contiene en la variable `SOURCES` los modulos a importar, y en `GHC_FLAGS` el uso de flags para incluir los paquetes `containers`, `directory`, y `filepath` y esto se utiliza para el manejo de archivos en `Cliente.hs`. Por lo tanto, antes de ejecutar el programa, debe ejecutarse `make rarisimo`

### Pruebas de los modulos

En la carpeta `pruebas` se incluyen pruebas especificas para los diferentes modulos, similar a el programa principal, cada prueba tiene su Makefile para facilitar el proceso de ejecucion y obtencion de resultados.

### Implementación de Cliente.hs
#### **Decodificación**
El programa permite procesar archivos `.raro` de manera eficiente, verificando la existencia del archivo y su validez antes de decodificarlo.  

#### **Análisis**
La funcionalidad de análisis calcula:
1. **Tamaño original**: Uso de `getFileSize` para obtener el tamaño en bytes.
2. **Tamaño comprimido**: Basado en el conteo de bits generados por el árbol.
3. **Ganancia porcentual**: Fórmula aplicada en la función auxiliar `calcularGanancia`:
 
    Ganancia = ((Tamaño original - Tamaño comprimido) / Tamaño original) x 100 

## **Ejemplo de Ejecución**

Digamos que tenemos en el repositorio raíz un archivo `ejemplo` (sin extension), con el siguiente contenido:

> Hola. Esta es la prueba del proyecto de Haskell de Lenguajes.

1. Ejecutar `make rarisimo` para importar todos los modulos junto con sus dependencias
2. Utilizar el comando `./rarisimo` para iniciar la ejecucion:
   1. Continuamente se le solicitará al usuario que escoja una de las 4 opciones. La opcion se elige con el numero asociado (1 para Codificar, 2 para Decodificar, 3 para Analizar, 4 para Salir)
   2. Al seleccionar 1, se le pedirá la ruta de un archivo a codificar, como `ejemplo`, al dar la ruta del archivo, `./ejemplo`, se generará en el directorio raiz el archivo `ejemplo.raro` con la codificación y representación del árbol de Hoffman, y se informará el éxito de dicha acción.
   3. Luego, al seleccionar 2, se le pedirá la ruta de un archivo de extension `.raro` a decodificar. Podemos usar `./ejemplo.raro` creado en el paso anterior, entonces se creará (o sobreescribirá) el archivo `ejemplo` resultado de decodificar, y se informará del éxito de esta acción.
   4. Al seleccionar 3, se le pedira la ruta de un archivo no codificado, como `ejemplo`, y se dara la informacion sobre tamaño original, tamaño codificado y ganancia (porcentaje).
   5. Al seleccionar 4, se termina el programa.

### Imágenes de referencia:
![image](https://github.com/user-attachments/assets/7c3bb1df-d851-4e0f-87e1-ab223eb2d39d)

![image](https://github.com/user-attachments/assets/8327575a-9cdb-4387-bd25-eb28a02084d1)

![image](https://github.com/user-attachments/assets/d5446c8b-d30f-417c-90df-aeebad31e7e3)

![image](https://github.com/user-attachments/assets/c42d459f-6612-486b-b2f6-993f283cc468)

