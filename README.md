# RARisimo
# Proyecto I: RARísimo
### Universidad Simón Bolívar  
**Departamento de Computación y Tecnología de la Información**  
**Curso**: CI-3661 – Laboratorio de Lenguajes de Programación  
**Trimestre**: Septiembre–Diciembre 2024  

---

## **Descripción del Proyecto**

El proyecto "RARísimo" consiste en desarrollar un sistema de compresión y descompresión de archivos utilizando algoritmos basados en árboles de Hoffman implementados en Haskell. Este sistema tiene como objetivo operar en un entorno limitado, mostrando el funcionamiento básico y emulando este sistema de compresión/descompresión.
---

## **Estructura**

### **1. Modulos Principales**

1. **`Frecuencia.hs`**  
   Este módulo define el tipo de datos `Frecuencia` para contar las ocurrencias de caracteres en una cadena. Incluye:
   - Funciones para inicializar y contar frecuencias.
   - Acceso al valor y la frecuencia.
   - Instancias de las clases `Show` y `Ord` para manejar comparaciones y representaciones.

2. **`Hoffman.hs`**  
   Contiene la implementación del tipo de datos `Hoffman` (árbol de Huffman) y las funciones asociadas:
   - Construcción: `nuevoHoffman` y `fusionHoffman`.
   - Acceso: `obtenerCaracter`, `arbolIzquierdo` y `arbolDerecho`.
   - Transformación: `codificacion`, para generar códigos binarios a partir del árbol.

3. **`RARisimo.hs`**  
   Implementa el algoritmo de construcción del árbol de Huffman basado en frecuencias, incluyendo:
   - `frecuencias`: Genera las frecuencias a partir de una cadena.
   - `ganadores`: Encuentra las dos frecuencias más pequeñas.
   - `hoffman`: Construye el árbol de Huffman.
   - `rarisimo`: Genera la codificación asociada a cada carácter.

4. **`Cliente.hs`**  
   Implementa la interfaz de usuario y el flujo principal del programa (`main`).  
   Incluye las siguientes funcionalidades:
   - **Codificar**: Convierte un archivo en uno comprimido con extensión `.raro`.
   - **Decodificar**: Restaura un archivo `.raro` a su estado original.
   - **Analizar**: Muestra el tamaño original, el tamaño comprimido y el porcentaje de ganancia.
   - **Salir**: Termina la ejecución del programa.

## Implementación de Cliente.hs
### **Decodificación**
El programa permite procesar archivos `.raro` de manera eficiente, verificando la existencia del archivo y su validez antes de decodificarlo.  

### **Análisis**
La funcionalidad de análisis calcula:
1. **Tamaño original**: Uso de `getFileSize` para obtener el tamaño en bytes.
2. **Tamaño comprimido**: Basado en el conteo de bits generados por el árbol.
3. **Ganancia porcentual**: Fórmula aplicada:  
   \[
   \text{Ganancia (\%)} = \frac{\text{Tamaño original} - \text{Tamaño comprimido}}{\text{Tamaño original}} \times 100
   \]

---

## **Ejemplo de Ejecución**

