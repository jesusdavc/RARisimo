# Makefile para compilar el proyecto en Haskell

# Definir el nombre del ejecutable
EXEC = main

# Definir los archivos de fuente
SOURCES = Frecuencia.hs Hoffman.hs RARisimo.hs Cliente.hs

# Definir las opciones de compilación
GHC_FLAGS = -package containers -package directory

# Regla por defecto: compilar el ejecutable
all: $(EXEC)

# Regla para compilar el ejecutable
$(EXEC): $(SOURCES)
	ghc $(GHC_FLAGS) -o $(EXEC) $(SOURCES)

# Limpiar los archivos generados (archivos intermedios y el ejecutable)
clean:
	rm -f $(EXEC) *.hi *.o

# Regla para ejecutar el programa (opcional)
run: $(EXEC)
	./$(EXEC)

.PHONY: all clean run
