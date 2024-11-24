# Variables
SRC_DIR = ../../
TEST_SRC = testFrecuencia.hs
TARGET = testFrecuencia
GHC = ghc

# Reglas
all: $(TARGET)

$(TARGET): $(TEST_SRC)
	$(GHC) -o $(TARGET) -i$(SRC_DIR) $(TEST_SRC) $(SRC_DIR)/Frecuencia.hs

run: $(TARGET)
	./$(TARGET)


clean:
	rm -f $(TARGET) *.hi *.o
