# Variables
SRC_DIR = ../../
TEST_SRC = testHoffman.hs
TARGET = testHoffman
GHC = ghc
GHC_FLAGS = -package containers

# Reglas
all: $(TARGET)

$(TARGET): $(TEST_SRC)
	$(GHC) $(GHC_FLAGS) -o $(TARGET) -i$(SRC_DIR) $(TEST_SRC) $(SRC_DIR)/Hoffman.hs

run: $(TARGET)
	./$(TARGET)


clean:
	rm -f $(TARGET) *.hi *.o
