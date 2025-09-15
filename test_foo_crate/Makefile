CC = gcc
CFLAGS = -Wall -Wextra -pthread -g -O0
TARGET = test_foo
SOURCE = test_foo.c

$(TARGET): $(SOURCE)
	$(CC) $(CFLAGS) -o $(TARGET) $(SOURCE)

clean:
	rm -f $(TARGET)

run: $(TARGET)
	./$(TARGET)

.PHONY: clean run