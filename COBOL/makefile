CC = cobc
FLAGS = -free -x

helloworld: helloWorld.cbl
	$(CC) $(FLAGS) helloWorld.cbl

gle: gle.cbl
	$(CC) $(FLAGS) gle.cbl

clean:
	ls | grep -v ".cbl" | grep -v "makefile" | xargs rm
