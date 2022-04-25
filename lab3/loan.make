listex: client.o loan.o
	gcc -o listex client.o loan.o

client.o: client.c loan.h
	gcc -c client.c

loan.o: loan.c loan.h
	gcc -c loan.c
