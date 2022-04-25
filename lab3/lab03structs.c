// Team 19 - Yudong Lin & Wei Wei Chien
#include <stdio.h>
#include <string.h>

struct address_tag {
    int parcelNum;
    int lotSize;
    int houseNum;
    char street[30];
    char city[30];
    char state[3];
};

typedef struct address_tag Address;

struct person_tag {
    char firstName[20];
    char lastName[20];
    Address addr;
};

typedef struct person_tag Person;

void printAddress(Address *addr);

void printPerson(Person per);

void displayPeople(Person ppl[], int pplCount);

void addPerson(Person newPerson, Person ppl[], int *pplCount);

void moveToTacoma(Person *per);

void moveAllToTacoma(Person ppl[], int pplCount);

void changeLastName(Person *per, char newLastName[]);

int main(void) {

    Person people[10] = {
            {"Joseph", "Miller",  {0, 0, 10,   "Downing",          "London",   "WA"}},
            {"Anne",   "Simpson", {0, 0, 1600, "Pennsylvania Ave", "Columbia", "WA"}},
            {"Peter",  "Price",   {0, 0, 832,  "Main St.",         "Dallas",   "TX"}}
    };

    int peopleCount = 3;

    displayPeople(people, peopleCount);

    Person historicFigure = {"Thea", "Foss", {0, 0, 17, "Prospect St.", "Tacoma", "WA"}};
    addPerson(historicFigure, people, &peopleCount);
    displayPeople(people, peopleCount);


    moveAllToTacoma(people, peopleCount);

    printf("After massive relocation\n");  //  Now this works!
    displayPeople(people, peopleCount);

    // test changeLastName function
    printf("Before changing the last name:\n");
    printPerson(people[2]);
    printf("\n");
    changeLastName(&people[2], "Griffin");
    printf("After changing the last name:\n");
    printPerson(people[2]);

    return 0;

}


void printAddress(Address *addr) {
    printf("%d %d\n", addr->parcelNum, addr->lotSize);
    printf("%d %s\n", addr->houseNum, addr->street);
    printf("%s, %s\n", addr->city, addr->state);
}

void printPerson(Person per) {
    printf("%s, %s\n", per.lastName, per.firstName);
    printAddress(&(per.addr));
}


void displayPeople(Person ppl[], int pplCount) {
    int i;
    for (i = 0; i < pplCount; i++) {
        printPerson(ppl[i]);
        printf("\n");
    }
}

void addPerson(Person newPerson, Person ppl[], int *pplCount) {
    ppl[*pplCount] = newPerson;
    (*pplCount)++;
}


void moveToTacoma(Person *per) {
    strcpy(per->addr.city, "Tacoma");
    strcpy(per->addr.state, "WA");
}

void moveAllToTacoma(Person ppl[], int pplCount) {
    int i = 0;
    for (i = 0; i < pplCount; i++) {
        moveToTacoma(&ppl[i]);
    }
}

void changeLastName(Person *per, char newLastName[]) {
    strcpy(per->lastName, newLastName);
}
