#include <stdio.h>
#include <stdlib.h>

#define MAX_RAM 4096

int main(int argc, char *argv[]) {

    int *ram = malloc(MAX_RAM * sizeof(*ram));

    int numItemsScanned = 0;
    while (numItemsScanned < MAX_RAM && scanf("%d", &ram[numItemsScanned]) != EOF) {
        numItemsScanned++;
    }

    int pc = 0;
    int count = 0;

    while (pc >= 0 && pc < numItemsScanned) {
        int instruction = ram[pc];
        // printf("PC: %d, Inst: %d, Count: %d\n", pc, instruction, count);
        if (instruction >= 3) {
            ram[pc]--;
        } else {
            ram[pc]++;
        }
        pc += instruction;
        count++;
    }

    printf("%d\n", count);

    return 0;
}