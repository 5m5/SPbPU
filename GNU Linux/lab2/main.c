#include <stdio.h>
#include <stdlib.h>
#include "node.c"

int main(int argc, char **argv)
{
    node_ptr_t L = NULL;
    list_init(&L);
    list_add(L, mk(55));
    list_add(L, mk(4));
    list_add(L, mk(100));
    list_add(L, mk(5));
    printf("\nThe number of index list L: %d\n\n", list_get(L, 0));
    printf("\nElement is found in the list %d times\n\n", list_count(L, 100));

    printf("List L:\n");
    list_print(L);

    reverse(L);
    printf("\nReversed list L:\n");
    list_print(L);

    printf("\nList L length: %d\n",list_length(L));
    printf("\n");

    node_ptr_t P = NULL;
    list_init(&P);
    list_add(P, mk(2));
    list_add(P, mk(7));
    list_add(P, mk(9));
    list_add(P, mk(6));
    printf("List P:\n");
    list_print(P);

    list_merge(L, P);
    printf("\nMerged list:\n");
    list_print(L);

    list_destroy(P);
    list_destroy(L);
    printf("\n");
    return 0;
    system("pause");
}
