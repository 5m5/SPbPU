#include <stdio.h>
#include <stdlib.h>

typedef struct node node_t;
typedef struct node* node_ptr_t;

struct node
{
    void *data;
    struct node *next;
};

#define list_for_each(pos, head) for(pos = head->next; pos != NULL; pos = pos->next)

void list_init(node_ptr_t *head)
{
    *head = (node_ptr_t)malloc(sizeof(node_t));
    if(*head == NULL)
    {
        fprintf(stderr, "Can't allocate memory\n");
        exit(1);
    }
    (*head)->next = NULL;
    (*head)->data = NULL;
}

void list_add(node_ptr_t head, void* data)
{
    node_ptr_t new_node = (node_ptr_t)malloc(sizeof(node_t));
    if(new_node == NULL)
    {
        fprintf(stderr, "Can't allocate memory\n");
        exit(1);
    }
    new_node->data = data;
    new_node->next = head->next;
    head->next = new_node;
}

void list_add_tail(node_ptr_t head, void* data)
{
    node_ptr_t new_node = (node_ptr_t)malloc(sizeof(node_t));
    if(new_node == NULL)
    {
        fprintf(stderr, "Can't allocate memory\n");
        exit(1);
    }
    new_node->data = data;
    new_node->next = NULL;
    node_ptr_t current = head->next;
    if(current == NULL)
        head->next = new_node;
    else
    {
        while(current->next != NULL)
            current = current->next;
        current->next = new_node;
    }
}

node_ptr_t list_del(node_ptr_t head)
{
    node_ptr_t temp = NULL;
    if(head->next != NULL)
    {
        temp = head->next;
        head->next = head->next->next;
    }
    return temp;
}

void list_del_node(node_ptr_t *node)
{
    free((*node)->data);
    free(*node);
}

node_ptr_t list_del_tail(node_ptr_t head)
{
    node_ptr_t temp = NULL;
    if(head->next == NULL)
        return NULL;
    else if(head->next->next == NULL)
    {
        temp = head->next;
        head->next = NULL;
    }
    else
    {
        node_ptr_t current = head->next;
        while(current->next->next != NULL)
            current = current->next;
        temp = current->next;
        current->next = NULL;
    }
    return temp;
}

size_t list_length(node_ptr_t head)
{
    node_ptr_t current;
    size_t length = 0;
    list_for_each(current, head)
        length++;
    return length;
}

void list_print(node_ptr_t head)
{
    node_ptr_t current;
    list_for_each(current, head)
        printf("%d\n",*((int*)current->data));
}


int* mk(int n)
{
    int *np = malloc(sizeof(int));
    *np = n;
    return np;
}

void reverse(node_ptr_t head)
{
    node_ptr_t prev = NULL;
    node_ptr_t current = head->next;
    node_ptr_t next;
    while(current != NULL)
    {
        next = current->next;
        current->next = prev;
        prev = current;
        current = next;
    }
    head->next = prev;
}

size_t list_count(node_ptr_t head, int e)
{
    int count = 0;
    node_ptr_t current;
    list_for_each(current, head)
    {
        if(e == *((int*)current->data))
            count++;
    }
    return count;
}

size_t list_get(node_ptr_t head, int index)
{
    if(index < 0 || index > list_length(head))
    {
        fprintf(stderr, "Index out of bounds\n");
        exit(1);
    }
        node_ptr_t current = head->next;
        int i;
        for(i = 0; i < index-1; i++)
            current = current->next;
        return *((int*)current->data);
}

void list_destroy(node_ptr_t head)
{
    node_ptr_t current;
    list_for_each(current, head)
    {
        free(current->data);
        free(current);
    }
}

void list_insert(node_ptr_t head, int index, int e)
{
    if(index < 0 || index > list_length(head))
    {
        fprintf(stderr, "Index out of bounds\n");
        exit(1);
    }
    if(index == 0)
        list_add(head, mk(e));
    else
    {
        node_ptr_t current = head->next;
        int i;
        for(i = 0; i < index-1; i++)
            current = current->next;
        list_add(current, mk(e));
    }
}

void list_merge(node_ptr_t head1, node_ptr_t head2) 
{
    node_ptr_t current;
    list_for_each(current, head2)
        list_add_tail(head1, ((int*)current->data));
}
