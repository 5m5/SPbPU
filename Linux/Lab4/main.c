#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define N 1000

int main() {
  int **A = (int**)malloc(N * sizeof(int*));
  int **B = (int**)malloc(N * sizeof(int*));
  int **C = (int**)malloc(N * sizeof(int*));

  int i, j, k;

  for(i = 0; i < N; i++) {
    A[i] = (int*)malloc(N * sizeof(int));
    B[i] = (int*)malloc(N * sizeof(int));
    C[i] = (int*)malloc(N * sizeof(int));
  }

  srand(time(NULL));
  for(i = 0; i < N; i++)
    for (j = 0; j < N; j++) {
      A[i][j] = rand() % 10;
      B[i][j] = rand() % 10;
    }

  for(i = 0; i < N; i++)
  for(j = 0; j < N; j++) {
    C[i][j] = 0;
    for(k = 0; k < N; k++)
      C[i][j] += A[i][k] * B[k][j];
    }

  // printf("\nmatrix A\n");
  // for(i = 0; i < N; i++) {
  //   for(j = 0; j < N; j++)
  //     printf("%d ", A[i][j]);
  //   printf("\n");
  // }

  // printf("\nmatrix B\n");
  // for(i = 0; i < N; i++) {
  //   for(j = 0; j < N; j++)
  //     printf("%d ", B[i][j]);
  //   printf("\n");
  // }

  // printf("\nthe result of multiplying\n");
  // for(i = 0; i < N; i++) {
  //   for(j = 0; j < N; j++)
  //     printf("%3d ", C[i][j]);
  //   printf("\n");
  // }

  for(i = 0; i < N; i++) {
    free(A[i]);
    free(B[i]);
    free(C[i]);
  }

  free(A);
  free(B);
  free(C);

  return 0;
}
