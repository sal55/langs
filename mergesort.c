// (Based on code by Mayank Tyagi)
#include <stdio.h>
#include <stdlib.h>

#define THRESHOLD 1000

#define merge mergevla
//#define merge mergefixed

void mergeheap(int arr[], int l, int m, int r);

void mergevla(int arr[], int l, int m, int r)
{
    int n1 = m - l + 1;
    int n2 = r - m;

    if (n1>=THRESHOLD || n2>=THRESHOLD) {
        mergeheap(arr,l,m,r);
        return;
    }

    int L[n1], R[n2];

    for (int i = 0; i < n1; i++)
        L[i] = arr[l + i];
    for (int j = 0; j < n2; j++)
        R[j] = arr[m + 1 + j];

    int i = 0;
    int j = 0;
    int k = l;

    while (i < n1 && j < n2) {
        if (L[i] <= R[j]) {
            arr[k] = L[i];
            i++;
        }
        else {
            arr[k] = R[j];
            j++;
        }
        k++;
    }

    while (i < n1) {
        arr[k] = L[i];
        i++;
        k++;
    }

    while (j < n2) {
        arr[k] = R[j];
        j++;
        k++;
    }
}

void mergefixed(int arr[], int l, int m, int r)
{
    int n1 = m - l + 1;
    int n2 = r - m;

    if (n1>=THRESHOLD || n2>=THRESHOLD) {
        mergeheap(arr,l,m,r);
        return;
    }

    int L[THRESHOLD], R[THRESHOLD];

    for (int i = 0; i < n1; i++)
        L[i] = arr[l + i];
    for (int j = 0; j < n2; j++)
        R[j] = arr[m + 1 + j];

    int i = 0;
    int j = 0;
    int k = l;

    while (i < n1 && j < n2) {
        if (L[i] <= R[j]) {
            arr[k] = L[i];
            i++;
        }
        else {
            arr[k] = R[j];
            j++;
        }
        k++;
    }

    while (i < n1) {
        arr[k] = L[i];
        i++;
        k++;
    }

    while (j < n2) {
        arr[k] = R[j];
        j++;
        k++;
    }
}

int* makeheaparray(int n) {
    return malloc(n*sizeof(int));
}

void mergeheap(int arr[], int l, int m, int r)
{
    int n1 = m - l + 1;
    int n2 = r - m;

//  int L[n1], R[n2];
    int* L=makeheaparray(n1);
    int* R=makeheaparray(n2);

    for (int i = 0; i < n1; i++)
        L[i] = arr[l + i];
    for (int j = 0; j < n2; j++)
        R[j] = arr[m + 1 + j];

    int i = 0;
    int j = 0;
    int k = l;

    while (i < n1 && j < n2) {
        if (L[i] <= R[j]) {
            arr[k] = L[i];
            i++;
        }
        else {
            arr[k] = R[j];
            j++;
        }
        k++;
    }

    while (i < n1) {
        arr[k] = L[i];
        i++;
        k++;
    }

    while (j < n2) {
        arr[k] = R[j];
        j++;
        k++;
    }

    free(L);
    free(R);

}

void mergeSort(int arr[],int l,int r){
    if(l>=r){
        return;
    }
    int m =l+ (r-l)/2;
    mergeSort(arr,l,m);
    mergeSort(arr,m+1,r);
    merge(arr,l,m,r);
}

void printArray(int A[], int size)
{
    for (int i = 0; i < size; i++) {
        if (i<10 || i>(size-10))
            printf("%d ",A[i]);
    }
}

int main(void)
{
    int n = 10000000;
    int* arr = malloc(n*sizeof(int));

    for (int i=0; i<n; ++i) arr[i]=rand();

    puts("Given array is ");
    printArray(arr, n);
    puts("");

    mergeSort(arr, 0, n - 1);

    puts("Sorted array is ");
    printArray(arr, n);
    puts("");
    return 0;
}
