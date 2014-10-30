#include "graph.h"
#define ARR_SWAP(arr,i,j)                                               \
  __extension__ ({__typeof__(arr[i]) __temp = arr[i];                   \
  arr[i]=arr[j];                                                        \
  arr[j]=__temp;})
/* Its a shame C doesn't have nested functions, because most of the
   parameters to the partiton function are in the containing scope
   of the qsort function where it's called
*/
typedef unsigned long ulong;
static int qsort_partiton(graph_edge *edges, ulong left,
                          ulong right, ulong pivot_ind){
  double pivot = arr[pivot_ind].weight;
  arr[pivot_ind] = arr[right];
  uint i;
  ulong index = pivot_ind;
  for(i=left;i<right-1;i++){
    if(arr[i].weight < pivot){
      ARR_SWAP(edges,i,index);
      index++;
    }
  }
}
long int random(void);
//call this at somepoint srandom(time(NULL));
static void qsort_inplace(graph_edge *arr, ulong left, ulong right){
  if(left < right){
    ulong pivot = (random() % right) + left;
    pivot = qsort_partiton(arr, left, right, pivot);
    //the next two lines should be dowe in parallel
    qsort_inplace(arr, left, pivot-1,comp);
    qsort_inplace(arr, pivot +1, right ,comp);
  }
}
void edge_qsort(graph_edge *edges, size_t len){
  qsort_inplace(edges,0, len);
}
