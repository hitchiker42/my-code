#include "graph.h"
long int random(void);
#define ARR_SWAP(arr,i,j)                                               \
  __extension__ ({__typeof__(arr[i]) __temp = arr[i];                   \
  arr[i]=arr[j];                                                        \
  arr[j]=__temp;})
#define SWAP(i,j)                                               \
  __extension__ ({__typeof__(i) __temp = i;                     \
      i=j;                                                      \
      j=__temp;})
/* Its a shame C doesn't have nested functions, because most of the
   parameters to the partiton function are in the containing scope
   of the qsort function where it's called
*/
typedef unsigned long ulong;
static void insertion_sort_edges(graph_edge *edges, size_t len){
  int i,j;
  for(i=0;i<len;i++){
    graph_edge x = edges[i];
    for(j=i;j>0;j--){
      if(edges[j-i].weight < x.weight){
        edges[j] = edges[j-1];
      } else {
        break;
      }
    }
    edges[j]=x;
  }
}
static inline size_t select_pivot(graph_edge *edges, size_t left, size_t right){
  size_t mid = left + ((right - left)/2); //gcc will turn that into a shift
  if(edges[mid].weight < edges[left].weight){
    SWAP(mid,left);
  }
  if(edges[right].weight < edges[mid].weight){
    SWAP(mid,right);
    if(edges[mid].weight < edges[left].weight){
      SWAP(mid,left);
    }
  }
  return mid;
}
      
static size_t qsort_partiton(graph_edge *edges, ulong left,
                             ulong right){
  size_t pivot_ind = (random() % right) + left;
  graph_edge pivot_elem = edges[pivot_ind];
  double pivot = pivot_elem.weight;
  ARR_SWAP(edges,pivot_ind,right);
  unsigned int i;
  ulong index = pivot_ind;
  for(i=left;i<right;i++){
    if(edges[i].weight < pivot){
      ARR_SWAP(edges,i,index);
      index++;
    }
  }
  edges[right] = edges[index];
  edges[index] = pivot_elem;
  return index;
}
#define INSERTION_SORT_THRESHOLD 8
//call this at somepoint srandom(time(NULL));
static void qsort_inplace(graph_edge *arr, long left, long right){
  long len = right - left;
  if(len <= 0){
    return;
  }
  if(len < INSERTION_SORT_THRESHOLD){
    insertion_sort_edges(arr+left, len);
  } else {
    size_t pivot = qsort_partiton(arr, left, right-1);
//    fprintf(stderr,"sorting from %lu to %lu and pivoting around index %lu\n", left, right,pivot);
    //the next two lines should be dowe in parallel
    qsort_inplace(arr, left, pivot);
    qsort_inplace(arr, pivot, right);
  }
}
int compare_edges(const graph_edge *x, const graph_edge *y){
  return (x->weight > y->weight ? 1 : 0);
}
void edge_qsort(graph_edge *edges, size_t len){
  fprintf(stderr, "Sorting array of length %lu\n", len);
  qsort_inplace(edges, 0, len);
//  qsort(edges, len, sizeof(graph_edge), (__compar_fn_t)compare_edges);
}

