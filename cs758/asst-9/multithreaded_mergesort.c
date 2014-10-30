/*
  Taken from ch 27 of clrs
*/
#include "graph.h"
//we're sorting edges

int binary_search(double weight, graph_edge *edges, int start, int end){
  if (weight < edges[start].weight || end <= start){
    return start;
  }
  while(start < end){
    mid = (start+end)/2;
    if(weight<=edges[mid].weight){
      end = mid;
    } else {
      start = mid+1;
    }
  }
  return start;
}
