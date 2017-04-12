#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <vector>
#include <unordered_map>
#include <utility>
#include <tuple>
#include <forward_list>
#include <list>
#include <queue>
using std::vector;
using std::pair;
using std::tuple;
template <typename T>
using list = std::forward_list<T>;
template <typename K, typename V>
using hashtable = std::unordered_map<K,V>;

template <typename T>
using queue = std::queue<T,std::list<T>>;
template<typename T>
void enqueue(queue<T> q, T val){
  q.push(val);
}
template<typename T>
T& dequeue(queue<T> q){
  T& ret = q.front();
  q.pop();
  return ret;
}
#define zmalloc(sz) calloc(sz,1)
#define malloc_type(T) (T*)malloc(sizeof(T))
#define alloc_type(T) (T*)zmalloc(sizeof(T))
#define self this
#ifdef static
#undef static
#endif
template <typename V, typename E = double>
struct Graph {
  //these 2 are the only actual fields
  hashtable<V,struct Vertex*> vertices;
  E default_edge_value;
  struct Vertex {
    V key;
    //this could be a hashtable, but the assumption is that there are relatively
    //few edges per vertex, otherwise we'd use an adjency matrix.
    list<pair<V,E>> edges;
    //Used for holding state for various algorithms
    int priv[4];
    Vertex *parent;//used in some algorithms
    static Vertex *make_vertex(V key){
      Vertex *vtx = alloc_type(Vertex);
      vtx->key = key;
      vtx->edges = list<pair<V,E>>();
      return vtx;
    }
    static void destroy_vertex(Vertex *vtx){
      vtx->edges.~list();
      free(vtx);
      return;
    }
    static void zero_priv_data(Vertex *vtx){
      memset(vtx->priv,'\0',sizeof(vtx->priv));
      vtx->parent = NULL;
    }
  };
  template<int idx> int& vertex_priv_ref(Vertex *v){return v->priv[idx];}
  Vertex*& vertex_parent_ref(Vertex *v){return v->parent;}

  static Graph *make_graph(E default_edge){
    Graph *g = alloc_type(Graph);
    g->vertices = hashtable<V,Vertex*>();
    g->default_edge_value = default_edge;
    return g;
  }
  static void destroy_graph(Graph *g){
    //destroy all the vertices, and as a slight optimization remove them from
    //the hashtable at the same time
    for(auto iter = g->vertices.begin(); iter != g->vertices.end(); iter = erase(iter)){
      Vertex *vtx = iter->second;
      Vertex::destroy_vertex(vtx);
    }
    g->vertices.~hashtable();

  }
  //Check existance of vertex/edge
  bool vertex_exists(V key){
    return vertices.count(key);
  }
  bool edge_exists(V src, V dst){
    if(!vertex_exists(src) || !vertex_exists(dst)){
      return false;
    }
    auto edges = vertices[src]->edges;
    return any_of(edges.begin(),edges.end(),
                  [&dst](pair<V,E> &edge){return edge.first == dst;});
  }
  //adding/deleting vertices/edges
  //return true on success false on error
  bool add_vertex(V key){//fails if vertex already exists
    if(vertex_exists(key)){
      return false;
    }
    Vertex *vtx = Vertex::make_vertex(key);
    vertices.insert({key,vtx});
    return true;
  }
  int remove_vertex(V key){//fails if vertex doesn't exist
    if(!vertex_exists(key)){
      return false;
    }
    auto elt = vertices.find(key);
    Vertex *vtx = elt->second;
    vertices.erase(elt);
    Vertex::destroy_vertex(vtx);
    return true;
  }

  bool add_edge(V src, V dst){
    return add_edge(src,dst,default_edge_value,true);
  }
  bool add_edge(V src, V dst, bool add_vertices){
    return add_edge(src,dst,default_edge_value,add_vertices);
  }
  bool add_edge(V src, V dst, E value, bool add_vertices){
    //Make sure the vertices exist/add them if add_vertices is true
    if(!vertex_exists(src) || !vertex_exists(dst)){
      if(add_vertices){
        add_vertex(src);
        add_vertex(dst);
      } else {
        return false;
      }
    }
    if(edge_exists(src,dst)){
      return false;
    }
    Vertex *vtx = vertices[src];
    vtx->edges.push_front({dst,value});
    return true;
  }
  bool remove_edge(V src, V dst){
    auto edges = vertices[src]->edges;
    for(auto iter = edges.before_begin(); (iter+1) != edges.end(); ++iter){
      auto edge = *(iter+1);
      if(dst == edge.first){
        edges.erase_after(iter);
        return true;
      }
    }
    return false;
  }
  //silently ignores errors
  void add_vertices(vector<V> keys){
    for(auto key : keys){
      add_vertex(key);
    }
  }
  void add_edges(vector<tuple<V,V,E>> edges){
    V src,dst;
    E value;
    for(auto edge : edges){
      std::tie(src,dst,value) = edge;
      add_edge(src,dst,value);
    }
  }
  //Get information about edges/vertices

  //Return pointers to NULL if the vertex/edge doesn't exist
  Vertex* get_vertex(V key){
    if(vertex_exists(key)){
      return vertices[key];
    } else {
      return NULL;
    }
  }
  E* get_edge_value(V src, V dst){
    if(!vertex_exists(src) || !vertex_exists(dst)){
      return NULL;
    }
    auto edges = vertices[src]->edges;
    for(auto iter = edges.begin(); iter != edges.end(); ++iter){
      auto edge = iter;
      if(edge->first == dst){
        return &(edge->second);
      }
    }
    return NULL;
  }
  //Algorithms
  typedef void(*visit_func)(V vertex);
  void bfs(V start, visit_func visit);  
  void dfs(visit_func visit);
  list<V> topological_sort();
  //In general this needs to be called before each algorithm
  void zero_vertex_priv_data(){
    for(auto v : vertices){
      Vertex::zero_priv_data(v->second);
    }
  }
private:

  void dfs_recur(Vertex *v, int& time, visit_func visit);
  int&(*bfs_dist)(Vertex *v) = vertex_priv_ref<0>;
  int&(*bfs_color)(Vertex *v) = vertex_priv_ref<1>;
  Vertex&(*bfs_parent)(Vertex *v) = vertex_parent_ref;
  void set_bfs_data(Vertex *v,int dist,
                    int color, Vertex* parent){
    bfs_dist(v) = dist;
    bfs_color(v) = color;
    bfs_parent(v) = parent;
  }

  int&(*dfs_start)(Vertex *v) = vertex_priv_ref<0>;
  int&(*dfs_end)(Vertex *v) = vertex_priv_ref<1>;
  int&(*dfs_color)(Vertex *v) = vertex_priv_ref<2>;
  Vertex&(*dfs_parent)(Vertex *v) = vertex_parent_ref;
};
template <typename V, typename E>
void Graph<V,E>::bfs(V start, visit_func visit){
  queue<Vertex*> q;
  Vertex *vtx = get_vertex(start);
  set_bfs_data(vtx, 0, 1, NULL);
  enqueue(q, get_vertex(start));
  while(!q.empty()){
    Vertex *v = dequeue(q);
    for(auto edge : vtx->edges){
      Vertex *u = edge.first;
      if(bfs_color(u) == 0){
        set_bfs_data(u,bfs_dist(v)+1,v);
        enqueue(q,u);
      }
    }
    visit(v->key);
    bfs_color(v) = 1;
  }
}
template <typename V, typename E>
void Graph<V,E>::dfs_recur(Vertex *v, int& time, visit_func visit){
  dfs_start(v) = ++time;
  dfs_color(v) = 1;
  for(auto edge : v->edges){
    auto u = edge.first;
    if(dfs_color(u) == 0){
      dfs_parent(u) = v;
      dfs_recur(u, time, visit);
    }
  }
  visit(v->key);
  dfs_end(v) = ++time;
}
    
template <typename V, typename E>
void Graph<V,E>::dfs(visit_func visit){
  int time = 0;
  int n_vertices = vertices.size();
  for(auto vv : vertices){
    Vertex *v = vv->second;
    if(dfs_color(v) == 0){
      dfs_recur(v, time, visit);
    }
    if(time >= n_vertices*2){
      break;
    }
  }
}
template <typename V, typename E>
list<V> Graph<V,E>::topological_sort(){
  list<V> ls;
  auto visit = [ls](V key){ls.push_front(key);};
  dfs(visit);
  return ls;
}
