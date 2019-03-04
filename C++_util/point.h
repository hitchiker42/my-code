#ifndef __POINT_H__
#define __POINT_H__
namespace util {
/*
  For N dimensional points (i.e vectors) there are much better
  alternatives, but for the common case of a 3 dimensional point
  we get some advantage in ease of use/performance by writing our own.
*/
template<typename T>
struct Point3D {
  T x;
  T y;
  T z;
  constexpr Point3D()
    : x{0}, y{0}, z{0} {};
  constexpr Point3D(T x, T y, T z)
    : x{x}, y{y}, z{z} {}
  constexpr Point3D(const Point3D &other)
    : x{other.x}, y{other.y}, z{other.z} {}
  friend Point3D& operator+=(Point3D &pt1, Point3D pt2){
    pt1.x += pt2.x; pt1.y += pt2.y; pt1.z += pt2.z; return pt1;
  }  
  friend Point3D operator+(Point3D pt1, Point3D pt2){
    return pt1 += pt2;
  }
  friend Point3D operator-(Point3D pt){
    return {-pt.x,-pt.y, -pt.z};
  }
  friend Point3D& operator-=(Point3D &pt1, Point3D pt2){
    pt1.x -= pt2.x; pt1.y -= pt2.y; pt1.z -= pt2.z; return pt1;
  }  
  friend Point3D operator-(Point3D pt1, Point3D pt2){
    return pt1 -= pt2;
  }
  friend Point3D& operator*=(Point3D& pt, T scalar){
    pt.x *= scalar; pt.y *= scalar; pt.z *= scalar; return pt;
  }
  friend Point3D operator*(Point3D pt, T scalar){
    return pt *= scalar;
  }    
  friend Point3D operator*(int scalar, Point3D pt){
    return pt *= scalar;
  }  
  friend bool operator<(Point3D pt1, Point3D pt2){
    return (pt1.x == pt2.x ?
            (pt1.y == pt2.y ?
             pt1.z < pt2.z : pt1.y < pt2.y) : pt1.x < pt2.x);
  }
  friend bool operator==(Point3D pt1, Point3D pt2){
    return((pt1.x == pt2.x) && (pt1.y == pt2.y) && pt1.z == pt2.z);
  }
  friend bool operator!=(Point3D pt1, Point3D pt2){
    return !(pt1 == pt2);
  }
  friend bool is_is_valid_index(Point3D pt, int m, int n){
    return (pt.x>=0 && pt.y>=0 && pt.x < m && pt.y < n);
  }
  //Support for structured binding
  template<size_t N>
  constexpr int get() const {
    if constexpr(N == 0){ return i;} else { return j; }
  }
};
#endif /* __POINT_H__ */
