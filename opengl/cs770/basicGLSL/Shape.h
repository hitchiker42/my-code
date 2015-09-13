/**
 * Shape.h - an abstract class representing an OpenGL graphical object 
 * 
 * rdb
 * August 17, 2012
 * ----------------------------------------------
 * 09/05/12 - added getX and getY methods
 */
#ifndef SHAPE_H_
#define SHAPE_H_

#include "gl770.h"

#define point2 vec2

class Shape
{
public:
   Shape();
   virtual ~Shape();
   
   virtual void setLocation( float x, float y ); 
                                         // set the location/origin of object
   
   void setSize( float xs, float ys );   // set the size of the object
   float getX();                         // return x location
   float getY();                         // return y location
   point2 getLocation();
   
   void setColor( float r, float g, float b, float a = 1 );   // set color
   
   virtual void redraw() = 0;
   
protected: 
   point2  loc;
   GLuint  vertexArrayId;    // id of vertexarray in gpu
   GLuint  bufferId;
   GLuint  vPositionId;
   
   float   color[ 4 ];      // main color for the shape
   float   xSize, ySize;    // size of the object
   float*  verts;           // vertex array to be stored in buffer
   int     nVerts;          // # vertexes in the array.

};

#endif /*SHAPE_H_*/

