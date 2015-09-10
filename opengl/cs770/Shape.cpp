/**
 * Shape.cpp - an abstract class representing an OpenGL graphical object 
 * 
 * rdb
 * August 17, 2008
 * ----------------------------------------------
 * 09/05/12 - added intialization of color in constructor
 *            added getX and getY methods
 */
#include "Shape.h"
//------------------ Constructors ------------------------------------
/**
 * Create a new shape at position 0,0 of size 1,1
 */
Shape::Shape()
{
   setColor( 1, 1, 0 ); // default color is yellow
   setLocation( 0, 0 );
   setSize( 1, 1 );
}

//------------------ Destructor ------------------------------------
Shape::~Shape()
{
}

//------------------ public methods -------------------------------
/**
 * set the location of the object to the x,y position defined by the args
 */
void Shape::setLocation( float x, float y )
{
   loc = point2( x, y );
}
/**
 * return the value of the x origin of the shape
 */
float Shape::getX()
{
   return loc.x;
}
/**
 * return the value of the y origin of the shape
 */
float Shape::getY()
{
   return loc.y;
}
/**
 * return the origin of the shape
 */
point2 Shape::getLocation()
{
   return loc;
}
/**
 * set the "nominal" color of the object to the specified color; this
 *   does not require that ALL components of the object must be the same
 *   color. Typically, the largest component will take on this color,
 *   but the decision is made by the child class.
 */  
void Shape::setColor( float r, float g, float b, float a )
{
   color[0] = r;  color[1] = g; color[2] = b; color[3] = a;
}
 
/** 
 * set the size of the shape to be scaled by xs, ys
 *    That is, the shape has an internal fixed size, the shape parameters
 *    scale that internal size.
 */  
void Shape::setSize( float xs, float ys )
{
   xSize = xs;
   ySize = ys;
}
