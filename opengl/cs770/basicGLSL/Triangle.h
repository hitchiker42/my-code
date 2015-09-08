/**
 * Triangle.h - a class implementation representing a triangle object
 *           in OpenGL
 * 
 * rdb
 * August 26, 2012
 */

#ifndef TRIANGLE_H_
#define TRIANGLE_H_

#include "Shape.h"

class Triangle: public Shape
{
public:
    Triangle();
    virtual ~Triangle();
    
    virtual void redraw();
    
protected:
    void   makeBuffers();
    void   buildShaders();

    GLuint attrLoc_vpos;   // vao for vPosition
    GLuint unif_model;  // uniform var for model matrix
    GLuint unif_vColor;  // uniform var for vertex color
};

#endif /*TRIANGLE_H_*/
