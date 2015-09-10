/**
 * Triangle.cpp - a class implementation representing a triangle object
 *           in OpenGL
 * 
 * rdb
 * October 2014: derived from C++ basicOpenGL demo to use glsl
 * August 2015: revised to pure OpenGL 3.2 or higher.
 *              should probabaly replace float arrays with vec2
 *              arrays. Has hard coded viewing specification.
 *              Perhaps should adopt GLM.
 */
#include "gl770.h"
#include "Triangle.h"

extern GLuint shaderProgram;

//------------- constructor -----------------------
Triangle::Triangle()
{
    bufferId = 0;
    float dxDefault[] = { -0.25, 0.25, 0.0 };
    float dyDefault[] = { 0.0, 0.0, 0.5 };

    setLocation( 0, 0 );
    setColor( 1, 1, 0 );   // Yellow is default color

    nVerts = 3;
    verts = new float[ nVerts * 2 ]; // allocate buffer array
    
    for ( int i = 0; i < nVerts; i++ )
    {
        //relPoints[ i ] = point2( dxDefault[ i ], dyDefault[ i ] );
        verts[ 2*i ] = dxDefault[ i ];
        verts[ 2*i + 1 ] = dyDefault[ i ];
    }
    makeBuffers();

    // define a uniform variable for the model matrix, "model"
    unif_model = glGetUniformLocation( shaderProgram, "model" ); checkGL();
    // For now we'll color entire polygon the same using a uniform variable
    unif_vColor = glGetUniformLocation( shaderProgram, "vColor" ); checkGL();
    std::cerr << "Triangle buf: " << bufferId << "\n";
}

//------------- destructor -----------------------
Triangle::~Triangle()
{
}

//------------------ public methods -------------------------------

//------------------------------ makeBuffers -----------------------
/**
 *  create VertaxArrayObject and VertexBufferObject
 */
void Triangle::makeBuffers()
{
    // ---- set up to transfer points to gpu
    // 1. Create a vertex array object
    GLuint vao[ 1 ];
    glGenVertexArrays( 1, vao );
    vertexArrayId = vao[ 0 ];
    glBindVertexArray( vertexArrayId );  // binding => this VAO is "current" one
    
    // 2. Create a vertex buffer
    GLuint bufferIds[ 1 ];        // we need just 1 buffer.
    glGenBuffers( 1, bufferIds ); checkGL();
    
    bufferId = bufferIds[ 0 ];    // get its id
    // make it the current buffer
    glBindBuffer( GL_ARRAY_BUFFER, bufferId ); checkGL();

    int bufbytes = sizeof( float ) * nVerts * 2; // 2 floats ver vertex
    glBufferData( GL_ARRAY_BUFFER, bufbytes, verts, GL_STATIC_DRAW ); checkGL();

    // debug: test if correct data is in the buffer
    /************
    float buff[] = { -90, -91, -92, -93, -94, -95 };
    std::cerr << nVerts << "  " << sizeof( float ) << " " << sizeof( buff ) << "\n";
    glGetBufferSubData( GL_ARRAY_BUFFER, 0, nVerts * sizeof( float ) * 2, &buff );
    checkGL();
    for ( int i = 0; i < 6; i++ ) 
        std::cerr << buff[ i ] << ", ";
    std::cerr << "++++++++ bufId: " << bufferId << "\n";
    **************/

    // define a uniform variable, "vPosition"
    GLuint attrLoc_vpos = glGetAttribLocation( shaderProgram, "vPosition" ); checkGL();

    glEnableVertexAttribArray( attrLoc_vpos ); checkGL();

    glVertexAttribPointer( attrLoc_vpos, 2, GL_FLOAT, GL_FALSE, 0, 
                           BUFFER_OFFSET( 0 ) );   checkGL();

    glBindVertexArray( 0 );               // unbind the VAO
    glBindBuffer( GL_ARRAY_BUFFER, 0 );   // unbind the VBO
}
//------------- redraw ---------------------------
void Triangle::redraw()
{
    checkGL();

    // simple modeling: only size and location. 
    //   we can write down the desired matrix.
    GLfloat model[ 16 ]={ xSize,   0,   0, 0,  
                           0,    ySize, 0, 0,  
                           0,      0,   1, 0,  
                          loc.x, loc.y, 0, 1 };

    //--- now push the matrix into a uniform var in vertex shader
    glUniformMatrix4fv( unif_model, 1, false, model ); checkGL();
    
    // there is 1 array/struct
    glUniform4fv( unif_vColor, 1, color ); checkGL();
    
    std::cerr<< "<Triangle.redraw() ::::::: " << bufferId << " :::::\n";

    //---------- checking the data in the buffer ------------
    /*************
    glBindBuffer( GL_ARRAY_BUFFER, bufferId );
    float buff[] = { -90, -91, -92, -93, -94, -95 };
    int nBytes = nVerts * sizeof( float ) * 2;
    glGetBufferSubData( GL_ARRAY_BUFFER, 0, nBytes, &buff );
    checkGL();
    for ( int i = 0; i < nVerts * 2; i++ ) 
        std::cerr << buff[ i ] << ", ";
    std::cerr << "---------\n";
    *******************/
    //----------------------
    
    //------ redraw the triangle 
    glBindVertexArray( vertexArrayId );
    glDrawArrays( GL_TRIANGLES, 0, nVerts ); checkGL();
        
    // unbind vao
    glBindVertexArray( 0 );
}
