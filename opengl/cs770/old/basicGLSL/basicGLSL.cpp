/**
 * CS 770/870 GLSL cpp basic
 *
 * This shaderProgram uses very simple shaders.
 *
 * Derived from the basic OpenGL demo1 which was derived from 
 *           Chapter 2 examples of Hill & Kelley textbook
 *
 * @author rdb
 * @date   October 2013
 * 
 *  6 Nov 2013: build shaders in main shaderProgram rather than
 *              in Triangle.cpp
 *
 *  7 Nov 2014: Added call to glew for Mac OS X
 */


// define std:vector to be included
#include <vector>
#include <string>

#include "gl770.h"

#include "Triangle.h"

bool useVBO = true;
GLuint shaderProgram;

// A vector of all the shapes we will create
std::vector<Shape*>  shapes;

// window size parameters
int windowWidth  = 600;
int windowHeight = 640;

//----- forward method declarations
void openWindow();
void buildShaders();
void setupView();
void makeScene();
void redraw();
void keyboardHandler( unsigned char key, int x, int y );

//--------------------- constructor ---------------------

//----------------------< init --------------------
void init( void )
{
    openWindow();

    glutDisplayFunc( redraw );                    // register redraw function
    checkGL();
    glClearColor( 0, 0, 0, 1 );  // set the bg color to black
    checkGL();
    buildShaders();
    checkGL();
    setupView(); 
    makeScene();
    
    glutKeyboardFunc( keyboardHandler );
    glutMainLoop();                               // go into a perpetual loop
}

//-------------------- openWindow --------------------
/**
 * Attempt to figure out what we have out there. Problem is that
 * you cannot ask the support level until you open a window, but
 * you can't open a 3.2 window without using the
 * GLUT_3_2_CORE_PROFILE constant, which you can't use if it is
 * not defined by the GLUT installation.
 *
 * Could add a parameter to specified desired OpenGL version.
 */
void openWindow()
{
    //this should fail if OpenGL3.3 isn't supported
    glutInitContextVersion( 3,3 );
    glutInitWindowSize( windowWidth, windowHeight );
    glutInitWindowPosition( 100, 150 );           // window pos on screen
    glutInitDisplayMode( GLUT_SINGLE | GLUT_RGB);// | // GLUT_DEPTH |
    //                         GLUT_3_2_CORE_PROFILE ); 

    glutCreateWindow( "cppGLSL demo" );           // open the screen window
    checkGL();

    // Must be done after create window
    glewExperimental = GL_TRUE;  // Add ARB extensions always
    int glewReturn = glewInit(); 
    checkGL();                   // returns an error message
    if ( glewReturn != GLEW_OK )
        std::cerr << "glewInit returned: " << glewReturn << "\n";

    char* oglVersion =(char *)glGetString( GL_VERSION );
    checkGL();
    std::cerr << "OpenGL: " << oglVersion << "\n";

    const GLubyte* shVersion = glGetString( GL_SHADING_LANGUAGE_VERSION );
    if ( !checkGL() )
        std::cerr << "Shader # " << shVersion << "\n";
    else
        std::cerr << "Shader version query not supported? \n";

    if ( atof( oglVersion ) < 1.3 )  // 130 == 1.3 == 3.0 == 330
    {
        // could have a global parameter and execute different // code???
        std::cerr << "--- Program uses v3.0 (130 or 330) shaders.  This is "
                  << oglVersion << "\n";
        exit( 1 );
    }
}    
//--------------------- makeScene -----------------------------------
/**
 * Generate the objects in the scene
 */
void makeScene()
{
    // Create 3 triangle objects, with different locations/sizes/colors
    //    add them to the shapes array.
    Triangle* tri = new Triangle();
    tri->setLocation( 0, 0 );
    tri->setColor( 0, 1, 1 );    // cyan
    shapes.push_back( tri );

    tri = new Triangle();
    tri->setLocation( -0.5, -0.25 );
    tri->setColor( 1, 0, 1 );    // magenta
    tri->setSize( 0.7, 2.0 );
    shapes.push_back( tri );
    
    tri = new Triangle();
    tri->setLocation( 0.3, -0.2 );  // yellow by default
    tri->setSize( 2.0, 0.7 );
    shapes.push_back( tri );
}

//------------------ setupView --------------------------
/**
 * We have a constant viewing and projection specification.
 *   Can define it once and send the spec to the shader.
 */
void setupView()
{
    // equivalent to glOrtho2D( -2, 2, -2, 2 )
    GLfloat pXv[16] = { 0.5, 0,  0, 0,  
                         0, 0.5, 0, 0,  
                         0,  0,  1, 0,  
                         0,  0,  0, 1 };
    /**********
    // default: equivalent to glOrtho2D( -1, 1, -1, 1 )
    GLfloat pXv[16] = { 1,  0, 0, 0,  
                        0,  1, 0, 0,  
                        0,  0, 1, 0,  
                        0,  0, 0, 1 };
    **************/
    std::cerr << "projXview: ";
    for ( int i = 0; i < 16; i++ ) 
        std::cerr << pXv[ i ] << ", ";
    std::cerr << "\n";
    /******************************/
    
    //--- now push the composite into a uniform var in vertex shader
    GLint matLoc = glGetUniformLocation( shaderProgram, "projXview" );
    glUniformMatrix4fv( matLoc, 1, false, pXv );
    checkGL();
}
//------------------ buildShaders --------------------------
/**
 * Build the shaders only once, rather than on every Triangle call.
 */
void buildShaders()
{
    std::cerr << "buildShaders " << std::endl;
    
    // Load shaders and use the resulting shader shaderProgram
    shaderProgram = makeShaders( "transform.vsh", "flat.fsh" );
    
    // If using vColor attached to a vertex, we'd do something like:
    //GLuint col = glGetAttribLocation( shaderProgram, "vColor" );
    //glEnableVertexAttribArray( col );
    //glVertexAttribPointer( col, 2, GL_FLOAT, GL_FALSE, 0, BUFFER_OFFSET(0) );
    
    glUseProgram( shaderProgram );
    checkGL();
}
//------------------------ redraw -----------------
// the redraw function
void redraw( void )
{
	glClear( GL_COLOR_BUFFER_BIT );  // clear buffers
    checkGL();
    
    // Now "redraw" all the objects in the scene.
    // create a vector iterator to access the Shapes in the "shapes" vector
    std::vector<Shape*>::iterator it;
    for ( it = shapes.begin(); it < shapes.end(); it++ )
        (*it)->redraw();
    
    glFlush();                     // send all output to display
}
//--------------------- keyboardHandler ------------------
/**
 * respond to keyboard events; all this one does is quit on Esc or qQ
 */
void keyboardHandler( unsigned char key, int x, int y )
{
    switch ( key )
    {
        case 033:   // escape key
        case 'q':
        case 'Q':
            exit ( EXIT_SUCCESS );
            break;
    }
}
//------------------------ main ----------------------
int main( int argc, char **argv )
{    
    glutInit( &argc, argv );                       // initialize toolkit
    
    init();
}
