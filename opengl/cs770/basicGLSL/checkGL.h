//////////////////////////////////////////////////////////////////////////////
//
//  --- CheckError.h ---
//
// From Angel demos
//       
//
// 11/07/14 rdb: Angel version always prints the return code.
//     Changed _CheckError to add 3rd arg: false => don't print no-error case.
//     Added printGL: same as Angel's CheckError
//           checkGL: only prints when an error occurs
//////////////////////////////////////////////////////////////////////////////

#ifndef __CHECKERROR_H__
#define __CHECKERROR_H__

#include <stdio.h>
#if defined( __APPLE__ ) 
    #include <GLUT/glut.h>    // this includes gl.h
#else
    #include <GL/gl.h>
#endif

//----------------------------------------------------------------------------

#define Case( Token )  case Token: msg = #Token; break;

static const char*
errorString( GLenum error )
{
    const char*  msg;
    switch( error ) 
    {
	    Case( GL_NO_ERROR );
	    Case( GL_INVALID_VALUE );
	    Case( GL_INVALID_ENUM );
	    Case( GL_INVALID_OPERATION );
	    Case( GL_STACK_OVERFLOW );
	    Case( GL_STACK_UNDERFLOW );
	    Case( GL_OUT_OF_MEMORY );
    }

    return msg;
}

#undef Case	
//----------------------------------------------------------------------------

/**
 * rdb: 11/07/14 
 *    return the first return code, so caller can know something went wrong.
 */
static int _CheckError( const char* file, int line, bool printAll )
{
    GLenum  error = glGetError();
    GLenum  firstCode = error;

    while ( error != GL_NO_ERROR )
    {
	    fprintf( stderr, "[%s:%d] %s\n", file, line, errorString(error) );
        error = glGetError();
    }

    if ( printAll )
	    fprintf( stderr, "[%s:%d] %s\n", file, line, errorString(error) );
    return firstCode;
}

//----------------------------------------------------------------------------

#define CheckError()  _CheckError( __FILE__, __LINE__, true )

#define checkGL()  _CheckError( __FILE__, __LINE__, false )
#define printGL()  _CheckError( __FILE__, __LINE__, true )

//----------------------------------------------------------------------------

#endif // !__CHECKERROR_H__
