/* 
 * makeShaders.cpp
 *     create a GLSL shader program from vertex and fragment shader file
 *     names.
 *
 * This is a combination of 
 *     -Edward Angel's initShaders.cpp (supporting his 6e graphics textbook)
 *     -Tutorial code from http://www.opengl.org/wiki
 *     -My edits that changed style, broke into more methods, and replaced
 *      open/seek to end code for finding file length with invocation of stat
 *
 * rdb: 10/24/13
 *      11/07/14 Cleaned up a bit more; it uses a modified version of Angel's
 *               CheckError macro and a slimmed down
 *               version of Angel's Angel.h. along with gl770.h
 */

#include <sys/stat.h>
#include "gl770.h"

    //----------------------- logError ------------------------
    // Report and log an error -- this version then quits, but it
    //    probably should allow caller to decide.
    //
    static void logError( std::string errMsg, int exitCode )
    {
        // Don't need to log or delete since exiting, but
        // probably should return an error code rather than exiting
        // and this code shows the better way to handle such failures:
        // which is to let the caller decide what to do.
        std::cerr << errMsg << std::endl;
        if ( exitCode )
        {
            exit( exitCode );
        }
    }
    //----------------------- readShaderSource ------------------------
    // Create a NULL-terminated string containing the entire contents of
    //   the provided file
    //
    static char* readShaderSource( const char* shaderFile )
    {
        struct stat filestatus;
        stat( shaderFile, &filestatus );
        
        long size = filestatus.st_size;
        FILE* fp = fopen( shaderFile, "r" );
        if ( fp == NULL )
            return NULL;
        
        char* buf = new char[ size + 1 ];
        
        fread( buf, 1, size, fp );
        
        buf[ size ] = '\0';
        fclose( fp );
        return buf;
    }
    //--------------------- makeShader ------------------------------
    /*
     * read a shader file and create a shader object.
     */
    GLuint makeShader( const char* fileName, GLenum sType )
    {
        GLchar* source = readShaderSource( fileName );
        checkGL();
        if ( source == NULL )
        {
            logError( std::string( "Open failed: " ) + fileName,
                     EXIT_FAILURE );
        }
        std::cerr << "Shader: " << fileName << "  " << sType << "\n";
        GLuint shader = glCreateShader( sType );
        checkGL();

        glShaderSource( shader, 1, (const GLchar**) &source, NULL );
        //std::string context = std::string( "glShaderSource: " ) + fileName;
        //checkGLerror( context.c_str() );
        checkGL();

        glCompileShader( shader );
        checkGL();
        
        GLint  compiled;
        glGetShaderiv( shader, GL_COMPILE_STATUS, &compiled );
        if ( !compiled )
        {
            // Don't need to log or delete since exiting, but
            // probably should return an error code rather than exiting
            // and this code shows the better way to handle such failures:
            // which is to let the caller decide what to do.
            std::string msg;
            msg = fileName + std::string( " failed to compile:\n" );
            GLint  logSize;
            glGetShaderiv( shader, GL_INFO_LOG_LENGTH, &logSize );
            char* logMsg = new char[ logSize ];
            glGetShaderInfoLog( shader, logSize, NULL, logMsg );
            msg +=  std::string( "Log: " ) + logMsg;
            delete [] logMsg;
            glDeleteShader( shader );
            logError( msg, EXIT_FAILURE );
        }
        
        delete [] source;
        return shader;
    }
    
    //------------------- makeShaders ---------------------------
    // Create a GLSL program object from vertex and fragment shader files
    //
    GLuint makeShaders( const char* vShaderFile,
                        const char* fShaderFile )
    {
        GLuint program = glCreateProgram();
        checkGL();
        
        GLuint vShader = makeShader( vShaderFile, GL_VERTEX_SHADER );
        checkGL();
        GLuint fShader = makeShader( fShaderFile, GL_FRAGMENT_SHADER );
        checkGL();
        
        glAttachShader( program, vShader );
        glAttachShader( program, fShader );
        
        /* link  and error check */
        glLinkProgram( program );
        checkGL();
        
        GLint  linked;
        glGetProgramiv( program, GL_LINK_STATUS, &linked );
        if ( !linked )
        {
            std::string msg;
            msg = std::string( "Shader program failed to link\n" );
            GLint  logSize;
            glGetProgramiv( program, GL_INFO_LOG_LENGTH, &logSize);
            char* logMsg = new char[logSize];
            glGetProgramInfoLog( program, logSize, NULL, logMsg );
            msg += std::string( logMsg );
            
            // cleanup not really need since we are exiting; but
            //  probably should return -1 and not exit
            delete [] logMsg;
            glDeleteProgram( program ); // program no good any more
            
            logError( msg, EXIT_FAILURE );
        }
        //Always detach shaders after a successful link.
        glDetachShader(program, vShader);
        glDetachShader(program, fShader);
        
        return program;
    }
