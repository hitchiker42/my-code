#version 330
//#version 130
/**
 * Simple vertex shader; it just transforms the vertex coordinate 
 * by the current projection * view * model matrix.
 */

uniform mat4 projXview;    // this is projection * viewing matrix 
uniform mat4 model;     

uniform vec4 vColor;  

in  vec4 vPosition;
out vec4 color; 

void main()
{
   gl_Position = projXview * model * vPosition;
   color = vColor;
}
