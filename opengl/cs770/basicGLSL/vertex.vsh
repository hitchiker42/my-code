#version 330 
/**
 * Simple vertex shader; it just transforms the vertex coordinate 
 * by the current projection * view * model matrix.
 */

layout(row_major) uniform; //Use C matrix layout
uniform mat4 projXview;    // this is projection * viewing matrix 
uniform mat4 model;     

layout(location = 0) in vec4 vPosition;
in vec4 vColor;  

out vec4 color; 

void main()
{
   gl_Position = projXview * model * vPosition;
   color = vColor;
}
