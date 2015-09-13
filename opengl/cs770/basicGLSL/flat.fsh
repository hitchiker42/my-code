#version 330  
/**
 * flat fragment shader
 * color comes from program
 */

in vec4 color;
out vec4 fcolor;

void main()
{
    fcolor = color;
}
