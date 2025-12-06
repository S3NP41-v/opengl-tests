#version 330
layout (location = 0) in vec3 vPos;
layout (location = 1) in vec3 vCol;
layout (location = 2) in vec2 vUV;

out vec3 ourColor;
out vec2 texCoord;

uniform float totalTime;
uniform float delta;
uniform vec2  resolution;



void main() {
  gl_Position = vec4(vPos, 1.0);
  
  ourColor = vCol;
  texCoord = vUV;
}
  
