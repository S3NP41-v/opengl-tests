#version 330
layout (location = 0) in vec3 vPos;
layout (location = 1) in vec2 vUV;

out vec2 texCoord;

uniform float totalTime;
uniform float delta;
uniform vec2  resolution;
uniform mat4  rotation;


void main() {
  vec4 npos = rotation * vec4(vPos, 1.0);

  gl_Position = npos;
  
  texCoord = vUV;
}
  
