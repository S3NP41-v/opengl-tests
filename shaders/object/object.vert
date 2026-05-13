#version 330

layout (location = 0) in vec3 vPos;
layout (location = 1) in vec2 vUV;
layout (location = 2) in vec3 vNorm;

out float dist;
out vec3 pos;
out vec2 uv;
out vec3 norm;


uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;


void main() {
  vec3 fpos = vec3(model * vec4(vPos, 1.0));
  norm = mat3(transpose(inverse(model))) * vNorm;
  uv = vUV;
  pos = fpos;
  
  vec4 npos = projection * view * vec4(fpos, 1.0);
  
  dist = npos.z;


  gl_Position = npos;
}
  
