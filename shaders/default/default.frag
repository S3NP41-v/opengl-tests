#version 330
in vec2 texCoord;

out vec4 finalColor;

uniform float totalTime;
uniform float delta;
uniform vec2  resolution;
uniform mat4  rotation;
uniform sampler2D cubeTexture;




void main() {
  finalColor = texture(cubeTexture, texCoord);

}
  
