#version 330
in vec3 ourColor;
in vec2 texCoord;

out vec4 finalColor;

uniform float totalTime;
uniform float delta;
uniform vec2  resolution;

uniform sampler2D missing;

void main() {

  finalColor = texture(missing, texCoord);
}
  
