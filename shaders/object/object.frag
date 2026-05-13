#version 330
in float dist;

in vec3 pos;
in vec2 uv;
in vec3 norm;

out vec4 finalColor;

uniform sampler2D vTexture;
uniform vec3 lightPos;
uniform vec3 viewPos;

void main() {
  // ambient
  float ambientStrength = 0.1;
  vec4  ambientColor    = vec4(1, 1, 1, 1);
  
  vec4 ambient = ambientStrength * ambientColor;
  
  // diffuse
  float diffuseStrength = 1;
  vec4 diffuseColor     = vec4(1, 1, 1, 1);

  vec3 lightDir = normalize(lightPos - pos);
  float diff    = max(dot(norm, lightDir), 0.0);
  vec4 diffuse  = diffuseStrength * diff * diffuseColor;

  // specular
  float specularStrength = 0.5;
  vec4  specularColor    = vec4(1, 1, 1, 1);
  int   specularShine    = 32;

  vec3 viewDir = normalize(viewPos - pos);
  vec3 reflectDir = reflect(-lightDir, norm);

  float spec = pow(max(dot(viewDir, reflectDir), 0.0), specularShine);
  vec4 specular = specularStrength * spec * specularColor;


  finalColor = (ambient + diffuse + specular) * texture(vTexture, uv);
}
  
