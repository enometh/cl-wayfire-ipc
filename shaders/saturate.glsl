#version 300 es
@builtin_ext@
@builtin@

#ifdef GL_ES
precision highp float;
#endif

in highp vec2 uvpos;
out vec4 out_color;

// gnome-saturation-extension/glslEffect.js
vec3 hueShift(vec3 col, float hue) {
  // assert (hue != 0.0 )
    const vec3 k = vec3(0.57735, 0.57735, 0.57735);
    float cosAngle = cos(hue);
    return vec3(col * cosAngle + cross(k, col) * sin(hue) + k * dot(k, col) * (1.0 - cosAngle));
};

// stackexchange questions/9234724/how-to-change-hue-of-a-texture-with-glsl
vec4 yiqShift4(vec4 color, float hueAdjust)
{

  if (hueAdjust == 0.0) {
    return color;
  }

    const vec4  kRGBToYPrime = vec4 (0.299, 0.587, 0.114, 0.0);
    const vec4  kRGBToI     = vec4 (0.596, -0.275, -0.321, 0.0);
    const vec4  kRGBToQ     = vec4 (0.212, -0.523, 0.311, 0.0);

    const vec4  kYIQToR   = vec4 (1.0, 0.956, 0.621, 0.0);
    const vec4  kYIQToG   = vec4 (1.0, -0.272, -0.647, 0.0);
    const vec4  kYIQToB   = vec4 (1.0, -1.107, 1.704, 0.0);

    // Convert to YIQ
    float   YPrime  = dot (color, kRGBToYPrime);
    float   I      = dot (color, kRGBToI);
    float   Q      = dot (color, kRGBToQ);

    // Calculate the hue and chroma
    float   hue     = atan (Q, I);
    float   chroma  = sqrt (I * I + Q * Q);

    // Make the user's adjustments
    hue += hueAdjust;

    // Convert back to YIQ
    Q = chroma * sin (hue);
    I = chroma * cos (hue);

    // Convert back to RGB
    vec4    yIQ   = vec4 (YPrime, I, Q, 0.0);
    color.r = dot (yIQ, kYIQToR);
    color.g = dot (yIQ, kYIQToG);
    color.b = dot (yIQ, kYIQToB);
 return color;
}

// wayfire-plugins-extra/subprojects/filters/shaders/saturate
vec3 saturate2(vec3 rgb, float adjustment)
{
    const vec3 w = vec3(0.2125, 0.7154, 0.0721);
    vec3 intensity = vec3(dot(rgb, w));
    return mix(intensity, rgb, adjustment);
}

// gnome-saturation-extension/glslEffect.js
vec3 saturate1(vec3 color, float saturation_factor)
{

  if (saturation_factor == 1.0) {
    return color;
  }

    // screen to linear - approx
//    color = pow(color, vec3(2.2));

    float luminance = dot(color, vec3(0.212656, 0.715158, 0.072186));
    vec3 gray = vec3(luminance);
    float mix_factor = saturation_factor;
    if (mix_factor > 1.0) {
        mix_factor = 1.0 + (mix_factor - 1.0) * 5.0;
        vec3 delta = color - gray;
        delta = mix(delta, vec3(sign(delta.x)*0.0001, sign(delta.y)*0.0001, sign(delta.z)*0.0001), step(abs(delta), vec3(0.0001)));
        vec3 limit_pos = (1.0 - gray) / delta;
        vec3 limit_neg = gray / (-delta);
        vec3 limit = mix(limit_neg, limit_pos, step(vec3(0.0), delta));
        limit = min(limit, vec3(1e6));
        float max_factor = min(min(limit.x, limit.y), limit.z);
        mix_factor = min(mix_factor, max_factor);
    }
    color = mix(gray, color, mix_factor);
    return color;
//    // linear to screen - approx
//    return pow(color, vec3(1.0/2.2));
}

// wayfire filters plugin
void main()
{
     float   opacity = 1.0;
     float  brightness = 1.0;
     float  saturation= 0.81;

     float hue_shift = -0.1;

     vec4 c = get_pixel(uvpos);

     /*
    c = c * opacity;
    c = vec4(c.rgb * brightness, c.a);
    if (brightness != 1.0) {
      c = vec4(c.rgb * brightness, c.a);
    }
     */


    if (saturation != 1.0) {
      c = vec4(saturate2(c.rgb, saturation), c.a);
      //c = vec4(saturate1(c.rgb, saturation), c.a);
    }
    if (hue_shift != 0.0) {
      c =  yiqShift4( c, hue_shift );
      //c = hueShift(c, hueShift);
    }

    out_color = c;
}


/*
;;; ----------------------------------------------------------------------
;;madhu 251005  notes: fragment shaders skeletons for
- picom: picom --window-shader-fg=SHADER --root-pixmap-shader=SHADER
```
#version 330
in vec2 texcoord;
uniform sampler2D tex;
vec4 default_post_processing(vec4 c);
vec4 window_shader() {
 vec2 texsize = textureSize(tex, 0);
 vec4 c = texture2D(tex, texcoord / texsize, 0);
 return default_post_processing(c);
}
```
- hyprland: hyprctl keyword decoration:screen_shader SHADER
```
#version 320 es
precision mediump float;
in vec2 v_texcoord;
uniform sampler2D tex;
out vec4 color;
void main() {
  color = texture2D(tex, v_texcoord);
|
```
*/