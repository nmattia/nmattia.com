#define TAU 6.28318530718

precision mediump float;

varying vec2 vPosition;
uniform vec4 uColor;
uniform float uTime;
uniform float uLastTouch;
uniform float uGap;

vec2 polar(in vec2 uv) {
    float theta = atan(uv.y, uv.x);
    float rho = length(uv);
    return vec2(rho, theta);
}

vec2 cart(in vec2 p) {
    return vec2(p.x*cos(p.y), p.x*sin(p.y));
}

const float RADIUS = .7;

const int NDOTS = 24;

// x from 0 to 1
float foo(float thickness, float x) {
    float start = RADIUS - thickness/2.;
    float stop = RADIUS + thickness/2.;

    float rampup = thickness/3.;


    return smoothstep(start, start + rampup, x) * (1. - smoothstep(stop - rampup, stop, x));
}

void main() {

    vec2 uv = vec2(vPosition.x, vPosition.y);

    float factor = 1.;

    vec2 p = polar(uv);


    float nSlices = 180.;
    float sliceAngle = TAU/nSlices;
    float theta = p.y;
    p.y = mod(p.y + sliceAngle/2., sliceAngle) - sliceAngle/2.;

    uv = cart(p);

    float elapsed = uTime - uLastTouch;
    float explodeDuration = .34;
    float exploderampup = explodeDuration/2.;

    float explodeFactor = smoothstep(0., 0. + exploderampup, elapsed) * (1. -
            smoothstep(explodeDuration - exploderampup, explodeDuration, elapsed))
        ;

    float thickness = explodeFactor * .45 + .1 +
        (1. - explodeFactor) * (0.06 * sin(theta + (1./9.) * uTime * TAU) +
                0.02 * cos(theta + (1./5.) * uTime * TAU));

    float n = 1./thickness * float(NDOTS);
    uv.x += 1./n/2.;
    float ix = floor(uv.x * n)/n;
    uv.x = mod(uv.x, 1./n);
    uv.x -= 1./n/2.;
    float alpha = clamp(foo(thickness, ix), 0., 1.);

    // Size & blur of a dot
    float size = .015;
    float blur = size/20.;

    factor *= (1. - smoothstep(size/2. - 2.*blur, size/2., length(uv)));
    gl_FragColor = alpha * factor * vec4(.2 + 0.01/thickness, .6 + .02/thickness, .9,1.);
}
