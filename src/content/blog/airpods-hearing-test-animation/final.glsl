
#define TAU 6.28318530718

precision mediump float;

varying vec2 vPosition;
uniform float uTime;
uniform float uExtra;
uniform float uAspectRatio;
uniform vec4 uColor;

void main() {
        vec3 rgb = uColor.rgb;
        vec2 uv = vPosition.xy;

        float r = .01;

        float uSymmetries = 200.;

        float a = TAU/uSymmetries;
        float a2 = a/2.;

        float theta = atan(uv.y, uv.x);
        float rho = length(uv);

        // find the slice theta is in (slice 0 around theta == 0*a, slice 1 around theta == 1*a, etc)
        // NOTE: we shift theta to in [0;TAU] instead of [-TAU/2,TAU/2] to have positive
        // indices and add a2 to effectively move the slices back (so that they are e.g.
        // _around_ // theta = 0 and not _starting at_ theta = 0
        float slice_ix = floor(mod(theta + a2 + TAU, TAU) / a);

        float uDist = .007;
        uDist += .002 * sin(3./17. * uTime * TAU + slice_ix/uSymmetries * TAU);
        uDist += .005 * sin(1./11. * uTime * TAU + slice_ix/uSymmetries * TAU);

        uDist = mix(uDist, 1./32., uExtra);

        // move theta up by half alpha to avoid being cut in half, then make everything repeat
        // [0, a], [a, 2a], then move back by half alpha
        theta = mod(theta+a2, a)-a2;

        uv = vec2(rho*cos(theta), rho*sin(theta));
        // what is this?
        uv += vec2(-.8 , 0.);

        float opacity = 0.;

        const int nSideDots = 8;

        for (int i = -nSideDots; i <= nSideDots; i ++) {
            vec2 delta = vec2(float(i)*uDist, 0.);
            float v = length(uv + delta);
            float ri = .98 * r;
            float alpha_this = 1.;
            alpha_this *= (1. - smoothstep(ri, r, v));
            alpha_this *= 1. - smoothstep(.2, 1., abs(float((i)))/float(nSideDots));
            opacity = 1. - (1. - alpha_this) * (1. - opacity);
        }

        gl_FragColor = opacity * vec4(rgb, 1.);
}
