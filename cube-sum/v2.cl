__kernel void cube_sums(__global uint* in, __global uint* out) {
    uint out_i = 0;
    uint a = in[0];
    ulong a3 = (ulong)a * (ulong)a * (ulong)a;

    // why no access to in[1]?
    //out[0] = 400;
    //out[1] = 399;
    //out[2] = in[0];
    //out[3] = in[0];
    //return;

    for (uint b = 1; b <= a - 2; b++) {
        ulong a3b3 = a3 + (ulong)b * (ulong)b * (ulong)b;
        for (uint c = ceil(cbrt((float)a3b3 / 2)); c <= a - 1; c++) {
            ulong d3 = a3b3 - (ulong)c * (ulong)c * (ulong)c;
            ulong d = round(cbrt((float)d3));
            if (d * d * d == d3) {
                out[out_i++] = a;
                out[out_i++] = b;
                out[out_i++] = c;
                out[out_i++] = d;
            }
        }
    }
}
