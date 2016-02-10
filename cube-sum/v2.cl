__kernel void cube_sums(__global uint* in, __global uint* out) {
    uint a = in[0];
    uint b = get_global_id(0);
    ulong a3 = (ulong)a * (ulong)a * (ulong)a;
    ulong a3b3 = a3 + (ulong)b * (ulong)b * (ulong)b;

    for (uint c = ceil(cbrt((float)a3b3 / 2)); c <= a - 1; c++) {
        ulong d3 = a3b3 - (ulong)c * (ulong)c * (ulong)c;
        ulong d = round(cbrt((float)d3));
        if (d * d * d == d3) {
            out[2 * b] = c;
            out[2 * b + 1] = d;
            return;
        }
    }
    out[2 * b] = 0;
    out[2 * b + 1] = 0;
}
