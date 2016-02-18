__kernel void cube_sums(__global uint* in, __global uint* out) {
    __local uint a;
    __local uint b;
    __local uint c;
    __local uint d;
    __local ulong a3b3;
    __local ulong d3;

    a = in[0];
    b = get_global_id(0);
    a3b3 = (ulong)a * (ulong)a * (ulong)a + (ulong)b * (ulong)b * (ulong)b;

    for (c = ceil(cbrt((float)a3b3 / 2)); c <= a - 1; c++) {
        d3 = a3b3 - (ulong)c * (ulong)c * (ulong)c;
        d = round(cbrt((float)d3));
        if (d * d * d == d3) {
            out[2 * b] = c;
            out[2 * b + 1] = d;
            return;
        }
    }
    out[2 * b] = 0;
    out[2 * b + 1] = 0;
}
