#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <CL/opencl.h>

typedef uint64_t n_t;

cl_device_id create_device_id() {
   cl_platform_id platform;
   cl_device_id device_id;
   int err;

   err = clGetPlatformIDs(1, &platform, NULL);
   if (err < 0) {
      perror("Couldn't identify a platform");
      exit(1);
   }
   err = clGetDeviceIDs(platform, CL_DEVICE_TYPE_GPU, 1, &device_id, NULL);
   if (err == CL_DEVICE_NOT_FOUND) {
      err = clGetDeviceIDs(platform, CL_DEVICE_TYPE_CPU, 1, &device_id, NULL);
   }
   if (err < 0) {
      perror("Couldn't access any devices");
      exit(1);
   }
   return device_id;
}

cl_program build_program(cl_context context, cl_device_id device_id,
        const char* filename) {
    cl_program program;
    FILE *program_handle;
    char *program_buffer, *program_log;
    size_t program_size, log_size;
    int err;
 
    program_handle = fopen(filename, "r");
    if (program_handle == NULL) {
        perror("Couldn't find the program file");
        exit(1);
    }
    fseek(program_handle, 0, SEEK_END);
    program_size = ftell(program_handle);
    rewind(program_handle);
    program_buffer = (char*)malloc(program_size + 1);
    program_buffer[program_size] = '\0';
    int i = fread(program_buffer, sizeof(char), program_size, program_handle);
    fclose(program_handle);
 
    program = clCreateProgramWithSource(context, 1,
       (const char**)&program_buffer, &program_size, &err);
    if (err < 0) {
        perror("Couldn't create the program");
        exit(1);
    }
    free(program_buffer);
 
    err = clBuildProgram(program, 0, NULL, NULL, NULL, NULL);
    if (err < 0) {
        clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG,
            0, NULL, &log_size);
        program_log = (char*)malloc(log_size + 1);
        program_log[log_size] = '\0';
        clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG,
            log_size + 1, program_log, NULL);
        printf("%s\n", program_log);
        free(program_log);
        exit(1);
    }
    return program;
}

void main(int argc, char *argv[]) {
    struct timeval tv1, tv2;
    cl_device_id device_id;
    cl_context context;
    cl_program program;
    cl_kernel kernel;
    cl_command_queue comm_q;
    //int max_out_rows_per_in = 1;
    //int max_out_rows = max_out_rows_per_in * in_size;
    uint32_t a;
    int in_size;
    int out_size;
    uint32_t *in;
    uint32_t *out;
    cl_mem in_buffer;
    cl_mem out_buffer;
    int err = 0;
    size_t local_size = 1;
    size_t global_size;
    //cl_int num_groups = global_size / local_size;

    gettimeofday(&tv1, NULL);

    a = atoi(argv[1]);
    global_size = a - 2;
    in_size = a + 1;
    out_size = 2 * in_size;

    in = (uint32_t*)malloc(in_size * sizeof(uint32_t));
    in[0] = atoi(argv[1]);
    for (uint32_t b = 1; b <= a - 2; b++) {
        in[b] = b;
    }

    out = (uint32_t*)malloc(out_size * sizeof(uint32_t));

    device_id = create_device_id();
    context = clCreateContext(0, 1, &device_id, NULL, NULL, &err);
    if (!context) {
        perror("Failed to create a compute context");
        exit(1);
    }
    //cl_command_queue my_device_q = clCreateCommandQueueWithProperties(CLU_CONTEXT, cluGetDevice(CL_DEVICE_TYPE_GPU), qprop, &status);
    comm_q = clCreateCommandQueue(context, device_id, 0, &err);
    if (!comm_q) {
        perror("Failed to create a command queue");
        exit(1);
    }
    program = build_program(context, device_id, "v2.cl");
    kernel = clCreateKernel(program, "cube_sums", &err);
    if (err < 0) {
        perror("Couldn't create a kernel");
        exit(1);
    }

    in_buffer = clCreateBuffer(context,
            CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
            in_size * sizeof(uint32_t), in, &err);
    if (err < 0) {
        perror("Couldn't create in_buffer");
        exit(1);
    }
    out_buffer = clCreateBuffer(context,
            CL_MEM_WRITE_ONLY | CL_MEM_COPY_HOST_PTR,
            out_size * sizeof(uint32_t), out, &err);
    if (err < 0) {
        perror("Couldn't create out_buffer");
        exit(1);
    }
    for (int i = 0; i < out_size; i++) {
        out[i] = 0;
    }
    err = clSetKernelArg(kernel, 0, sizeof(cl_mem), &in_buffer);
    if (err < 0) {
        perror("Couldn't create kernel argument in_buffer");
        exit(1);
    }
    err = clSetKernelArg(kernel, 1, sizeof(cl_mem), &out_buffer);
    if (err < 0) {
        perror("Couldn't create kernel argument out_buffer");
        exit(1);
    }
    err = clEnqueueNDRangeKernel(comm_q, kernel, 1, NULL, &global_size,
        &local_size, 0, NULL, NULL);
    if (err < 0) {
        perror("Couldn't enqueue the kernel");
        exit(1);
    }
    err = clEnqueueReadBuffer(comm_q, out_buffer, CL_TRUE, 0,
        out_size * sizeof(uint32_t), out, 0, NULL, NULL);
    if (err < 0) {
        perror("Couldn't read the buffer");
        exit(1);
    }
    for (int b = 1, i = 0; b <= a - 2; b++, i += 2) {
        if (out[i] == 0) continue;
        printf("(%u,%u,%u,%u)\n", a, b, out[i], out[i + 1]);
    }

    /*
    n_t a = atoll(argv[1]);
    n_t a3 = a * a * a;
    for (n_t b = 1; b <= a - 2; b++) {
        n_t a3b3 = a3 + b * b * b;
        n_t c_min = ceil(cbrt((double)a3b3 / 2));
        for (n_t c = c_min; c <= a - 1; c++) {
            n_t d3 = a3b3 - c * c * c;
            if (d3 % 4 == 2) continue;
            if (d3 % 8 == 4) continue;
            if (d3 % 32 == 16) continue;
            if (d3 % 64 == 32) continue;
            if (d3 % 256 == 128) continue;
            n_t d = round(cbrt((double)d3));
            if (d * d * d == d3) {
                printf("(%lud,%lud,%lud,%lud)\n", a, b, c, d);
            }
        }
    }
    */

    gettimeofday(&tv2, NULL);
    fprintf(stderr, "%s: %fs\n", argv[1],
        (double)(tv2.tv_sec - tv1.tv_sec) +
        (double)(tv2.tv_usec - tv1.tv_usec) / 1000000);
}
