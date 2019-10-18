#include <iostream>
#include <random>
#include <SDL.h>
using namespace std;

using num_t = uint16_t;
using num_udist = uniform_int_distribution<num_t>;

num_t scr_w = 2560;
num_t scr_h = 1440;
num_t life_w = 32;
num_t life_h = 32;
num_t food_w = 4;
num_t food_h = 4;

#define life_num 128
#define food_num 128

struct life_t {
    num_t x;
    num_t y;
    float energy;
} lifes[life_num];

struct food_t {
    num_t x;
    num_t y;
} foods[food_num];

mt19937 gen(1);

void init_lifes() {
    num_udist x_dist(0, scr_w - life_w);
    num_udist y_dist(0, scr_h - life_h);
    for (num_t i = 0; i < life_num; i++) {
        lifes[i].x = x_dist(gen);
        lifes[i].y = y_dist(gen);
        lifes[i].energy = 0;
    }
}

num_udist food_x_dist(0, scr_w - food_w);
num_udist food_y_dist(0, scr_h - food_h);

void init_foods() {
    for (num_t i = 0; i < food_num; i++) {
        foods[i].x = food_x_dist(gen);
        foods[i].y = food_y_dist(gen);
    }
}

SDL_Event event;
SDL_Window* window;
SDL_Renderer* rendr;

int init_gfx() {
    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        fprintf(stderr, "Couldn't intialize SDL: %s\n", SDL_GetError());
        return EXIT_FAILURE;
    }
    window = SDL_CreateWindow("block-life", SDL_WINDOWPOS_UNDEFINED,
        SDL_WINDOWPOS_UNDEFINED, scr_w, scr_h,
        SDL_WINDOW_BORDERLESS | SDL_WINDOW_SHOWN);
    if (!window) {
        fprintf(stderr, "Couldn't create SDL window: %s\n", SDL_GetError());
        SDL_Quit();
        return EXIT_FAILURE;
    }
    rendr = SDL_CreateRenderer(window, -1, 0);
    return EXIT_SUCCESS;
}

void draw_sq(num_t x1, num_t y1, num_t w, num_t h) {
    for (num_t x = x1; x < x1 + w; x++) {
        SDL_RenderDrawPoint(rendr, x, y1);
        SDL_RenderDrawPoint(rendr, x, y1 + h - 1);
    }
    for (num_t y = y1 + 1; y < y1 + h - 1; y++) {
        SDL_RenderDrawPoint(rendr, x1, y);
        SDL_RenderDrawPoint(rendr, x1 + w - 1, y);
    }
}

void show() {
    SDL_SetRenderDrawColor(rendr, 255, 255, 255, 255);
    SDL_RenderClear(rendr);
    SDL_SetRenderDrawColor(rendr,   0,   0,   0, 255);
    for (num_t i = 0; i < food_num; i++) {
        draw_sq(foods[i].x, foods[i].y, food_w, food_h);
    }
    for (num_t i = 0; i < life_num; i++) {
        draw_sq(lifes[i].x, lifes[i].y, life_w, life_h);
    }
    SDL_RenderPresent(rendr);
}

void move() {
    num_udist dist(0, 2);
    for (num_t i = 0; i < life_num; i++) {
        lifes[i].x += dist(gen);
        if (lifes[i].x > 0) {
            if (lifes[i].x < scr_w) {
                lifes[i].x--;
            } else {
                lifes[i].x = scr_w - 1;
            }
        }
        lifes[i].y += dist(gen);
        if (lifes[i].y > 0) {
            if (lifes[i].y < scr_h) {
                lifes[i].y--;
            } else {
                lifes[i].y = scr_h - 1;
            }
        }
        for (num_t j = 0; j < food_num; j++) {
            if (foods[j].x > lifes[i].x &&
                foods[j].x + food_w < lifes[i].x + life_w &&
                foods[j].y > lifes[i].y &&
                foods[j].y + food_h < lifes[i].y + life_h) {

                lifes[i].energy++;
                foods[j].x = food_x_dist(gen);
                foods[j].y = food_y_dist(gen);
            }
        }
    }
}

int main() {
    init_lifes();
    init_foods();
    if (int r = init_gfx()) {
        return r;
    }
    while (true) {
        show();
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_QUIT) {
                SDL_DestroyRenderer(rendr);
                SDL_DestroyWindow(window);
                SDL_Quit();
                printf("Exited normally.\n");
                return EXIT_SUCCESS;
            }
        }
        move();
    }
}
