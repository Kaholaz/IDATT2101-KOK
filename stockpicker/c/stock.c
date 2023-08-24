/*
 *  Copyright (C) 2023 Callum Gran
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>

#define __USE_POSIX199309 1
#define MAX_DATA_SIZE (10 * 100000) // x * 100k elements

struct profit_info_t {
    uint32_t index_min;
    uint32_t index_max;
    int32_t profit;
};

void fill_random(int32_t *data, uint32_t len)
{
    for (int32_t i = 0; i < len; i++) {
        data[i] = rand() % 1000;
        if (rand() % 2) {
            data[i] *= -1;
        }
    }
}

void stock_picker_linear(const int32_t *data, uint32_t len, struct profit_info_t *profits)
{
    int32_t curr_val = data[0];
    int32_t min_temp = curr_val;
    uint32_t index_min_temp = 1;

    for (uint32_t i = 1; i <= len; i++) {
        if (curr_val - min_temp > profits->profit) { 
            profits->profit = curr_val - min_temp; 
            profits->index_min = index_min_temp; 
            profits->index_max = i;
        } else if (curr_val < min_temp) { 
            min_temp = curr_val; 
            index_min_temp = i; 
        }
        curr_val += data[i];
    }
}

void stock_picker_quadratic(const int32_t *data, uint32_t len, struct profit_info_t *profits)
{
    int32_t min_temp = *(data);

    for (uint32_t i = 1; i <= len; i++) {
        min_temp += *(data + i);
        int32_t temp_dif = 0;
        int32_t curr_val = min_temp;
        for (uint32_t j = i + 1; j < len; j++) {
            curr_val += *(data + j);
            temp_dif = curr_val - min_temp;
            if (temp_dif > profits->profit) {
                profits->profit = temp_dif;
                profits->index_min = i + 1;
                profits->index_max = j + 1;
            }
        }
    }
}

void pick_stocks_with_time(const int32_t *data, uint32_t len, void (*stock_picker)(const int32_t *, uint32_t, struct profit_info_t *))
{
    struct profit_info_t profits = {0};

    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC_RAW, &start);
    stock_picker(data, len, &profits);
    clock_gettime(CLOCK_MONOTONIC_RAW, &end);

    double elapsed;
    elapsed = (end.tv_sec - start.tv_sec);
    elapsed += (end.tv_nsec - start.tv_nsec) / 1000000000.0;
    elapsed *= 1000;
    
    printf("----------------------------------\n");
    printf("Elements: %d \n", len);
    printf("Time used: %f ms \n", elapsed);
    printf("Buy day: %d \n", profits.index_min);
    printf("Sell day: %d \n", profits.index_max);
    printf("Profit: %d \n", profits.profit);
    printf("----------------------------------\n");
}

int main() 
{
    for (uint32_t i = 100; i <= MAX_DATA_SIZE; i *= 10) {
        srand(time(NULL));
        int32_t *restrict data = malloc(i * sizeof(int32_t));
        fill_random(data, i);
        pick_stocks_with_time(data, i, stock_picker_linear);
        // pick_stocks_with_time(data, i, stock_picker_quadratic);
        free(data);
    }

    return 0;
}