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

#include <iostream>
#include <cstdlib>
#include <ctime>
#include <cstdint>
#include <chrono>
#include <random>

#define MAX_DATA_SIZE (1 * 100000) // x * 100k elements

class StockPicker {
private:
    uint32_t index_min;
    uint32_t index_max;
    int32_t min_val;
    int32_t max_val;
    int32_t profit;

public:
    StockPicker() : index_min(0), index_max(0), min_val(0), max_val(0), profit(0) {}

    void stock_picker_linear(const int32_t *data, uint32_t len) {
        int32_t curr_val = *(data);
        int32_t min_temp = curr_val;
        int32_t max_temp = 0;
        uint32_t index_min_temp = 1;
        int32_t temp_dif = 0;

        for (uint32_t i = 1; i <= len; i++) {
            if (curr_val > max_temp) {
                max_temp = curr_val;
                temp_dif = max_temp - min_temp;
                if (temp_dif > profit) {
                    profit = temp_dif;
                    min_val = min_temp;
                    max_val = max_temp;
                    index_min = index_min_temp;
                    index_max = i;
                }
            } else if (curr_val < min_temp) {
                min_temp = curr_val;
                max_temp = min_temp;
                index_min_temp = i;
            }
            curr_val += *(data + i);
        }
    }

    void stock_picker_quadratic(const int32_t *data, uint32_t len) {
        int32_t min_temp = *(data);

        for (uint32_t i = 1; i <= len; i++) {
            min_temp += *(data + i);
            int32_t temp_dif = 0;
            int32_t curr_val = min_temp;
            for (uint32_t j = i + 1; j < len; j++) {
                curr_val += *(data + j);
                temp_dif = curr_val - min_temp;
                if (temp_dif > profit) {
                    profit = temp_dif;
                    min_val = min_temp;
                    max_val = curr_val;
                    index_min = i + 1;
                    index_max = j + 1;
                }
            }
        }
    }

    void pick_stocks_with_time(const int32_t *data, uint32_t len, void (StockPicker::*stock_picker)(const int32_t *, uint32_t)) {
        auto start = std::chrono::high_resolution_clock::now();
        (this->*stock_picker)(data, len);
        auto end = std::chrono::high_resolution_clock::now();

        double elapsed = std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count();

        std::cout << "----------------------------------" << std::endl;
        std::cout << "Elements: " << len << std::endl;
        std::cout << "Time used: " << elapsed / 1000000.0 << " ms" << std::endl;
        std::cout << "Buy day: " << index_min << std::endl;
        std::cout << "Buy value: " << min_val << std::endl;
        std::cout << "Sell day: " << index_max << std::endl;
        std::cout << "Sell value: " << max_val << std::endl;
        std::cout << "Profit: " << profit << std::endl;
        std::cout << "----------------------------------" << std::endl;
    }
};

void fill_random(int32_t *data, uint32_t len, std::uniform_int_distribution<int32_t> dist, std::mt19937 gen)
{
    for (int32_t i = 0; i < len; i++) {
        data[i] = dist(gen);
    }
}

int main()
{
    for (uint32_t i = 100; i <= MAX_DATA_SIZE; i *= 10) {
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<int32_t> dist(-1000, 1000);

        int32_t *data = new int32_t[i];
        fill_random(data, i, dist, gen);

        StockPicker picker;
        picker.pick_stocks_with_time(data, i, &StockPicker::stock_picker_linear);
        picker.pick_stocks_with_time(data, i, &StockPicker::stock_picker_quadratic);

        delete[] data;
    }

    return 0;
}
