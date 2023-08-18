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

package main

import (
	"fmt"
	"math/rand"
	"time"
)

const (
	MAX_DATA_SIZE = 1 * 100000 // x * 100k elements
)

type profitInfo struct {
	IndexMin  uint32
	IndexMax  uint32
	MinVal    int32
	MaxVal    int32
	Profit    int32
}

func fillRandom(data []int32) {
	for i := 0; i < len(data); i++ {
		data[i] = rand.Int31n(1000)
		if rand.Intn(2) == 1 {
			data[i] *= -1
		}
	}
}

func stockPickerLinear(data []int32, profits *profitInfo) {
	currVal := data[0]
	minTemp := currVal
	maxTemp := int32(0)
	indexMinTemp := uint32(1)
	tempDif := int32(0)

	for i := uint32(1); i <= uint32(len(data)); i++ {
		if currVal > maxTemp {
			maxTemp = currVal
			tempDif = maxTemp - minTemp
			if tempDif > profits.Profit {
				profits.Profit = tempDif
				profits.MinVal = minTemp
				profits.MaxVal = maxTemp
				profits.IndexMin = indexMinTemp
				profits.IndexMax = i
			}
		} else if currVal < minTemp {
			minTemp = currVal
			maxTemp = minTemp
			indexMinTemp = i
		}
		currVal += data[i-1]
	}
}

func stockPickerQuadratic(data []int32, profits *profitInfo) {
	minTemp := data[0]

	for i := uint32(1); i <= uint32(len(data)); i++ {
		minTemp += data[i-1]
		tempDif := int32(0)
		currVal := minTemp
		for j := i + 1; j < uint32(len(data)); j++ {
			currVal += data[j-1]
			tempDif = currVal - minTemp
			if tempDif > profits.Profit {
				profits.Profit = tempDif
				profits.MinVal = minTemp
				profits.MaxVal = currVal
				profits.IndexMin = i + 1
				profits.IndexMax = j + 1
			}
		}
	}
}

func pickStocksWithTime(data []int32, stockPicker func([]int32, *profitInfo)) {
	var profits profitInfo

	start := time.Now()
	stockPicker(data, &profits)
	end := time.Now()

	elapsed := float64(end.Sub(start).Microseconds())

	fmt.Println("----------------------------------")
	fmt.Printf("Elements: %d \n", len(data))
	fmt.Printf("Time used: %F ms \n", elapsed / 1000.0)
	fmt.Printf("Buy day: %d \n", profits.IndexMin)
	fmt.Printf("Buy value: %d \n", profits.MinVal)
	fmt.Printf("Sell day: %d \n", profits.IndexMax)
	fmt.Printf("Sell value: %d \n", profits.MaxVal)
	fmt.Printf("Profit: %d \n", profits.Profit)
	fmt.Println("----------------------------------")
}

func main() {
	for i := 100; i <= MAX_DATA_SIZE; i *= 10 {
		rand.Seed(time.Now().UnixNano())
		data := make([]int32, i)
		fillRandom(data)
		pickStocksWithTime(data, stockPickerLinear)
		pickStocksWithTime(data, stockPickerQuadratic)
	}
}