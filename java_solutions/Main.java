package java_solutions;

import java.util.Random;

public class Main {

    static void fillRandom(int[] data, int len) {
        Random random = new Random();
        for (int i = 0; i < len; i++) {
            data[i] = random.nextInt(1000);
            if (random.nextBoolean()) {
                data[i] *= -1;
            }
        }
    }

    static void pickStocksWithTime(int[] data, int len, boolean linear) {
        StockPicker stockPicker = new StockPicker(0, 0, 0, 0, 0);
        long startTime = System.nanoTime();
        
        if (linear) {
            stockPicker.stockPickerLinear(data, len);
        } else {
            stockPicker.stockPickerQuadratic(data, len);
        }
        
        long endTime = System.nanoTime();

        double elapsed = (endTime - startTime) / 1000000.0; // Convert to milliseconds

        System.out.println("----------------------------------");
        System.out.println("Elements: " + len);
        System.out.println("Time used: " + elapsed + " ms");
        System.out.println("Buy day: " + stockPicker.indexMin());
        System.out.println("Buy value: " + stockPicker.minVal());
        System.out.println("Sell day: " + stockPicker.indexMax());
        System.out.println("Sell value: " + stockPicker.maxVal());
        System.out.println("Profit: " + stockPicker.profit());
        System.out.println("----------------------------------");
    }

    public static void main(String[] args) {
        int MAX_DATA_SIZE = 1 * 100000; // x * 100k elements

        for (int i = 100; i <= MAX_DATA_SIZE; i *= 10) {
            int[] data = new int[i];
            fillRandom(data, i);
            
            pickStocksWithTime(data, i, true);
            pickStocksWithTime(data, i, false);
        }
    }
}