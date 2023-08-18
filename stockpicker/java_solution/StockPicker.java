package stockpicker.java_solution;

public class StockPicker {

    private int profit;
    private int minVal;
    private int maxVal;
    private int indexMin;
    private int indexMax;

    StockPicker(int profit, int minVal, int maxVal, int indexMin, int indexMax) {
        this.profit = profit;
        this.minVal = minVal;
        this.maxVal = maxVal;
        this.indexMin = indexMin;
        this.indexMax = indexMax;
    }

    int profit() {
        return profit;
    }

    void profit(int profit) {
        this.profit = profit;
    }

    int minVal() {
        return minVal;
    }

    void minVal(int minVal) {
        this.minVal = minVal;
    }

    int maxVal() {
        return maxVal;
    }

    void maxVal(int maxVal) {
        this.maxVal = maxVal;
    }

    int indexMin() {
        return indexMin;
    }

    void indexMin(int indexMin) {
        this.indexMin = indexMin;
    }

    int indexMax() {
        return indexMax;
    }

    void indexMax(int indexMax) {
        this.indexMax = indexMax;
    }

    void stockPickerLinear(int[] data, int len) {
        int currVal = data[0];
        int minTemp = currVal;
        int maxTemp = 0;
        int indexMinTemp = 1;
        int tempDif = 0;

        for (int i = 1; i < len; i++) {
            if (currVal > maxTemp) {
                maxTemp = currVal;
                tempDif = maxTemp - minTemp;
                if (tempDif > profit()) {
                    profit(tempDif);
                    minVal(minTemp);
                    maxVal(maxTemp);
                    indexMin(indexMinTemp);
                    indexMax(i);
                }
            } else if (currVal < minTemp) {
                minTemp = currVal;
                maxTemp = minTemp;
                indexMinTemp = i;
            }
            currVal += data[i];
        }
    }

    void stockPickerQuadratic(int[] data, int len) {
        int minTemp = data[0];

        for (int i = 1; i < len; i++) {
            minTemp += data[i];
            int tempDif = 0;
            int currVal = minTemp;
            for (int j = i + 1; j < len; j++) {
                currVal += data[j];
                tempDif = currVal - minTemp;
                if (tempDif > profit()) {
                    profit(tempDif);
                    minVal(minTemp);
                    maxVal(currVal);
                    indexMin(i + 1);
                    indexMax(j + 1);
                }
            }
        }
    }
}
