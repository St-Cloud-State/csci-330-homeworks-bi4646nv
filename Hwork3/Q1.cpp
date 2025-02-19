#include <iostream>
#include <stack>
#include <vector>

using namespace std;

int partition(vector<int>& intArray, int low, int high) {
    int pivot = intArray[high];  // Pivot is last element
    int i = low - 1;

    for (int j = low; j < high; j++) {
        if (intArray[j] < pivot) {  // Iterate on elements compared to the pivot
            i++;
            swap(intArray[i], intArray[j]);  // Swap smaller element to the left
        }
    }
    swap(intArray[i + 1], intArray[high]);  // Place pivot in proper spot
    return i + 1;  // Return pivot index (same index used for the last swap)
}

void quickSort(vector<int>& intArray) {
    stack<pair<int, int>> arrayStack;  // Stack to store first and last indices of starting and subarrays

    // Push initial values (first index of the array, last index of the array)
    arrayStack.push({0, intArray.size() - 1});

    while (!arrayStack.empty()) {
        int low = arrayStack.top().first; // Grabs the first index of the array to work on
        int high = arrayStack.top().second; // Grabs the last index of the array to work on
        arrayStack.pop();

        if (low < high) {
            int piv = partition(intArray, low, high);  // Partition the array

            // Push left subarray indices
            if (piv - 1 > low) {
                arrayStack.push({low, piv - 1});
            }

            // Push right subarray indices
            if (piv + 1 < high) {
                arrayStack.push({piv + 1, high});
            }
        }
    }
}

// Driver
int main() {
    vector<int> intArray = {1, 8, 2, 9, 4, 6, 7};
    
    cout << "Unsorted array: ";
    for (int num : intArray) {
        cout << num << " ";
    }
    cout << endl;

    quickSort(intArray);

    cout << "Sorted array: ";
    for (int num : intArray) {
        cout << num << " ";
    }
    cout << endl;

    return 0;
}
