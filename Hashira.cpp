#include<bits/stdc++.h>
#include "json.hpp"

using namespace std;
using json = nlohmann::json;

// Manual implementation of GCD for C++14 compatibility
long long gcd(long long a, long long b) {
    while (b != 0) {
        long long temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

// Manual implementation of LCM for C++14 compatibility
long long lcm(long long a, long long b) {
    if (a == 0 || b == 0) return 0;
    return abs(a / gcd(a, b) * b);
}

// Convert string from a given base to decimal
long long from_base(const string &num, int base) {
    long long res = 0;
    for (char c : num) {
        int digit = 0;
        if (isdigit(c)) digit = c - '0';
        else if (islower(c)) digit = c - 'a' + 10;
        else if (isupper(c)) digit = c - 'A' + 10;
        res = res * base + digit;
    }
    return res;
}

// Function to evaluate small expressions like sum(), subtraction(), lcm(), etc.
long long evaluateExpression(const string& expr) {
    auto pos = expr.find('(');
    if (pos == string::npos) {
        throw invalid_argument("Invalid share function format: " + expr);
    }
    string func = expr.substr(0, pos);
    string args_str = expr.substr(pos+1, expr.size() - pos - 2);

    vector<long long> args;
    stringstream ss(args_str);
    string item;
    while (getline(ss, item, ',')) {
        size_t start = item.find_first_not_of(" \t");
        size_t end = item.find_last_not_of(" \t");
        if (start == string::npos) throw invalid_argument("Empty argument in expression: " + expr);
        string numstr = item.substr(start, end - start + 1);
        args.push_back(stoll(numstr));
    }

    if (func == "sum") {
        return accumulate(args.begin(), args.end(), 0LL);
    } else if (func == "subtraction" || func == "subtract") {
        if (args.size() < 2) throw invalid_argument("Subtraction requires 2+ arguments: " + expr);
        long long res = args[0];
        for (size_t i = 1; i < args.size(); i++) res -= args[i];
        return res;
    } else if (func == "mul" || func == "multiply" || func == "product") {
        long long res = 1;
        for (auto v : args) {
            // Check for overflow
            if (v != 0 && res != 0 && 
                ((v > 0 && res > numeric_limits<long long>::max() / v) || 
                 (v < 0 && res < numeric_limits<long long>::min() / v) ||
                 (v < 0 && res > numeric_limits<long long>::max() / v) ||
                 (v > 0 && res < numeric_limits<long long>::min() / v))) {
                throw overflow_error("Multiplication overflow in expression: " + expr);
            }
            res *= v;
        }
        return res;
    } else if (func == "div" || func == "divide") {
        if (args.size() != 2) throw invalid_argument("Division requires exactly 2 arguments: " + expr);
        if (args[1] == 0) throw invalid_argument("Division by zero: " + expr);
        return args[0] / args[1];
    } else if (func == "lcm") {
        if (args.empty()) throw invalid_argument("LCM requires at least 1 argument: " + expr);
        long long res = args[0];
        for (size_t i = 1; i < args.size(); i++) {
            // Check for overflow in LCM calculation
            if (res != 0 && args[i] != 0) {
                long long gcd_val = gcd(res, args[i]);
                if (gcd_val != 0 && (res / gcd_val > numeric_limits<long long>::max() / args[i] || 
                                     args[i] / gcd_val > numeric_limits<long long>::max() / res)) {
                    throw overflow_error("LCM overflow in expression: " + expr);
                }
            }
            res = lcm(res, args[i]);
        }
        return res;
    } else {
        throw invalid_argument("Unknown function: " + func);
    }
}

// Lagrange interpolation to reconstruct secret at x=0
long double lagrangeInterpolation(const vector<pair<long double, long double>>& points) {
    size_t k = points.size();
    if (k == 0) {
        throw invalid_argument("Empty points vector for Lagrange interpolation");
    }
    
    long double secret = 0.0L;
    for (size_t j = 0; j < k; ++j) {
        long double xj = points[j].first;
        long double yj = points[j].second;
        long double lj = 1.0L;
        
        for (size_t m = 0; m < k; ++m) {
            if (m == j) continue;
            long double xm = points[m].first;
            
            // Check for division by zero
            if (abs(xj - xm) < 1e-10) {
                throw invalid_argument("Duplicate x-coordinates found in Lagrange interpolation");
            }
            
            lj *= (0.0L - xm) / (xj - xm);
        }
        secret += yj * lj;
    }
    return secret;
}

// Function to process a single JSON file
long long processFile(const string& filename) {
    cout << "  Opening file: " << filename << endl;
    ifstream ifs(filename);
    if (!ifs.is_open()) {
        cerr << "Error: Cannot open file " << filename << "\n";
        return -1;
    }

    json data;
    try {
        data = json::parse(ifs);
        cout << "  JSON parsed successfully" << endl;
    } catch (const exception& e) {
        cerr << "JSON parse error in " << filename << ": " << e.what() << "\n";
        return -1;
    }

    if (!data.contains("keys") || !data["keys"].contains("n") || !data["keys"].contains("k")) {
        cerr << "Error: " << filename << " must contain 'keys' with 'n' and 'k'\n";
        return -1;
    }

    int n = data["keys"]["n"].get<int>();
    int k = data["keys"]["k"].get<int>();
    cout << "  n=" << n << ", k=" << k << endl;
    if (k <= 0 || n <= 0 || k > n) {
        cerr << "Error: Invalid values for n and k in " << filename << "\n";
        return -1;
    }

    vector<pair<long double, long double>> shares;
    for (auto it = data.begin(); it != data.end(); ++it) {
        string key = it.key();
        if (key == "keys") continue;

        long long x;
        try {
            x = stoll(key);
        } catch (...) {
            cerr << "Warning: Skipping invalid key '" << key << "' in " << filename << "\n";
            continue;
        }

        // Check if the value is an object with base and value fields
        if (it.value().is_object() && it.value().contains("base") && it.value().contains("value")) {
            int base = stoi(it.value()["base"].get<string>());
            string val = it.value()["value"].get<string>();
            long long y = from_base(val, base);
            shares.push_back({(long double)x, (long double)y});
            cout << "  Share " << x << ": " << val << " (base " << base << ") = " << y << endl;
        } else if (it.value().is_string()) {
            // Handle function expressions for backward compatibility
            string func = it.value().get<string>();
            long long y;
            try {
                y = evaluateExpression(func);
            } catch (const exception& e) {
                cerr << "Error evaluating expression '" << func << "' in " << filename << ": " << e.what() << "\n";
                continue;
            }
            shares.push_back({(long double)x, (long double)y});
        } else {
            cerr << "Warning: Skipping invalid value format for key '" << key << "' in " << filename << "\n";
            continue;
        }
    }

    cout << "  Total shares: " << shares.size() << endl;
    if (shares.size() < (size_t)k) {
        cerr << "Error: Not enough shares for threshold k in " << filename << "\n";
        return -1;
    }

    sort(shares.begin(), shares.end(),
         [](auto& a, auto& b){ return a.first < b.first; });

    map<long long, vector<vector<int>>> combosMap;
    vector<int> indices(k);
    for (int i = 0; i < k; i++) indices[i] = i;

    cout << "  Processing combinations..." << endl;
    int comboCount = 0;
    while (true) {
        vector<pair<long double, long double>> subset;
        for (int idx : indices) {
            if (idx >= 0 && idx < (int)shares.size()) {
                subset.push_back(shares[idx]);
            }
        }

        if (subset.size() != (size_t)k) {
            cerr << "Error: Invalid subset size in combination for " << filename << "\n";
            return -1;
        }

        long double secValue;
        try {
            secValue = lagrangeInterpolation(subset);
        } catch (const exception& e) {
            cerr << "Error in Lagrange interpolation for " << filename << ": " << e.what() << "\n";
            // Continue with next combination instead of failing
            int i = k - 1;
            while (i >= 0 && indices[i] == (int)shares.size() - k + i) i--;
            if (i < 0) break;
            indices[i]++;
            for (int j = i + 1; j < k; j++) indices[j] = indices[j-1] + 1;
            continue;
        }
        
        long long sec = (long long)llround(secValue);
        combosMap[sec].push_back(indices);
        comboCount++;

        int i = k - 1;
        while (i >= 0 && indices[i] == (int)shares.size() - k + i) i--;
        if (i < 0) break;
        indices[i]++;
        for (int j = i + 1; j < k; j++) indices[j] = indices[j-1] + 1;
    }

    cout << "  Processed " << comboCount << " combinations" << endl;
    if (combosMap.empty()) {
        cerr << "Error: No valid combinations found in " << filename << "\n";
        return -1;
    }

    long long bestSecret = 0;
    size_t maxCount = 0;
    for (auto& entry : combosMap) {
        if (entry.second.size() > maxCount) {
            maxCount = entry.second.size();
            bestSecret = entry.first;
        }
    }

    cout << "  Most frequent secret: " << bestSecret << " (appears " << maxCount << " times)" << endl;
    return bestSecret;
}

int main() {
    vector<string> inputFiles = {"input1.json", "input2.json"};
    
    for (const string& filename : inputFiles) {
        cout << "Processing " << filename << ":" << endl;
        long long result = processFile(filename);
        if (result != -1) {
            cout << "Result: " << result << endl;
        } else {
            cout << "Failed to process " << filename << endl;
        }
        cout << "---" << endl;
    }
    
    return 0;
}
