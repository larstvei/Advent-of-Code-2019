include "globals.mzn";

int: low = 367479;
int: high = 893698;
bool: partB = false;

array [1..6] of var 1..9: pass;

var int: n = sum([pass[6-i]*pow(10, i) | i in 0..5]);

constraint low <= n;
constraint n <= high;
constraint increasing(pass);
constraint exists(i in 1..5)(pass[i] == pass[i+1]);

% part two

constraint partB -> (count(pass, 4, 2) \/
                     count(pass, 5, 2) \/
                     count(pass, 6, 2) \/
                     count(pass, 7, 2) \/
                     count(pass, 8, 2) \/
                     count(pass, 9, 2));
