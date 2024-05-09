def bool test_neq(int a, int b){
    if(a != b){
        return true;
    } else {
        return false;
    }
}

def bool test_geq(int a, int b){
    if(a >= b){  
        return true;
    } else {
        return false;
    }
}

def bool test_less(int a, int b){
    if(a < b){
        return true;
    } else {
        return false;
    }
}

def bool test_leq(int a, int b){
    if(a <= b){  
        return true;
    } else {
        return false;
    }
}



print(test_neq(5, 5));  # Expected: false
print(test_neq(5, 4));  # Expected: true

print(test_geq(5, 5));  # Expected: true
print(test_geq(4, 5));  # Expected: false

print(test_less(4, 5));  # Expected: true
print(test_less(5, 4));  # Expected: false

print(test_leq(5, 5));  # Expected: true
print(test_leq(5, 6));  # Expected: true
