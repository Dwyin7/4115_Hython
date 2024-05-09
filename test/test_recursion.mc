



def int factorial(int n){
    # Base case: factorial 1 is 1
    if(n == 1){
        return 1;
    } else {
        # Recursive case: n * factorial of (n-1)
        return n * factorial(n - 1);
    }
}


def int fibonacci(int n){
    # Base cases: fibonacci of 0 is 0, of 1 is 1
    if(n == 0){
        return 0;
    } else{
        if(n == 1){
            return 1;
        } else {
            # Recursive case: sum of the two preceding numbers
            int fst = fibonacci(n - 1);
            int snd = fibonacci(n - 2);
            return fst + snd;
        }
    }
}

def int test_rec(int n){
    if(n == 0){
        return 1;
    } else{
        int x = factorial(n);
        int y = test_rec(n-1);
        return x + y;
    }
    
}


print(test_rec(10));