
int x;
x = 1;
def int test_recursion(int a){
    if(a!=1){
        return a * test_recursion(a-1);
    }else{
        return a;
    }
}

x = test_recursion(10);
print(x);