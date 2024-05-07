# Test import
import x;

# Global variables
int x;
x = 123;
int y;
y = x;
x = 10;

# Test scope in different symbol tables
char z = 's';
def void test_scope(char x){
    int z = 10;
    x = 'x';
}

# Test while
def int test_while(){
    int x = 3;
    x = x + 1;
    while(x > 0) {
        x = x - 1;
        int y = 0;
    }
    char y = 'y';
}

# Test for
def int test_for(){
    int i = 0;
    int j = 2;
    for(i in j){
        bool a = false;
        int i = 15;
    }
}

# Test tensor 
INT b = [[1,2,3],[1,2,3]];
INT a = [b,b];
b;
a;
int i = 0;
INT c = [1,2,3];
INT D = [[1,1,i],c];
FLOAT f = [[1.0,2.0,3.0],[1.0,2.0,3.0]];
FLOAT e = [[1.0,2.0],[1.0,2.0],[3.0,3.0]];
INT g = [[1],[2],[3]];
c@g;
e@f;


