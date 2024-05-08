def int test_call_1 () {
    int i = 1;
}

def int test_call_2 () {
    int call1 = test_call_1();
    int res = call1 + 2;
}

def float test_call_3 () {
    int call2 = test_call_2();
    int res = call2 + 3;
}

def void test_call_4() {
    int i;
}

test_call_1();
test_call_2();
test_call_3();
test_call_4();


