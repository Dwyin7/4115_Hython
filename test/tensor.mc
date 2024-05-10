def int test_deep_nested_logic() {
    INT i = [1,2,3];
    INT j = [1,2,3];
    INT k = i * j;
    print(k[0]);
    print(k[1]);
    print(k[2]);
}

test_deep_nested_logic();
print(0);
