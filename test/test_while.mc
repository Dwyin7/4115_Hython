def int test_while_loop() {
    int i;
    i = 10;
    int acc;
    acc = 0;
    while(i != 0){
        i = i - 1;
        int j;
        j = 10;
        while(j != 0){
            j = j - 1;
            acc = acc + 1;
            print(acc);
        }
    }
    print(acc);
}

test_while_loop();
print(0);
