def int test_deep_nested_logic() {
    int i;
    i = 4;
    int acc;
    acc = 0;
    while (i != 0) {
        i = i - 1;
        int j;
        j = 3;
        while (j != 0) {
            j = j - 1;
            if (j != 2) {
                acc = acc + 3;
                int x;
                x = 2;
                while (x != 0) {
                    x = x - 1;
                    if (x != 1) {
                        acc = acc + 5;
                    } else {
                        acc = acc + 2;
                    }
                    print(acc);
                }
            } else {
                acc = acc + 1;
                int y;
                y = 3;
                while (y != 0) {
                    y = y - 1;
                    if (y != 2) {
                        acc = acc + 4;
                    } else {
                        acc = acc + 1;
                    }
                    print(acc);
                }
            }
            print(acc);
        }
        if (i != 3) {
            acc = acc + 10;
        } else {
            acc = acc - 5;
        }
        print(acc);
    }
    print(acc);
}

test_deep_nested_logic();
print(0);
