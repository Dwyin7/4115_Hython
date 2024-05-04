def int test_deep_nested() {
    if (true) {
        print(100);
        if (false) {
            print(200);
        } else {
            print(300);
            if (true) {
                print(400);
                if (false) {
                    print(500);
                } else {
                    print(600);
                    if (true) {
                        print(700);
                    } else {
                        print(800);
                    }
                    #if_end5
                    print(900);
                }
                #if_end4
                print(1000);
            } else {
                print(1100);
            }
            #if_end3
            print(1200);
        }
        #if_end2
        print(1300);
    } else {
        print(1400);
        if (true) {
            print(1500);
        } else {
            if (false) {
                print(1600);
            } else {
                print(1700);
            }
            #if_end8
        }
        #if_end7
        print(1800);
    }
    #if_end1
    print(1900);
}

test_deep_nested();
test_deep_nested();
print(0);
