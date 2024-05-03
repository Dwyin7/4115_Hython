bool c1;
c1 = true;

bool c2;
c2 = false;

def int test(){
    if(c1){
        if(c2){
            print(000);
        } else {
            print(9090);
        }
    } else {
        print(99999);
    }
}

test();
