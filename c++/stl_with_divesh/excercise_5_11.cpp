#include <string>
#include <iostream>

using namespace std;

const string ascenders = "bdfhkl";
const string descenders = "gjpqy";

bool isAscOrDesc(string s){

}

int main(){
    string longest, ln;
    while(cin >> ln){
        if(!isAscOrDesc(ln) && ln.length() > longest.length())
            longest = ln;
    }

    cout << "Longest flat word: " << longest << endl;
    return 0;
}
