#include <string>
#include <vector>
#include <iostream>
#include <cctype>

using namespace std;

template<class Iter>
bool allIsAlpha(Iter start, Iter end){
    for(; start != end; start++){
        if(!isalpha(*start)) return false;
    }
    return true;
}

int main(){
    cout << "Enter strings followed by the empty string" << endl;
    vector<string> uppers;
    vector<string> lowers; //lower first
    string temp;
    while(cin >> temp){
        if(!allIsAlpha(temp.begin(), temp.end())){
            cout << "Invalid!" << endl;
        }else if(islower(temp[0]))
            lowers.push_back(temp);
        else
            uppers.push_back(temp);
    }

    cout << "Uppers:" << endl;
    for(string u: uppers){
        cout << u << endl;
    }

    cout << "Lowers:" << endl;
    for(string u: lowers){
        cout << u << endl;
    }

}
