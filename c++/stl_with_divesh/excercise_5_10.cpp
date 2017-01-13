#include <string>
#include <vector>
#include <iostream>

using namespace std;

void readAllWords(vector<string>& words){
    string t;
    while(cin >> t)
        words.push_back(t);
}

bool isPalindrome(string s){
    int l, r;
    for(l = 0, r = s.length() - 1; l <= r; l++, r--){
        if(s[l] != s[r]) return false;
    }
    return true;
}

int main(){
    vector<string> wds;
    readAllWords(wds);
    string maxPali = "";
    typedef std::vector<string>::iterator Iter;
    for(Iter it = wds.begin(); it != wds.end(); it++){
        if(isPalindrome(*it)){
            cout << "PALINDROME: " << *it << endl;
            if(it->length() > maxPali.length()){
                maxPali = *it;
            }
        }
    }
    cout << "MAX PALINDROME: " << maxPali << endl;
}
