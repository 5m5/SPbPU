#include <iostream>
#include <string>
#include <fstream>
#include <streambuf>
#include <regex>

using namespace std;

void replaceSymbols(string &str) {
    auto position = 0;

    for(int i = 0; i < str.size(); ++i) {
        position = str.find_first_of("\a\b\f\n\r\t\v");
        if(position != string::npos)
            str.replace(position, 1, " ");
    }
}

void removeSpaces(string &str) {
    for(auto itr = str.begin(); itr != str.end(); ++itr) {
        auto begin = itr;
        while(itr != str.end() && *itr == ' ')
            ++itr;
        if(itr - begin > 1)
            itr = str.erase(begin + 1, itr) - 1;
        if(itr == str.end())
            break;
    }
}

void checkPoints(string &str) {
    for(int i = 0; i < str.size(); ++i) {
        int pos = str.find_last_of(".,?!:;", i);
        if(pos != string::npos) {
            auto itr = str.begin() + pos;
            if(*--itr == ' ')
                str.erase(itr);
        }

        pos = str.find_last_of(".,?!:;", i);
        
        if(pos != string::npos) {
            string::iterator itr = str.begin() + pos;
            if(*++itr != ' ')
                str.insert(itr, ' ');
        }
    }  
}

void replaceWord(string &str) {
    regex e("([a-zA-Z-]){10,}");
    string tmp = str;
    str = regex_replace(tmp, e, "Vau!!!");
}

void splitString(string &str) {
    for(int i = 40; i < str.size(); i += 40) {
        while(!isspace(str.at(i)) && i > 0)
            --i;
        if(i <= 0)
            break;
        str.replace(i, 1, 1, '\n');
        ++i;
    }
}

int main()
{
    ifstream t("data.txt");
    string str((std::istreambuf_iterator<char>(t)),
                std::istreambuf_iterator<char>());

    replaceSymbols(str);
    removeSpaces(str);
    checkPoints(str);
    replaceWord(str);
    splitString(str);
    cout << str;

    return 0;
}
