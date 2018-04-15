#include "StringTools.hpp"


template<typename Out>
void split(const std::string &s, char delim, Out result) {
    std::stringstream ss(s);
    std::string item;
    while (std::getline(ss, item, delim))
    {
        if( !item.empty() )
            *(result++) = item;
    }
}

std::vector<std::string> split(const std::string &s, char delim) {
    std::vector<std::string> elems;
    split(s, delim, std::back_inserter(elems));
    return elems;
}


std::string trimStringzeros(std::string str)
{
    for(std::string::size_type s=str.length()-1; s>0; --s)
    {
        if(str[s] == '0' || str[s] == '.' )
            str.erase(s,1);
        else
            break;
    }
    return str;
}

