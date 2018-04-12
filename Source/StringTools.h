#pragma once

#include <iostream>
#include <sstream>
#include <string>
#include <typeinfo>
#include <stdexcept>

/**
 *  some helper functions for parsing strings
 */

template<typename Out>
void split(const std::string &s, char delim, Out result) {
    std::stringstream ss(s);
    std::string item;
    while (std::getline(ss, item, delim)) {
        *(result++) = item;
    }
}

std::vector<std::string> split(const std::string &s, char delim) {
    std::vector<std::string> elems;
    split(s, delim, std::back_inserter(elems));
    return elems;
}

/*
// copied from https://isocpp.org/wiki/faq/misc-technical-issues#convert-string-to-num

class BadConversion : public std::runtime_error
{
public:
    BadConversion(const std::string& s)
    : std::runtime_error(s)
    { }
};

template<typename T>
inline void convert(const std::string& s, T& x,
                    bool failIfLeftoverChars = true)
{
    std::istringstream i(s);
    char c;
    if (!(i >> x) || (failIfLeftoverChars && i.get(c)))
        throw BadConversion(s);
}

template<typename T>
inline T convertTo(const std::string& s,
                   bool failIfLeftoverChars = true)
{
    T x;
    convert(s, x, failIfLeftoverChars);
    return x;
}

template<typename T>
inline std::string stringify(const T& x)
{
    std::ostringstream o;
    if (!(o << x))
        throw BadConversion(std::string("stringify(") + typeid(x).name() + ")");
    return o.str();
}

*/
