#include <re2/re2.h>
#include <stdio.h>
#include <fstream>
#include <iostream>
#include <string>

using namespace re2;
using namespace std;

int main(int argc, char* argv[]) {
  ifstream infile;
  infile.open(argv[2]);
  
  string s("");
  string p("");
  string x0;
  string x1;
  string x2;
  string x3;
  string x4;
  string x5;
  string x6;
  string x7;
  string x8;
  string x9;
  string x10;
  p = argv[1];
  s = "";
  
  RE2 re(p,RE2::POSIX);

  while (!infile.eof())
    { 
      getline(infile,s);
      
      if(RE2::FullMatch(s, re, &x0, &x1, &x2, &x3, &x4, &x5, &x6, &x7, &x8, &x9, &x10)) {
	cout << x0 << ";" << x1 << ";" << x2 << ";" << x3 << ";" << x4 << ";" << x5 << ";" << x6 << ";" << x7 << ";" << x8 << ";" << x9 << ";" << x10 << ";" << endl; 
      } else {
	cout << "No matched" << endl;
      }
    }
  infile.close();
}
