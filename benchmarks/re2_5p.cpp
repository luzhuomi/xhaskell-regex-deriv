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
  string x;
  string y;
  string z;
  string w;
  string u;
  p = argv[1];
  s = "";
  
  RE2 re(p,RE2::POSIX);

  while (!infile.eof())
    { 
      getline(infile,s);
      
      if(RE2::FullMatch(s, re, &x, &y, &z, &w, &u)) {
	cout << x << ";" << y << ";" << z << ";" <<  w << ";" << u << ";" << endl; 
      } else {
	cout << "No matched" << endl;
      }
    }
  infile.close();
}
