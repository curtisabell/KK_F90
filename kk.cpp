#include <iostream>
#include <cmath>
#include <string>
#include <cstdlib>
#include <fstream>
#include <stdio.h>
#include <ctime>
using namespace std;

string *arrRandomise(string str_arr[], int length)
{
  srand(time(NULL));
  string *new_arr = new string[length];
  int count = 0;
  int r_num;
  while (count < length)
    {
      r_num = rand() % length;
      // cout << r_num;
      if (str_arr[r_num] != "null")
	{
	  new_arr[count] = str_arr[r_num];
	  str_arr[r_num] = "null";
	  count++;
	}
    }
  return new_arr;
}

bool isUnique(string arr_old[], string arr_new[], int lenOld, int lenNew)
{
  int i, j, ip1, jp1;
  bool output = 1;
  string nN, nNp1, nO, nOp1;
  for (i=0; i<lenNew; i++)
    {
      ip1 = i + 1;
      if (ip1 == lenNew)
	ip1 = 0;
      nN = arr_new[i];
      nNp1 = arr_new[ip1];

      for (j=0; j<lenOld; j++)
	{
	  jp1 = j + 1;
	  if (jp1 == lenOld)
	    jp1 = 0;
	  nO = arr_old[j];
	  nOp1 = arr_old[jp1];

	  cout << nO << " help";
	  // cout << nN << " " << nNp1 << endl;

	  if ((nO == nN) && (nOp1 == nNp1))
	    output = 0;
	}
    }
  return output;
}

string *fileRead(int len, string inStr)
{
  ifstream myfile (inStr.c_str());
  int i = 0;
  string *names_arr = new string[len];
  string tempStr;
  while (!myfile.eof())
    {
      getline(myfile, tempStr);
      if (i < len) names_arr[i] = tempStr;
      // cout << i << names_arr[i] << endl;
      i++;
    }
  return names_arr;
}

int numberOfLines(string inStr)
{
  int number = 0;
  string line;
  ifstream myfile (inStr.c_str());
  while (!myfile.eof())
    {
      getline(myfile,line);
      number++;
      // cout << line << number << endl;
    }
  number--;
  return number;
}

void fileWrite(string str_arr[], int length)
{
  ofstream myfile ("new_names");
  for (int i=0; i<length; i++)
    {
      if (i == length-1)
	myfile << str_arr[i];
      else
	myfile << str_arr[i] << "\n";
    }
}

int main()
{
  bool unique = 0, unique16, unique17;
  string *arrout, *arrin, *arrin16, *arrin17;
  int counter = 0;
  int len   = numberOfLines("names");
  int len16 = numberOfLines("kk_list_16");
  int len17 = numberOfLines("kk_list_17");

  // cout << len << len16 << len17 << endl;
  arrin16 = fileRead(len16, "kk_list_16");
  arrin17 = fileRead(len17, "kk_list_17");

  // unique = 1;
  while (!unique && (counter < 20))
    {
      // if ((counter % 100) == 0) cout << counter << endl;
      // if ((counter % 100) == 0) printf("%d \n", counter);
      arrin = fileRead(len, "names");
      arrout = arrRandomise(arrin, len);
      // arrout[2] = "Sam";
      // arrout[3] = "David";
      unique16 = isUnique(arrin16, arrout, len16, len);
      unique17 = isUnique(arrin17, arrout, len17, len);
      unique = unique16 && unique17;
      counter++;
      printf("count : %d \n", counter);
      cout << "isUnique: " << unique << endl;
      // cout << counter << endl;
    }
  fileWrite(arrout,len);
  cout << endl;

  delete[] arrout;
  delete[] arrin;
  delete[] arrin16;
  delete[] arrin17;
  return 0;
}
