// 单纯形.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"
#include<fstream>
#define f0 0.0000001

double Main_Matrix[100][100], tar[100];
int var, cons;
int base[100];
void read_file()
{
	int i, j;
	char Type[4];
	int symb[100], var_symb[100];
	memset(Main_Matrix, 0.0, sizeof(Main_Matrix));
	memset(tar, 0.0, sizeof(tar));
	memset(base, -1, sizeof(base));
	ifstream ReadFile;
	ReadFile.open("input.txt");
	ReadFile >> Type;
	ReadFile >> var;
	ReadFile >> cons;

	while (!ReadFile.eof())
	{
		//目标函数系数
		if (Type[1] == 'i'&&Type[2] == 'n')
		{
			for (i = 0; i < var + 1; i++)
			{//目标函数第一个数为常数
				ReadFile >> tar[i];
				tar[i] *= -1;
			}
		}
		else
		{
			for (i = 0; i < var + 1; i++)
				ReadFile >> tar[i];
		}
		//主矩阵
		for (j = 0; j < cons; j++) //第一个是等号右边的系数,数量要加1
		{
			for (i = 0; i < var + 1; i++)
			{
				ReadFile >> Main_Matrix[i][j];
			}
		}

		for (i = 0; i < cons; i++)
			ReadFile >> symb[i];   //约束符号＝0；＞2 ＜ 1
		for (i = 0; i < var; i++)
			ReadFile >> var_symb[i];//约束符号 无0；＞2 ＜ 1
		ReadFile.close();


		//处理变量系数为负或为无的情况
		for (i = 0; i < var; i++)
		{
			switch (var_symb[i])
			{
			case 0:
			{
					  tar[var + 1] = -tar[i + 1]; //无约束则用x`-x``，系数只需要拷贝过去，增加一个变量
					  for (j = 0; j < cons; j++)
					  {
						  Main_Matrix[var + 1][j] = -Main_Matrix[i + 1][j]; //把增加的变量系数取负
					  }
					  var_symb[var] = 2; //把符号变成大于
					  var_symb[i] = 2;
					  var++; //变量数加一
					  break;
			}
			case 1:
			{
					  tar[i + 1] *= -1; //把目标函数的那个变量取反
					  for (j = 0; j < cons; j++)
					  {
						  Main_Matrix[i + 1][j] *= -1; //这里所有变量系数取反
					  }
					  var_symb[i] = 2; //变符号
					  break;
			}
			}

		}

		//松弛

		for (i = 0; i < cons; i++)
		{
			switch (symb[i])
			{
			case 0: //处理约束相等的情况
			{
						for (j = 0; j < var + 1; j++)
							Main_Matrix[j][cons] = Main_Matrix[j][i]; //增加一个，并且约束符号相反
						//对增加的等式与原等式进行松弛
						//原式默认大于
						Main_Matrix[var + 1][i] = 1; //右边减一个松弛变量，移过去为正
						base[i] = var;
						var_symb[var] = 2;
						var++;
						for (j = 0; j < var + 1; j++)
							Main_Matrix[j][cons] *= -1; //取反，令不等号反向
						Main_Matrix[var + 1][cons] = 1; //加一个松弛变量，移过去为负
						var_symb[var] = 2;
						base[cons] = var;
						var++;
						cons++;
						break;
			}
			case 2: //小于零
			{
						for (j = 0; j < var + 1; j++)
							Main_Matrix[j][cons] *= -1; //取反，令不等号反向
						Main_Matrix[var + 1][i] = 1;    //加一个松弛变量，移过去为负
						var_symb[var] = 2;
						base[i] = var;
						var++;
						break;
			}
			case 1: //大于零
			{
						Main_Matrix[var + 1][i] = 1; //加一个松弛变量，移过去为负
						var_symb[var] = 2;
						base[i] = var;
						var++;
						break;
			}
			}
		}

	}

}

int get_min(int s)
{
	int i, mincon = 0;
	double j = 100000000;
	for (i = 0; i < cons; i++)
	{
		if (Main_Matrix[0][i] * Main_Matrix[s][i] < f0 && (-1 * Main_Matrix[0][i] / Main_Matrix[s][i]) < j-f0)
		{
			j = -1 * Main_Matrix[0][i] / Main_Matrix[s][i];
			mincon = i;
		}
	}
	return mincon;
}

int comb(int a, int b)
{
	int temp = 1,k;
	for (k = 1; k <= b; k++)
		temp = (temp*(a - b + k)) / k;
	return temp;
}

void solve()
{
	int i, j, k, u, Cycle = 0, flag = 0;
	double temp = 0, Prebase, Pretar;
	int n = 100;

	for (i = 0; i < cons; i++)
		Main_Matrix[0][i] *= -1; //b设置为负数

	while (n > 0)
	{
		flag = 0;
		Cycle++;
		n = 0;
		for (i = 1; i < var + 1; i++)
		{
			if (tar[i] > f0)
				n++;
		}

		for (i = 1; i < var + 1; i++)
		{
			if (tar[i] > f0)
			{

				flag = 1;
				k = get_min(i); //获取约束最紧的那一行
				base[k] = i - 1; //把这一行基变量变成目标非负的那个

				for (j = 0; j < var + 1; j++)
				{
					if (j != base[k] + 1)
						Main_Matrix[j][k] /= Main_Matrix[base[k] + 1][k]; //处以系数，使得基变量系数为1
				}



				//消元
				for (j = 0; j < cons; j++)
				{
					Prebase = Main_Matrix[base[k] + 1][j];
					if (j != k)
					{
						for (u = 0; u < var + 1; u++)
							Main_Matrix[u][j] -= Prebase* Main_Matrix[u][k];
					}

				}

				//处理目标函数

				Pretar = tar[base[k] + 1];
				for (u = 0; u < var + 1; u++)
					tar[u] -= Pretar*Main_Matrix[u][k];
			}
			if (flag == 1)
				break;
		}
		if (Cycle>comb(cons + var, cons))//数据量大时这一句计算很耗时，可定义变量优化不必每次计算
			break;
	}
}

int main()
{


	read_file();
	
	solve();
	for (int i = 0; i < var + 1; i++)
		cout << tar[i] << "  ";
	cout << endl;
	for (int j = 0; j < cons; j++) //第一个是等号右边的系数,数量要加1
	{
		for (int i = 0; i < var + 1; i++)
		{
			cout << Main_Matrix[i][j] << "  ";
		}
		cout << endl;
	}
	for (int i = 0; i < var; i++)
		cout << base[i] << "  ";
	cout << endl;
	system("pause");
	return 0;
}
//2 2 7 1 1 4 1 -2 0 2 2 0 1 0 -2 3

