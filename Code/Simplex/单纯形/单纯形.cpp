// ������.cpp : �������̨Ӧ�ó������ڵ㡣
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
		//Ŀ�꺯��ϵ��
		if (Type[1] == 'i'&&Type[2] == 'n')
		{
			for (i = 0; i < var + 1; i++)
			{//Ŀ�꺯����һ����Ϊ����
				ReadFile >> tar[i];
				tar[i] *= -1;
			}
		}
		else
		{
			for (i = 0; i < var + 1; i++)
				ReadFile >> tar[i];
		}
		//������
		for (j = 0; j < cons; j++) //��һ���ǵȺ��ұߵ�ϵ��,����Ҫ��1
		{
			for (i = 0; i < var + 1; i++)
			{
				ReadFile >> Main_Matrix[i][j];
			}
		}

		for (i = 0; i < cons; i++)
			ReadFile >> symb[i];   //Լ�����ţ�0����2 �� 1
		for (i = 0; i < var; i++)
			ReadFile >> var_symb[i];//Լ������ ��0����2 �� 1
		ReadFile.close();


		//�������ϵ��Ϊ����Ϊ�޵����
		for (i = 0; i < var; i++)
		{
			switch (var_symb[i])
			{
			case 0:
			{
					  tar[var + 1] = -tar[i + 1]; //��Լ������x`-x``��ϵ��ֻ��Ҫ������ȥ������һ������
					  for (j = 0; j < cons; j++)
					  {
						  Main_Matrix[var + 1][j] = -Main_Matrix[i + 1][j]; //�����ӵı���ϵ��ȡ��
					  }
					  var_symb[var] = 2; //�ѷ��ű�ɴ���
					  var_symb[i] = 2;
					  var++; //��������һ
					  break;
			}
			case 1:
			{
					  tar[i + 1] *= -1; //��Ŀ�꺯�����Ǹ�����ȡ��
					  for (j = 0; j < cons; j++)
					  {
						  Main_Matrix[i + 1][j] *= -1; //�������б���ϵ��ȡ��
					  }
					  var_symb[i] = 2; //�����
					  break;
			}
			}

		}

		//�ɳ�

		for (i = 0; i < cons; i++)
		{
			switch (symb[i])
			{
			case 0: //����Լ����ȵ����
			{
						for (j = 0; j < var + 1; j++)
							Main_Matrix[j][cons] = Main_Matrix[j][i]; //����һ��������Լ�������෴
						//�����ӵĵ�ʽ��ԭ��ʽ�����ɳ�
						//ԭʽĬ�ϴ���
						Main_Matrix[var + 1][i] = 1; //�ұ߼�һ���ɳڱ������ƹ�ȥΪ��
						base[i] = var;
						var_symb[var] = 2;
						var++;
						for (j = 0; j < var + 1; j++)
							Main_Matrix[j][cons] *= -1; //ȡ������Ⱥŷ���
						Main_Matrix[var + 1][cons] = 1; //��һ���ɳڱ������ƹ�ȥΪ��
						var_symb[var] = 2;
						base[cons] = var;
						var++;
						cons++;
						break;
			}
			case 2: //С����
			{
						for (j = 0; j < var + 1; j++)
							Main_Matrix[j][cons] *= -1; //ȡ������Ⱥŷ���
						Main_Matrix[var + 1][i] = 1;    //��һ���ɳڱ������ƹ�ȥΪ��
						var_symb[var] = 2;
						base[i] = var;
						var++;
						break;
			}
			case 1: //������
			{
						Main_Matrix[var + 1][i] = 1; //��һ���ɳڱ������ƹ�ȥΪ��
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
		Main_Matrix[0][i] *= -1; //b����Ϊ����

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
				k = get_min(i); //��ȡԼ���������һ��
				base[k] = i - 1; //����һ�л��������Ŀ��Ǹ����Ǹ�

				for (j = 0; j < var + 1; j++)
				{
					if (j != base[k] + 1)
						Main_Matrix[j][k] /= Main_Matrix[base[k] + 1][k]; //����ϵ����ʹ�û�����ϵ��Ϊ1
				}



				//��Ԫ
				for (j = 0; j < cons; j++)
				{
					Prebase = Main_Matrix[base[k] + 1][j];
					if (j != k)
					{
						for (u = 0; u < var + 1; u++)
							Main_Matrix[u][j] -= Prebase* Main_Matrix[u][k];
					}

				}

				//����Ŀ�꺯��

				Pretar = tar[base[k] + 1];
				for (u = 0; u < var + 1; u++)
					tar[u] -= Pretar*Main_Matrix[u][k];
			}
			if (flag == 1)
				break;
		}
		if (Cycle>comb(cons + var, cons))//��������ʱ��һ�����ܺ�ʱ���ɶ�������Ż�����ÿ�μ���
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
	for (int j = 0; j < cons; j++) //��һ���ǵȺ��ұߵ�ϵ��,����Ҫ��1
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

