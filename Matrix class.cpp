#include<iostream>
#include<time.h>
using namespace std;

class Matrix
{
	int row;
	int col;
	double **A;
	
	public:
		Matrix(int row,int col)
		{
			this->row = row;
			this->col = col;
			A = new double*[row];
			for(int i=0;i<row;i++) A[i] = new double[col];
		}
		bool isNull()
		{
			if(row==col && row==0) return true;
			return false;
		}
		void assignRandom(int lim)
		{
			srand(time(NULL));
			for(int i=0;i<row;i++)
			{
				for(int j=0;j<col;j++)
				{
					A[i][j] = rand()%(lim+1);
				}
			}
		}
		friend istream & operator>>(istream &is,Matrix &mat);
		friend ostream & operator<<(ostream &os,Matrix &mat);
		Matrix operator +(Matrix &mat);
		Matrix operator *(Matrix &mat);
		Matrix operator -(Matrix &mat);
		double det();
		Matrix inverse(int det);
//		~Matrix()
//		{
//			delete []A;
//		}
};

istream & operator>>(istream &is,Matrix &mat)
{
	cout<<"Enter the elements: "<<endl;
	for(int i=0;i<mat.row;i++)
	{
		for(int j=0;j<mat.col;j++)
		{
			cin>>mat.A[i][j];
		}
	}
	return is;
}

ostream & operator<<(ostream &os,Matrix &mat)
{
	cout<<"\nElements are: "<<endl;
	for(int i=0;i<mat.row;i++)
	{
		for(int j=0;j<mat.col;j++)
		{
			cout<<mat.A[i][j]<<"\t";
		}
		cout<<endl;
	}
	return os;
}

Matrix Matrix::operator +(Matrix &mat)
{
	if(!(row == mat.row && col == mat.col)) 
	{
		Matrix null(0,0);
		cout<<"\nMatrix can't be added, Matrix dimensions are not equal...";
		return null;
	}
	else
	{
		Matrix add(row,col);
		for(int i=0;i<row;i++)
		{
			for(int j=0;j<col;j++)
			{
				add.A[i][j] = 0;
				add.A[i][j] = A[i][j] + mat.A[i][j];
			}
		}
		return add;
	}
}

Matrix Matrix::operator -(Matrix &mat)
{
	if(!(row==mat.row && col==mat.col))
	{
		Matrix null(0,0);
		cout<<"\nMatrix substraction isn't possible, Matrix dimensions are unequal...";
		return null;	
	} 
	else
	{
		Matrix sub(row,col);
		for(int i=0;i<row;i++)
		{
			for(int j=0;j<col;j++)
			{
				sub.A[i][j] = 0;
				sub.A[i][j] = A[i][j] - mat.A[i][j];
			}
		}
		return sub;
	}
}

Matrix Matrix::operator *(Matrix &mat)
{
	if(col!=mat.row)
	{
		Matrix null(0,0);
		cout<<"\nMatrix multiplication is not possible, col1 != row2...";
		return null;
	}
	else
	{
		Matrix mul(row,mat.col);
		for(int i=0;i<row;i++)
		{
			for(int j=0;j<mat.col;j++)
			{
				mul.A[i][j] = 0;
				for(int k=0;k<col;k++)
				{
					mul.A[i][j] = mul.A[i][j] + A[i][k]*mat.A[k][j];
				} 
			}
		}
		return mul;
	}
}

double Matrix::det()
{
	if(row!=col)
	{
		cout<<"\nNot a square matrix, Determinant can't be found!..";
		return INT_MIN;	
	} 
	else
	{
		Matrix copy(row,col);
		for(int i=0;i<row;i++)
		{
			for(int j=0;j<col;j++)
			{
				copy.A[i][j] = A[i][j];
			}
		}
		for(int k=0;k<row-1;k++)
		{
			double x = A[k][k];
			for(int i=k+1;i<row;i++)
			{
				double y = A[i][k];
				for(int j=0;j<col;j++)
				{
					copy.A[i][j] = copy.A[i][j] - copy.A[k][j]*y/x;
				}
			}
		}
		double det = 1;
		for(int i=0;i<row;i++)
		{
			det = det*copy.A[i][i];
		}
		return det;
	}
}

Matrix Matrix::inverse(int det)
{
	Matrix null(0,0);
	if(row!=col)
	{
		cout<<"\nMatrix is not square.. inverse not possible!";
		return null;	
	} 
	else if(!det)
	{
		cout<<"\nDeterminant of matrix is 0, inverse not possible!";	
		return null;
	} 
	else
	{
		Matrix ident(row,col);
		Matrix copy(row,col);
		for(int i=0;i<row;i++)
		{
			for(int j=0;j<col;j++)
			{
				copy.A[i][j] = A[i][j]; 
				if(i==j) ident.A[i][j] = 1;
				else ident.A[i][j] = 0;
			}
		}
		
		for(int k=0;k<row-1;k++)
		{
			double x = copy.A[k][k];
			for(int i=k+1;i<row;i++)
			{
				double y = copy.A[i][k];
				for(int j=0;j<col;j++)
				{
					copy.A[i][j] = copy.A[i][j] - copy.A[k][j]*y/x;
					ident.A[i][j] = ident.A[i][j] - ident.A[k][j]*y/x;
				}
			}
		}
		
		for(int k=row-1;k>0;k--)
		{
			double x = copy.A[k][k];
			for(int i=0;i<k;i++)
			{
				double y = copy.A[i][k];
				for(int j=0;j<col;j++)
				{
					copy.A[i][j] = copy.A[i][j] - copy.A[k][j]*y/x;
					ident.A[i][j] = ident.A[i][j] - ident.A[k][j]*y/x;
				}
			}
		}
		
		for(int i=0;i<row;i++)
		{
			double x = copy.A[i][i];
			for(int j=0;j<col;j++)
			{
				copy.A[i][j] = copy.A[i][j]/x;
				ident.A[i][j] = ident.A[i][j]/x;
			}
		}
		return ident;
	}
}

int main()
{
	int userIn,row1,col1,row2,col2,lim1,lim2;
	double det1,det2;
	cout<<"MENU DRIVEN PROGRAM FOR   M A T R I X";
	cout<<"\n\nEnter the dimensions of 1st matrix...";
	cout<<"\nRow = ";
	cin>>row1;
	cout<<"Column = ";
	cin>>col1;
	cout<<"\n\nEnter the dimensions of 2nd matrix...";
	cout<<"\nRow = ";
	cin>>row2;
	cout<<"\nColumn = ";
	cin>>col2;
	Matrix m1(row1,col1);
	Matrix m2(row2,col2);
	system("CLS");
	do
	{
		cout<<"Press: \n1 to enter elements randomly"<<endl;
		cout<<"2 to enter elements manually"<<endl;
		cin>>userIn;
		switch(userIn)
		{
			case 1: cout<<"\nEnter max limit for matrix 1: ";
					cin>>lim1;
					cout<<"\nEnter max limit for matrix 2: ";
					cin>>lim2;
					m1.assignRandom(lim1);
					m2.assignRandom(lim2);
					break;
			case 2:	cout<<"\nEnter the elements of 1st matrix: ";
					cin>>m1;
					cout<<"\n\nEnter the elements of 2nd matrix: ";
					cin>>m2;
					break;
		}
	}while(!(userIn==1||userIn==2));
	Matrix add = m1+m2;
	Matrix sub = m1-m2;
	Matrix mul = m1*m2;
	det1 = m1.det(); if(det1==INT_MIN) cout<<" in matrix 1";
	det2 = m2.det(); if(det2==INT_MIN) cout<<" in matrix 2";
	Matrix inv1 = m1.inverse(det1); if(inv1.isNull()) cout<<" in matrix 1";
	Matrix inv2 = m2.inverse(det2); if(inv2.isNull()) cout<<" in matrix 2";
	Matrix back1 = m1*inv1;
	Matrix back2 = m2*inv2;
	cout<<"\n\nPress any key to continue...";
	cin>>userIn;
	do
	{
		system("CLS");
		cout<<"Press: "<<endl;
							cout<<"1 to display both matrix."<<endl;
		if(!add.isNull()) 	cout<<"2 to add two matrices."<<endl;
		if(!sub.isNull()) 	cout<<"3 to substract two matrices."<<endl;
		if(!mul.isNull())	cout<<"4 to multiply two matrices."<<endl;
		if(!(det1==INT_MIN && det2==INT_MIN)) cout<<"5 to find determinant of a matrix, "; 
		if(det1!=INT_MIN) cout<<" 1"; if(det2!=INT_MIN) cout<<" 2"; cout<<endl;					
		if(!(inv1.isNull()&&inv2.isNull()))	  cout<<"6 to find the inverse of a matrix,";
		if(!inv1.isNull()) cout<<" 1"; if(!inv2.isNull()) cout<<" 2\n"; cout<<endl;
		cin>>userIn;
		switch(userIn)
		{
			system("CLS");
			case 1:		cout<<"1st Matrix"<<endl;
						cout<<m1;
						cout<<"\n\n2nd Matrix"<<endl;
						cout<<m2;
						break;
			case 2:		if(!add.isNull())
						{
							cout<<"1st Matrix"<<endl;
							cout<<m1;
							cout<<"\n\n2nd Matrix"<<endl;
							cout<<m2;
							cout<<"\n\nMatrix addition"<<endl;
							cout<<add;	
						}
						break;
			case 3:		if(!sub.isNull())
						{
							cout<<"1st Matrix"<<endl;
							cout<<m1;
							cout<<"\n\n2nd Matrix"<<endl;
							cout<<m2;
							cout<<"\n\nMatrix substraction"<<endl;
							cout<<sub;	
						}
						break;
			case 4:		if(!mul.isNull())
						{
							cout<<"1st Matrix"<<endl;
							cout<<m1;
							cout<<"\n\n2nd Matrix"<<endl;
							cout<<m2;
							cout<<"\n\nMatrix multiplication"<<endl;
							cout<<mul;
						}
						break;
			case 5:		cout<<"1st Matrix"<<endl;
						cout<<m1;
						cout<<"\n\n2nd Matrix"<<endl;
						cout<<m2;
						if(det1!=INT_MIN) cout<<"\n\nDeterminant of 1st matrix = "<<det1;
						if(det2!=INT_MIN) cout<<"\n\nDeterminant of 2nd matrix = "<<det2<<endl; 
						break;
			case 6:		cout<<"1st Matrix"<<endl;
						cout<<m1;
						cout<<"\n\n2nd Matrix"<<endl;
						cout<<m2;
						if(det1&&det1!=INT_MIN)
						{
							cout<<"\n\nInverse of 1st matrix"<<endl;
							cout<<inv1;
							cout<<"\n\nMultiplication with inverse matrix generates matrix"<<endl;
							cout<<back1;
						}
						if(det2&&det2!=INT_MIN)
						{
							cout<<"\n\nInverse of 2nd matrix"<<endl;
							cout<<inv2;
							cout<<"\n\nMultiplication with inverse matrix generates matrix"<<endl;
							cout<<back2;
						}
						break;
			default:	cout<<"Invalid selection...";
						break;
		}
		cout<<"\n\nPress 1 to continue and any other key to exit...";
		cin>>userIn;
		if(userIn!=1) userIn = 0;
	}while(userIn);
	return 0;
}
