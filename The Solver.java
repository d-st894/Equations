/**
* This program is an n-dimensional equation-solver, i.e., it can solve n equations with n variables, n being any natural number input
* by the user. It uses Matrices and the process of Gaussian Elimination to factor the Equation Matrix entered by the user into an Upper
* Triangular Matrix, which is significantly easier to work with.
* After converting to a triangular Matrix, it uses some basic algebra to obtain the values of the varibles in this simplified system and
* prints them.
* The thing to note that it can solve a system, where n can have any reasonable value, with a time complexity of O(n^3), withput having to
* to actually compute the inverse of the equation matrix.
* In case the entered matrix is singular, an appropriate message is displayed.
*/
import java.util.Scanner;
class Equations
{
    double[][] RExc(double[][] A, int m, int n)
    {
        
        /**
         * Method to exchange the m(th)and n(th) rows of a matrix and then return the altered matrix. 
         * @param double[][] A : The Matrix whose rows are to be altered.
         * @param int m : The row number that is to be moved.
         * @param int n : The row number that is to replace the above row and whose place the above row will take.
         * @return double[][] A :The input matrix after the operation.
         */
        double a=0;
        for(int i=0;i<A[0].length;i++)
        {
            a=A[m][i];
            A[m][i]=A[n][i];
            A[n][i]=a;
        }
        return (A);
    }
    double[][] RSub(double[][] A, double x, int m, int n)
    {
        /**
         * Method to subtract a multiple of one row of a matrix from one row of a matrix from another row of the same matrix and then
         * the resulting matrix.
         * @param double[][] A : The matrix whose rows are to be subtracted from one another.
         * @param int x : The multiplying factor of the row to be subtracted.
         * @param int m : The row whose multiple is to be subtracted.
         * @param int n : The row that is to be subtracted from.
         * @return double[][] A : The original matrix after performing the desired subtraction.
         */
        for(int i=0;i<A[0].length;i++)
            A[n][i]=A[n][i]-x*A[m][i];
        return (A);
    }
    double[][] CheckNChange(double[][] A, int m, int n)
    {                                                 
        /**
         * Method to check that all the points on the main diagonal are non-zero. If they are not, then it uses Row Exchanges to make
         * every entry of the form A[i][i] non-zero.
         * @param double[][] A : The matrix whose main diagonal points are to be made non-zero.
         * @param int m : The row number of the element that is to checked and possibly exchanged.
         * @param int n : The coulumn number of the element that is to checked and possibly exchanged.
         * @return double[][] A : The original matrix after making the main diagonal non zero.
         */
        if(A[m][n]!=0)
        {}
        else
        {
            int i;
            for(i=m;i<A.length;i++)
            {
                if(A[i][n]!=0)
                {
                    A=RExc(A,m,i);
                    break;
                }
            }
        }
        return(A);
    }
    double Determinant(double[][] A)
    {
        /**
         * Recursive Method t find the determintn of a square matrix.
         * @param double[][] A : The matrix whose determinan is to be found.
         * @param double det : The determinant of the given square matrix.
         */
        double temp[][];
        double det=0.0;
        int i,j,k;
        if(A.length==2)
            return(A[0][0]*A[1][1]-A[0][1]*A[1][0]);
        for(i=0;i<A[0].length;i++) 
        {
            temp=new double[A.length-1][A[0].length-1];
            for(j=1;j<A.length;j++)
            {
                for(k=0;k<A[0].length;k++)
                {
                    if(k<i)
                        temp[j-1][k]=A[j][k];
                    else if(k>i)
                        temp[j-1][k-1]=A[j][k];
                }
            }
            det+=A[0][i]*Math.pow(-1,(double)i)*Determinant(temp);
        }
        return (det);
    }
    boolean isSingular(double[][] A)
    {
        /**
         * Method to check if the determinant of a matrix is zero, i.e., if the matrix s singular or not.
         * @param double[][] A : The matrix whose singularity is to checked.
         * @return boolean c : It is ture if A is singular, false otherwise.
         */
        if(Determinant(A)==0)
            return true;
        return false;
    }
    double[][] Elimination(double[][] A)/**Method to perform Gaussian Elimination on the Equation Matrix to convert it into an Upper-Triangular Matrix.*/  
    {
        /**
         * Method to perform Gaussian Elimination on the Equation Matrix to convert it into an Upper-Triangular Matrix.
         * @param double[][] A : The matrix who is to be made Upper-Triangular.
         * @return double[][] A : The matrix after Gaussian Elimination. 
         */    
        int n=A.length;
        int i,j;
        for(i=0;i<n;i++)
        {
            if(A[i][i]==0)
                A=CheckNChange(A,i,i);
            for(j=i+1;j<n;j++)
            {
                double m=A[j][i]/A[i][i];
                A=RSub(A,m,i,j);
            }
        }
        return (A);
    }
    void Solutions(double[][] A, double[] b)
    {
        /**
         * Method to finally calculate the solutions and then print them.
         * @param double[][] A : The equation matrix after Gaussian Elimination.
         * @param double[] b : The result vector after alteration.
         */
        int n=A.length;
        int i,j;
        double[] x=new double[n];
        x[n-1]=b[n-1]/A[n-1][n-1];
        for(i=(n-2);i>=0;i--)
        {
            x[i]=b[i];
            for(j=i+1;j<n;j++)
                x[i]-=A[i][j]*x[j];
            x[i]/=A[i][i];
            
        }
        System.out.println("The solutions  to the system of equations are:");
        for(i=0;i<n;i++)
            System.out.println("x"+(i+1)+"= "+x[i]);
    }
    static void main()
    {
        /**
        * Main method to input all parameters and then call the corresponding methods.
        */
        Equations eq=new Equations();
        Scanner in=new Scanner(System.in);
        System.out.println("Enter the number of unknowns and thus the number of equations. Let it be n.");
        int n=in.nextInt();
        double[][] A=new double[n][n];
        double[] b=new double[n];
        double[][] AU=new double[n][n+1];
        int i,j;
        System.out.println("You must enter a Non-Singular Equation Matrix A with dimensions n*n.");
        for(i=0;i<n;i++)
        {
            for(j=0;j<n;j++)
            {
                System.out.print("Enter the element in "+i+"(th) row and "+j+"(th) column:  ");
                A[i][j]=in.nextDouble();
                System.out.println();
            }
        }
        boolean c=eq.isSingular(A);
        if(!c)
        {
            System.out.println("The Matrix entered is :");
            for(i=0;i<n;i++)
            {
                for(j=0;j<n;j++)
                    System.out.print(A[i][j]+"  ");
                System.out.println();
            }
            System.out.println("Enter the result vector b. It must have n elements.");
            for(i=0;i<n;i++)
            {
                System.out.print("Enter the "+i+"(th) element:  ");
                b[i]=in.nextDouble();
                System.out.println();
            }
            System.out.println("The vector b entered is:");
            for(i=0;i<n;i++)
                System.out.print(b[i]+"  ");
            System.out.println();
            for(i=0;i<n;i++)
            {
                for(j=0;j<n;j++)
                    AU[i][j]=A[i][j];
            }
            for(i=0;i<n;i++)
                AU[i][n]=b[i];
            AU=eq.Elimination(AU);
            for(i=0;i<n;i++)
            {
                for(j=0;j<n;j++)
                    A[i][j]=AU[i][j];
            }
            for(i=0;i<n;i++)
                b[i]=AU[i][n];
            eq.Solutions(A,b);
        }
        else if(c)
            System.out.println("The entered Equation Matrix is singular. The equation system is dependent and thus has no solutions or an infinite number of solutions."); 
    }
}
