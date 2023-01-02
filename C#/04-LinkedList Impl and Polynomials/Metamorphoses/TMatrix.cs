using System;

namespace Metamorphoses;

public class TMatrix<T> where T : System.Numerics.INumber<T>
{
    public uint Height { get; set; }

    public uint Width { get; set; }

    public T[,] Elements { get; set; }

    public T this[uint h, uint w]
    {
        get => Elements[h, w];
        set => Elements[h, w] = value;
    }

    public class DimensionsMismatchException : Exception
    {
    }

    public class UnsupportedValueException : Exception
    {
    }

    //Instantiates a TMatrix of size width*height
    public TMatrix(uint width, uint height)
    {
        Elements = new T[width, height];
        Width = width;
        Height = height;
    }

    //Instantiates a TMatrix of size width*height filled with element
    public TMatrix(uint width, uint height, T element)
    {
        Elements = new T[width, height];
        Width = width;
        Height = height;
        
        for (int x = 0; x < width; x++)
        {
            for (int y = 0; y < height; y++)
            {
                Elements[x, y] = element;
            }
        }
    }

    //Instantiates a TMatrix from an array of two dimensions
    public TMatrix(T[,] elements)
    {
        Elements = elements;
        Width = (uint)elements.GetLength(0);
        Height = (uint)elements.GetLength(1);
    }

    //Returns the string representing the TMatrix graphically 
    public override string ToString()
    {
        string toPrint = "{ ";
        
        for (int x = 0; x < Width; x++)
        {
            toPrint += RowToString(x);
            if (x != Width - 1)
                toPrint += ",\n  ";
            else
                toPrint += ' ';
        }

        return toPrint + '}';
    }

    private string RowToString(int x)
    {
        string toReturn = "{ ";
        
        for (int y = 0; y < Height; y++)
        {
            toReturn += Elements[x, y];
            if (y != Height - 1)
                toReturn += ',';
            toReturn += ' ';
        }

        return toReturn + '}';
    }

    //Returns true if both matrices are equals
    public static bool operator ==(TMatrix<T> m1, TMatrix<T> m2)
    {
        if (m1 is null && m2 is null) return true;
        if (m1 is null || m2 is null) return false;
        if (m1.Width != m2.Width || m1.Height != m2.Height) return false;

        for (int x = 0; x < m1.Width; x++)
        {
            for (int y = 0; y < m1.Height; y++)
            {
                if (!m1.Elements[x, y].Equals(m2.Elements[x, y])) return false;
            }
        }
        
        return true;
    }

    //Returns false if both matrices are equals, we hightly suggest to use the == operator
    public static bool operator !=(TMatrix<T> m1, TMatrix<T> m2)
    {
        return !(m1 == m2);
    }

    //Returns the sum of two matrices of numbers. Tt must not modify the parameters
    public static TMatrix<T> operator +(TMatrix<T> m1, TMatrix<T> m2)
    {
        if (m1.Width != m2.Width ||
            m1.Height != m2.Height)
            throw new DimensionsMismatchException();

        TMatrix<T> toReturn = new TMatrix<T>(m1.Width, m1.Height);

        for (int x = 0; x < m1.Width; x++)
        {
            for (int y = 0; y < m1.Height; y++)
            {
                toReturn.Elements[x, y] = m1.Elements[x, y] + m2.Elements[x, y];
            }
        }

        return toReturn;
    }

    //Returns the subtraction of two matrices of numbers. It must not modify the parameters
    public static TMatrix<T> operator -(TMatrix<T> m1, TMatrix<T> m2)
    {
        if (m1.Width != m2.Width ||
            m1.Height != m2.Height)
            throw new DimensionsMismatchException();

        TMatrix<T> toReturn = new TMatrix<T>(m1.Width, m1.Height);

        for (int x = 0; x < m1.Width; x++)
        {
            for (int y = 0; y < m1.Height; y++)
            {
                toReturn.Elements[x, y] = m1.Elements[x, y] - m2.Elements[x, y];
            }
        }

        return toReturn;
    }

    //Returns the result of the multiplication of a TMatrix of numbers and an integer n
    public static TMatrix<T> operator *(TMatrix<T> m, int n)
    {
        TMatrix<T> toReturn = new TMatrix<T>(m.Width, m.Height);

        for (int x = 0; x < m.Width; x++)
        {
            for (int y = 0; y < m.Height; y++)
            {
                toReturn.Elements[x, y] = (dynamic) m.Elements[x, y] * n;
            }
        }

        return toReturn;
    }
    
    

    //Multiplies two matrices of numbers and returns the result
    public static TMatrix<T> operator *(TMatrix<T> m1, TMatrix<T> m2)
    {
        if (m1.Height != m2.Width) 
            throw new DimensionsMismatchException();
        
        TMatrix<T> toReturn = new TMatrix<T>(m1.Width, m2.Height);

        for (int x = 0; x < toReturn.Width; x++)
        {
            for (int y = 0; y < toReturn.Height; y++)
            {
                T[] values = new T[m2.Width];

                for (int i = 0; i < m2.Width; i++)
                {
                    values[i] = m1.Elements[x, i] * m2.Elements[i, y];
                }

                toReturn.Elements[x, y] = values[0];
                for (int i = 1; i < values.Length; i++)
                {
                    toReturn.Elements[x, y] += values[i];
                }
            }
        }

        return toReturn;
    }

    //Returns a new TMatrix of numbers that is the TMatrix m1 at power n
    public static TMatrix<T> operator ^(TMatrix<T> m1, uint n)
    {
        if (n < 1) throw new UnsupportedValueException();

        TMatrix<T> toReturn = new TMatrix<T>(m1.Width, m1.Height);

        for (int x = 0; x < m1.Width; x++)
        {
            for (int y = 0; y < m1.Height; y++)
            {
                toReturn.Elements[x, y] = m1.Elements[x, y];
            }
        }
        
        for (int i = 1; i < n; i++)
        {
            toReturn *= m1;
        }

        return toReturn;
    }

    //Transform the TMatrix into its transpose
    public void Transpose()
    {
        TMatrix<T> reversedMatrix = new TMatrix<T>(Height, Width);

        for (int x = 0; x < Width; x++)
        {
            for (int y = 0; y < Height; y++)
            {
                reversedMatrix.Elements[y, x] = Elements[x, y];
            }
        }

        Width = reversedMatrix.Width;
        Height = reversedMatrix.Height;
        Elements = reversedMatrix.Elements;
    }
}