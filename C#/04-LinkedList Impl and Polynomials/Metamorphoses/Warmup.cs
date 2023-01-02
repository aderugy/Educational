using System;

namespace Metamorphoses;

public class Warmup
{
    //Returns the maximum of two integers
    public static int Max(int x, int y)
    {
        return x > y ? x : y;
    }
    

    //Returns the maximum of two strings representing numbers
    public static string Max(string x, string y)
    {
        bool isXNegative = x[0] == '-';
        bool isYNegative = y[0] == '-';

        switch (isXNegative)
        {
            case true when !isYNegative:
                return y;
            case false when isYNegative:
                return x;
        }

        if (x.Length != y.Length)
            return (x.Length > y.Length && !isXNegative) || 
                   (y.Length > x.Length && isXNegative) ? x : y;

        for (int i = 0; i < x.Length; i++)
        {
            if (x[i] == '-' || y[i] == '-' || x[i] == y[i])
                continue;
            return (x[i] > y[i] && !isXNegative) ||
                   (x[i] < y[i] && isXNegative) ? x : y;
        }

        return "";
    }

    //Returns the sum of all the elements of an array of integers
    public static int SumArray(int[] tab)
    {
        int sum = 0;
        foreach (int i in tab)
            sum += i;
        return sum;
    }

    //Returns the concatenation of all the elements of an array of strings
    public static string SumArray(string[] tab)
    {
        string s = "";
        foreach (string e in tab)
            s += e;
        return s;
    }
}