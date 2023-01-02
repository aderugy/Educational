using System;

namespace BasicsAgain
{
    public class ZeroSizeArrayException : Exception { }

    public static class Core
    {
        
        public static int IndOf(string s, char c)
        {
            for (int i = 0; i < s.Length; i++)
            {
                if (s[i] == c) return i;
            }

            return -1;
        }
        
        public static bool Cont(string list, char obj)
        {
            foreach (char c in list)
            {
                if (c == obj) return true;
            }
            
            return false;
        }
        public static int[] NewShelf(uint size, int defaultValue)
        {
            if (size == 0)
                throw new ZeroSizeArrayException();

            int[] toReturn = new int[size];
            for (int i = 0; i < size; i++)
            {
                toReturn[i] = defaultValue;
            }

            return toReturn;
        }

        public static string Uppercase(string bookContent)
        {
            string upper = "AZERTYUIOPQSDFGHJKLMWXCVBN";
            string lower = "azertyuiopqsdfghjklmwxcvbn";

            string sb = "";

            foreach (char c in bookContent)
            {
                sb += Cont(lower, c) ? upper[IndOf(lower, c)] : c;
            }

            return sb;
        }

        public static char[] Split(string bookContent)
        {
            char[] ch = new char[bookContent.Length];

            for (int i = 0; i < bookContent.Length; i++)
            {
                ch[i] = bookContent[i];
            }

            return ch;
        }

        public static void Print(char[][] bookContents)
        {
            foreach (char[] row in bookContents)
            {
                foreach (char c in row)
                {
                    Console.Write(c);
                }
                Console.WriteLine();
            }
        }

        public static int AreRecordsOkay(char[] record, char[] recordCopy)
        {
            int l = (record.Length > recordCopy.Length ? record.Length : recordCopy.Length);
            for (int i = 0; i < l; i++)
            {
                char a = i >= record.Length ? (char)0 : record[i];
                char b = i >= recordCopy.Length ? (char)0 : recordCopy[i];

                if (a != b)
                    return a - b;
            }

            return 0;
        }

        public static int FindFirstGrade(int[] grades, int grade)
        {
            for (int i = 0; i < grades.Length; i++)
            {
                if (grades[i] == grade)
                    return i;
            }

            return -1;
        }

        public static int ViceMin(int[] grades)
        {
            if (grades.Length < 2) throw new ArgumentException();


            int min = grades[0];
            int vMin = grades[0];

            foreach (int grade in grades)
            {
                if (min == vMin)
                {
                    int c = vMin;
                    vMin = grade < min ? min : grade;
                    min = grade < min ? grade : c;
                    continue;
                }

                if (grade < min)
                {
                    vMin = min;
                    min = grade;
                    continue;
                }

                if (grade <= min || grade >= vMin) continue;
                vMin = grade;
            }

            return vMin;
        }

        public static double Average(double[] grades)
        {
            if (grades.Length == 0)
                throw new ArithmeticException();

            double sum = 0;

            foreach (double grade in grades)
            {
                sum += grade;
            }

            return sum / grades.Length;
        }

        public static double WeightedAverage(double[] grades, double[] coefficients)
        {
            if (grades.Length != coefficients.Length) throw new ArgumentException();
            if (grades.Length < 1 || coefficients.Length < 1) throw new ArithmeticException();

            double gSum = 0;
            double cSum = 0;
            for (int i = 0; i < grades.Length; i++)
            {
                if (coefficients[i] <= 0) throw new ArithmeticException();
                cSum += coefficients[i];

                gSum += coefficients[i] * grades[i];
            }

            return gSum / cSum;
        }
    }
}