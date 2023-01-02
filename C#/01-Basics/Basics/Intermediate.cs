using System;

namespace Basics
{
    public class Intermediate
    {
        public static string Substring(string s, int index)
        {
            if (index > s.Length) 
                throw new ArgumentException("Substring: Index > Length");

            string toRet = "";
            for (int i = index; i < s.Length; i++)
            {
                toRet += s[i];
            }

            return toRet;
        }

        public static string Substring(string s, int index, int length)
        {
            if (index + length > s.Length)
                throw new ArgumentException("Substring: Index > Length");

            string toRet = "";
            for (int i = index; i - index < length; i++)
            {
                toRet += s[i];
            }

            return toRet;
        }
        
        ///<summary>
        ///Try to understand what this function is supposed to do, and debug it !
        ///</summary>
        public static uint DebugMe3(uint x)
        {
            uint result = 0;
            for (uint i = x; i >= 2; i--)
            {
                uint j = i - 1;
                while (i % j != 0 && j > 1)
                {
                    j--;
                }
                if (j == 1)
                {
                    result += 1;
                }
            }
            return result;
        }

        /// <summary>
        /// Add all of the divisor of n.
        /// </summary>
        /// <param name="n">Int that we want to find the sum of its divisor</param>
        /// <returns>An int corresponding to the sum of the divisor of n</returns>
        public static int DivisorSum(int n)
        {
            if (n <= 0)
            {
                Console.Error.WriteLine("N must be positive");
                return -1;
            }

            int result = 0;

            for (int i = 1; i <= n / 2; i++)
            {
                if (n % i == 0)
                    result += i;
            }

            return result;
        }

        /// <summary>
        /// Split a string in two part at a certain index
        /// </summary>
        /// <param name="splitMe">String that we want to split in two</param>
        /// <param name="n">Int corresponding to the index where
        /// we want to cut the string</param>
        /// <returns>A couple of two string, the first contains the start of the string
        /// until index included and the second contain the rest of the string</returns>
        public static (string, string) SplitString(string splitMe, int n)
        {
            if (n >= splitMe.Length || n < 0)
            {
                Console.Error.WriteLine("N is out of bound");
                return ("", "");
            }

            return (Substring(splitMe, 0, n + 1), 
                Substring(splitMe, n + 1));
        }
    }
}