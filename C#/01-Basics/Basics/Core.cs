using System;

namespace Basics
{
    public class Core
    {
        /// <summary>
        /// This function must write in the console "Hello Olympus!" Followed by a new line
        /// </summary>
        public static void HelloGods()
        {
            Console.WriteLine("Hello Olympus!");
        }

        
        /// <summary>
        /// This function return the number of letter in your name
        /// multiplied by 42 added to your age.
        /// </summary>
        /// <param name="age">Your age</param>
        /// <param name="name">Your name</param>
        /// <returns>An int corresponding to your age if you were a god</returns>
        public static int MyGodAge(int age, string name)
        {
            return (age < 0 || name.Length == 0) ? -1 : age + 42 * name.Length;
        }

        /// <summary>
        /// Your own implementation of x power n.
        /// </summary>
        /// <param name="x">The number that will be powered</param>
        /// <param name="n">The power wanted</param>
        /// <returns>A double corresponding to the x powered by n </returns>
        public static double MyPow(double x, int n)
        {
            if (x == 0 && n < 0) 
                return -1;

            bool flag = n < 0;
            double result = 1;
            
            n = flag ? -n : n;
            for (int i = 0; i < n; i++)
            {
                result *= x;
            }

            return flag ? 1 / result : result;
        }
        
        /// <summary>
        /// Factorial of n.
        /// </summary>
        /// <param name="n">Number of the factorial</param>
        /// <returns>An int corresponding to the factorial of n</returns>
        public static int Facto(int n)
        {
            if (n < 0) 
                return -1;
            if (n == 0) 
                return 1;

            int result = 1;
            for ( ; n > 0; n--)
            {
                result *= n;
            }

            return result;
        }

        /// <summary>
        /// Fibonacci sequence of n.
        /// </summary>
        /// <param name="n">Term of the fibonacci wanted</param>
        /// <returns>An int corresponding to the n-th term of the fibonacci sequence</returns>
        public static int Fibo(int n)
        {
            if (n < 0)
                return -1;
            if (n <= 1) 
                return n;

            int a = 0;
            int b = 1;
            int k = 1;

            while (n > 0)
            {
                k = a + b;
                a = b;
                b = k;
                n--;
            }

            return a;
        }
        
        ///<summary>
        ///Try to understand what this function is supposed to do, and debug it !
        ///</summary>
        public static uint DebugMe1(uint x)
        {
            uint result = 0;
            for (uint i = x; i > 0; i /= 10)
            {
                result += i % 10;
            }
            return result;
        }

        ///<summary>
        ///Try to understand what this function is supposed to do, and debug it !
        /// </summary>
        public static int DebugMe2(int x)
        {
            int i = 1;
            int result = 0;
            while (i < x)
            {
                if (i % 2 == 0)
                {
                    result += i;
                }

                i += 1;
            }

            return result;
        }
        
        /// <summary>
        /// Determine if a number is prime or not.
        /// </summary>
        /// <param name="n">Number that we want to determine verify its primeness</param>
        /// <returns>A boolean that is true if n is prime</returns>
        public static bool IsPrime(uint n)
        {
            if (n <= 1) return false;
            
            for (int i = 2; i <= n / 2; i++)
            {
                if (n % i == 0)
                    return false;
            }

            return true;
        }

        /// <summary>
        /// Ask for a number and print in the console if the number is prime or not
        /// </summary>
        public static void AskPrime()
        {
            Console.WriteLine("What's your number?");
            
            uint n = Convert.ToUInt32(Console.ReadLine());
            bool isPrime = IsPrime(n);

            string s = string.Format((isPrime ? "Yes, {0:d} is a prime number." : "No, {0:d} is not a prime number."), n);
            Console.WriteLine(s);
        }
    }
}