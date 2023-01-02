namespace Basics
{
    public class Advanced
    {

        public static int IndexOf(string s, char c)
        {
            for (int i = 0; i < s.Length; i++)
            {
                if (s[i] == c) return i;
            }

            return -1;
        }
        public static string ToUpper(string s)
        {
            string lc = "qwertyuiopasdfghjklzxcvbnm";
            string uc = "QWERTYUIOPASDFGHJKLZXCVBNM";

            string toRet = "";

            foreach (char c in s)
            {
                if (lc.Contains(c))
                {
                    toRet += uc[IndexOf(lc, c)];
                    continue;
                }

                toRet += c;
            }

            return toRet;
        }
        public static int DecodeBase(string s, int b, string digits)
        {
            if (s.Equals(""))
                return 0;
            
            if (s.Equals("-"))
                return -1;

            bool isNegative = false;
            int result = 0;
            s = ToUpper(s);

            foreach (var c in s)
            {
                if (c == '-')
                {
                    isNegative = true;
                    continue;
                }

                if (!digits.Contains(c)) 
                    return -1;

                result *= b;
                result += IndexOf(digits, c);
            }

            return isNegative ? - result : result;
        }


        public static string EncodeBase(int n, int b, string digits)
        {
            string result = "";
            if (n == 0) return "0";
            
            bool isNegative = n < 0;
            n = isNegative ? -n : n;

            while (n > 0)
            {
                result = digits[n % b] + result;
                n /= b;
            }

            return isNegative ? "-" + result : result;
        }
        
        /// <summary>
        /// Convert an number written in a string into the corresponding int
        /// </summary>
        /// <param name="num">String of the number</param>
        /// <returns>An int of the number contained in the string</returns>
        public static int Atoi(string num)
        {
            return num == "" ? -1 : DecodeBase(num, 10, "0123456789");
        }

        /// <summary>
        /// Convert an int into its string representation 
        /// </summary>
        /// <param name="n">Int that we want to convert</param>
        /// <returns>A string representation of n</returns>
        public static string Itoa(int n)
        {
            return EncodeBase(n, 10, "0123456789");
        }
    }
}