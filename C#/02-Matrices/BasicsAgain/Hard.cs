using System;

namespace BasicsAgain
{
    public class Hard
    {
        public static string Sub(string s, int index)
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

        public static string Sub(string s, int index, int length)
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
        public static int DecodeBase(string s, int b, string digits)
        {
            if (s.Length == 0)
                return 0;
                
            if (s[0] == '-' && s.Length == 1)
                return -1;

            bool isNegative = false;
            int result = 0;
            s = Core.Uppercase(s);

            foreach (var c in s)
            {
                if (c == '-')
                {
                    isNegative = true;
                    continue;
                }

                if (!Cont(digits, c)) 
                    return -1;

                result *= b;
                result += IndOf(digits, c);
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

        private static int Parse(string s)
        {
            return DecodeBase(s, 10, "0123456789");
        }

        private static string ToStr(int n)
        {
            return EncodeBase(n, 10, "0123456789");
        }
        private static int GetCell(int[][] mat, int x, int y)
        {
            try
            {
                return mat[x][y];
            }
            catch (Exception)
            {
                return 0;
            }
        }

        private static string Add(string s, int n)
        {
            return ToStr(n) + (s.Length == 0 ? "" : " " + s);
        }

        private static string GetNext(string s)
        {
            int index = IndOf(s, ' ');
            return index == -1 ? s : Sub(s, 0, index);
        }

        private static string Tails(string s)
        {
            int index = IndOf(s, ' ');
            return index == -1 ? "" : Sub(s, index + 1);
        }

        private static bool CheckTown(int[][] mat, int posX, int posY)
        {
            string sx = ToStr(posX);
            string sy = ToStr(posY);

            if (GetCell(mat, posX, posY) == 0) return false;

            while (sx.Length > 0)
            {
                int x = Parse(GetNext(sx));
                int y = Parse(GetNext(sy));
                
                sx = Tails(sx);
                sy = Tails(sy);
                
                if (GetCell(mat, x, y) == 0) continue;

                mat[x][y] = 0;

                sx = Add(sx, x + 1);
                sx = Add(sx, x - 1);
                sx = Add(sx, x);
                sx = Add(sx, x);

                sy = Add(sy, y);
                sy = Add(sy, y);
                sy = Add(sy, y + 1);
                sy = Add(sy, y - 1);
            }

            return true;
        }
        
        public static int CountTowns(int[][] map)
        {
            int n = 0;
            
            for (int y = 0; y < map.Length; y++)
            {
                int[] row = map[y];

                for (int x = 0; x < row.Length; x++)
                {
                    if (CheckTown(map, y, x))
                    {
                        n++;
                    }
                }
            }

            return n;
        }
    }
}
