
using System;

namespace BasicsAgain
{
    public class Intermediate
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
        public static bool Cont(char[] list, char obj)
        {
            foreach (char c in list)
            {
                if (c == obj) return true;
            }
            
            return false;
        }
        public static bool Palindrome(string str)
        {
            str = Core.Uppercase(str);
            string sb = "";
            foreach (char c in str)
            {
                if (c != ' ')
                    sb += c;
            }

            if (str.Length == 0) return true;

            for (int i = 0; i < sb.Length / 2 + 1; i++)
            {
                if (sb[i] != sb[sb.Length - i - 1]) return false;
            }

            return true;
        }

        public static void Reverse(int[] arr)
        {
            for (int i = 0; i < arr.Length / 2; i++)
            {
                (arr[i], arr[arr.Length - i - 1]) = (arr[arr.Length - i - 1], arr[i]);
            }
        }

        public static string MyTrim(string toTrim, char[] toRemove)
        {
            if (toRemove.Length == 0)
                toRemove = new[] { ' ' };

            int index = 0;

            foreach (var t in toTrim)
            {
                if (!Cont(toRemove, t)) break;

                index++;
            }
            toTrim = Sub(toTrim, index);
            
            index = 0;

            for (int i = toTrim.Length - 1; i >= 0; i--)
            {
                if (!Cont(toRemove, toTrim[i])) break;
                index++;
            }
            
            return Sub(toTrim, 0, toTrim.Length - index);
        }

        public static bool DivineProofValidator(string victimsText, string divineProof)
        {
            if (divineProof.Length == 0) return true;

            for (int i = 0; i < victimsText.Length; i++)
            {
                if (victimsText[i] != divineProof[0]) continue;

                for (int j = 0; j < divineProof.Length; j++)
                {
                    int index = i + j;

                    if (index >= victimsText.Length || victimsText[index] != divineProof[j]) break;

                    if (j == divineProof.Length - 1) return true;
                }
            }

            return false;
        }

        public static string CodeZeus(string str, int n)
        {
            const string lc = "abcdefghijklmnopqrstuvwxyz";
            string uc = Core.Uppercase(lc);

            string sb = "";

            n = n < 0 ? lc.Length - n : n;

            foreach (char c in str)
            {
                int index;
                if (Cont(lc, c))
                {
                    index = (IndOf(lc, c) + n) % lc.Length;
                    sb += lc[index];
                    continue;
                }

                if (Cont(uc, c))
                {
                    index = (IndOf(uc, c) + n) % uc.Length;
                    sb += uc[index];
                    continue;
                }

                sb += c;
            }

            return sb;
        }
    }
}