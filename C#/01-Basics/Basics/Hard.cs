namespace Basics
{
    public class Hard
    {
        private static int DecodeBase(string s, int b, string digits)
        {
            return Advanced.DecodeBase(s, b, digits);
        }


        private static string EncodeBase(int n, int b, string digits)
        {
            return Advanced.EncodeBase(n, b, digits);
        }
        
        
        /// <summary>
        /// Decode a binary in a string into its decimal representation
        /// </summary>
        /// <param name="s">String holding the binary</param>
        /// <returns>An int corresponding to the decimal representation of the
        /// binary</returns>
        public static int DecodeBinary(string s)
        {
            bool isNegative = false;
            int result = 0;
            
            for (int i = 0 ; i < s.Length ; i++)
            {
                result <<= 1;

                switch (s[i])
                {
                    case '1':
                        result++;
                        break;
                    case '0':
                        break;
                    case '-':
                        isNegative = true;
                        break;
                    default:
                        return -1;
                }
            }
            return isNegative ? -result : result;
        }

        /// <summary>
        /// Encode a decimal in binary
        /// </summary>
        /// <param name="n">An int that we want to convert in binary</param>
        /// <returns>A string holding the binary representation of n</returns>
        public static string EncodeBinary(int n)
        {
            return EncodeBase(n, 2, "01");
        }

        /// <summary>
        ///Decode a number written in octal in a string into its decimal representation
        /// </summary>
        /// <param name="s">String holding the octal</param>
        /// <returns>An int corresponding to the decimal representation of the octal</returns>
        public static int DecodeOctal(string s)
        {
            return DecodeBase(s, 8, "01234567");
        }

        /// <summary>
        /// Encode a decimal in octal
        /// </summary>
        /// <param name="n">An int that we want to convert in octal</param>
        /// <returns>A string holding the octal representation of n</returns>
        public static string EncodeOctal(int n)
        {
            if (n == 0) return "0";
            return EncodeBase(n, 8, "01234567");
        }
        
        /// <summary>
        ///Decode a number written in hexadecimal in a string into its decimal representation
        /// </summary>
        /// <param name="s">String holding the hexadecimal</param>
        /// <returns>An int corresponding to the decimal representation of the hexadecimal</returns>
        public static int DecodeHexa(string s)
        {
            return DecodeBase(s, 16, "0123456789ABCDEF");
        }

        /// <summary>
        /// Encode a decimal in hexadecimal
        /// </summary>
        /// <param name="n">An int that we want to convert in hexadecimal</param>
        /// <returns>A string holding the hexadecimal representation of n</returns>
        public static string EncodeHexa(int n)
        {
            return EncodeBase(n, 16, "0123456789ABCDEF");
        }
    }
}