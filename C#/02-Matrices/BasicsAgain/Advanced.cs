namespace BasicsAgain
{
    public class Advanced
    {
        public static int KingOfTheHill(int[] performances)
        {
            if (performances.Length == 0) return -1;

            int max = 0;
            bool hasIncreased = false;
            bool hasDecreased = false;
            bool increasing = false;
            bool decreasing = false;

            for (int i = 1; i < performances.Length; i++)
            {
                var last = performances[i - 1];
                int n = performances[i];

                if (n == last)
                {
                    max = n >= performances[max] ? i : max;
                    continue;
                }

                if (n > last && !hasIncreased)
                {
                    increasing = true;
                    hasDecreased = decreasing;

                    max = i;
                    
                    continue;
                }

                if (n < last && !hasDecreased)
                {
                    decreasing = true;
                    hasIncreased = increasing;

                    continue;
                }

                return -1;
            }

            return max;
        }

        private static int[][] InitMat(int[][] mat, int l1, int l2)
        {
            for (int i = 0; i < mat.Length; i++)
            {
                mat[i] = new int[l2 + 1];
            }
            for (int i = 0; i <= l1; i++)
            {
                mat[i][0] = i;
            }

            for (int i = 0; i <= l2; i++)
            {
                mat[0][i] = i;
            }

            return mat;
        }

        public static int Levenshtein(string str1, string str2)
        {
            int l1 = str1.Length;
            int l2 = str2.Length;
            
            int[][] mat = new int[l1 + 1][];
            mat = InitMat(mat, l1, l2);
            
            for (int i = 1; i <= l1; i++)
            {
                for (int j = 1; j <= l2; j++)
                {
                    int cost = str1[i - 1] == str2[j - 1] ? 0 : 1;
                    mat[i][j] = Min3(
                        mat[i - 1][j    ] + 1,
                        mat[i    ][j - 1] + 1,
                        mat[i - 1][j - 1] + cost);
                }
            }
            
            return mat[l1][l2];
        }

        private static int Min3(int a, int b, int c)
        {
            var minAb = a < b ? a : b;
            return minAb < c ? minAb : c;
        }
    }
}