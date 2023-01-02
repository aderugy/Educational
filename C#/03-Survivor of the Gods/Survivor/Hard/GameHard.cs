using System;

namespace Survivor
{
    public class GameHard : GameAdvanced
    {
        private (int x, int y) _ulyssescoordinates;
        
        // CORRECT
        public GameHard(Random random, int spawnRate, int daysLeft, int boardWidth, int boardHeight)
        :base (random, spawnRate, daysLeft, boardWidth, boardHeight)
        {
            Player = new PlayerHard(30, boardWidth / 2, boardHeight / 2);
            Board = CreateBoard(boardWidth, boardHeight);
        }
        
        // CORRECT
        private Cell SpawnUlysses()
        {
            while (true)
            {
                int x = Random.Next(0, Board.GetLength(0));
                int y = Random.Next(0, Board.GetLength(1));

                if (Board[x, y] is not CellHard) continue;
                
                CellHard cell = (CellHard)Board[x, y];
                cell.SetUlysses(15);
                _ulyssescoordinates = (x, y);
                return cell;
            }
        }
        
        protected override bool NextDay()
        {
            if (!Player.SpendTheNight())
            {
                PrintEnd(false);
                return false;
            }

            if (UlyssesExists() && 
                Player.GetCoordinates().x == _ulyssescoordinates.x
                && Player.GetCoordinates().y == _ulyssescoordinates.y)
            {
                PrintEnd(true);
                return false;
            }

            if (DaysLeft <= 0 && !UlyssesExists())
                SpawnUlysses();
            else
                DaysLeft--;

            MakeOffering();
            UpdateGods();
            UpdateBoard();
            Spawn();

            if (UlyssesExists())
            {
                CellHard cell = (CellHard)Board[_ulyssescoordinates.x, _ulyssescoordinates.y];
                if (cell.GetUlysses() <= 0) _ulyssescoordinates = (0, 0);
            }

            return true;
        }

        private bool UlyssesExists()
        {
            int x = _ulyssescoordinates.x;
            int y = _ulyssescoordinates.y;
            return x > 0 && y > 0 && x < Board.GetLength(0) && y < Board.GetLength(1);
        }

        private void PrintPathToUlysses()
        {
            throw new NotImplementedException();
        }

        // CORRECT
        protected override void PrintAll()
        {
            base.PrintAll();
            PrintPathToUlysses();
        }
        
        // CORRECT
        protected new Cell[,] CreateBoard(int width, int height)
        {
            Cell[,] board = new Cell[width, height];

            for (int x = 0; x < width; x++)
            {
                for (int y = 0; y < height; y++)
                {
                    if (Random.Next(20) == 0)
                    {
                        board[x, y] = new CellHard();
                        continue;
                    }
                    
                    if (x == 0 || y == 0 || x == width - 1 || y == height - 1)
                    {
                        board[x, y] = new Sea(1, 2);
                        continue;
                    }

                    if (x == 2 * y)
                        board[x, y] = new River(2, 2);
                    else
                        board[x, y] = new Forest(1, 2);
                }
            }
            
            return board;
        }
    }
}