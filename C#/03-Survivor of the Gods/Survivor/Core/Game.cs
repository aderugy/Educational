using System;

// ReSharper disable once CheckNamespace
namespace Survivor
{
    // CORRECT
    public class Game
    {
        public Random Random;
        protected int SpawnRate;
        protected int DaysLeft;
        protected Cell[,] Board;
        protected Player Player;
        
        // CORRECT
        public Game(Random random, int spawnRate, int daysLeft, int boardWidth, int boardHeight)
        {
            Random = random;
            SpawnRate = spawnRate;
            DaysLeft = daysLeft;
            
            if (boardHeight < 5 || boardWidth < 5)
            {
                throw new ArgumentException("Board must be at least 5x5");
            }
            
            Board = CreateBoard(boardWidth, boardHeight);

            Player = new Player(30, boardWidth / 2, boardHeight / 2);
        }

        // CORRECT
        public Cell[,] GetBoard()
        {
            return Board;
        }

        // CORRECT
        protected virtual void SpawnItem(int x, int y)
        {
            Cell currentCell = Board[x, y];

            int x2;
            int y2;

            do
            {
                int range = Random.Next(1, currentCell.GetSpawnRange() + 1);
                x2 = Random.Next(0, range);
                y2 = x2 - range;
                
                x2 *= Random.Next(2) == 1 ? 1 : -1;
                y2 *= Random.Next(2) == 1 ? 1 : -1;
            } while (x + x2 < 0 || y + y2 < 0 ||
                     x + x2 >= Board.GetLength(0) ||
                     y + y2 >= Board.GetLength(1));

            Cell cellAround = Board[x + x2, y + y2];
            if (cellAround.GetContent() != null) return;

            switch (currentCell)
            {
                case Sea:
                    cellAround.SetContent(new Coconut(4, 5));
                    break;
                case River:
                    cellAround.SetContent(new Plum(2, 9));
                    break;
                case Forest:
                    cellAround.SetContent(new Banana(3, 7));
                    break;
            }
        }

        // CORRECT
        protected void Spawn()
        {
            for (int x = 0; x < Board.GetLength(0); x++)
            {
                for (int y = 0; y < Board.GetLength(1); y++)
                {
                    int rand = Random.Next(1, 100);
                    
                    if (rand <= SpawnRate * Player.GetLuck())
                        SpawnItem(x, y);
                }
            }
        }

        // CORRECT
        protected virtual void PrintBoard()
        {
            ConsoleColor bg = Console.BackgroundColor;
            ConsoleColor fg = Console.ForegroundColor;
            for (int y = 0; y < Board.GetLength(1); y++)
            {
                Console.Write("|");
                for (int x = 0; x < Board.GetLength(0); x++)
                {
                    switch (Board[x, y])
                    {
                        case Forest:
                            Console.BackgroundColor = ConsoleColor.DarkGreen;
                            break;
                        case River:
                            Console.BackgroundColor = ConsoleColor.Blue;
                            break;
                        case Sea:
                            Console.BackgroundColor = ConsoleColor.DarkBlue;
                            break;
                        default:
                            Console.BackgroundColor = bg;
                            break;
                    }

                    if (Player.GetCoordinates().x == x && Player.GetCoordinates().y == y)
                        Console.Write("X");
                    else
                        Console.Write(Board[x, y]);
                }
                Console.BackgroundColor = bg;
                Console.ForegroundColor = fg;
                Console.WriteLine("|");
            }
        }
        
        // CORRECT
        protected void PrintStats()
        {
            Console.WriteLine($"Days left: {DaysLeft}");
            Console.WriteLine($"Energy: {Player.GetEnergy()}");
            Console.WriteLine("Thirst: " + (Player.GetThirst() ? "True" : "False"));
        }
        
        // CORRECT
        protected virtual void PrintAll()
        {
            PrintBoard();
            PrintStats();
        }

        // CORRECT
        protected void PrintEnd(bool win)
        {
            if (win)
            {
                Console.WriteLine("You survived the journey!");
                return;
            }

            Console.WriteLine("You died.");
        }
        
        // CORRECT
        protected virtual void UpdateBoard()
        {
            for (int x = 0; x < Board.GetLength(0); x++)
            {
                for (int y = 0; y < Board.GetLength(1); y++)
                {
                    Board[x,y].Update();
                }
            }
        }
        
        // CORRECT
        protected virtual bool NextDay()
        {
            bool survived = Player.SpendTheNight();
            if (!survived || DaysLeft <= 0)
            {
                PrintEnd(survived);
                return false;
            }

            DaysLeft--;
            UpdateBoard();
            Spawn();

            return true;
        }


        // CORRECT
        protected virtual bool GetAction()
        {
            while (true)
            {
                char input = (char) Console.Read();
                if (Player.GetEnergy() <= 0)
                {
                    PrintEnd(false);
                    return false;
                }
                switch (input)
                {
                    case 'w':
                    case 'a':
                    case 's':
                    case 'd':
                        Player.Move(this, input);
                        break;

                    case 'i':
                        UseInteraction();
                        break;

                    case 'n':
                        return NextDayInteraction();

                    case 'q':
                        return false;
                }

                return true;
            }
        }
        
        
        // CORRECT
        protected void UseInteraction()
        {
            Cell cell = Board[Player.GetCoordinates().x, Player.GetCoordinates().y];
            if (cell is River && Player.GetThirst()) Player.Drink();
            if (cell.GetContent() == null) return;
            if (cell.GetContent().GetIsEdible() && Player.Eat(cell.GetContent())) cell.SetContent(null);
        }
        
        // CORRECT
        protected bool NextDayInteraction()
        {
            if (!Player.GetThirst()) return NextDay();
            while (true)
            {
                Console.WriteLine("If you are still thirsty, you will die during the night. " 
                                  + "Do you still want to end the day? (y/n)");
                char ans = (char) Console.Read();

                switch (ans)
                {
                    case 'y':
                        return NextDay();
                    case 'n':
                        return true;
                    default:
                        continue;
                }
            }
        }

        // NOT CHECKED
        public void Play()
        {
            Spawn();
            do
            {
                PrintAll();
            } while (GetAction());
        }
        
        // CORRECT
        protected Cell[,] CreateBoard(int width, int height)
        {
            Cell[,] board = new Cell[width, height];
            for (int x = 0; x < board.GetLength(0); x++)
            {
                for (int y = 0; y < board.GetLength(1); y++)
                {
                    if (x == 0 || x == board.GetLength(0) - 1 || y == 0 || y == board.GetLength(1) - 1)
                        board[x, y] = new Sea(1, 2);
                    else if (x == 2 * y)
                        board[x, y] = new River(2, 2); 
                    else
                        board[x, y] = new Forest(1, 2);
                }
            }

            return board;
        }

        // CORRECT
        public int GetSpawnRate()
        {
            return SpawnRate;
        }
        
        // CORRECT
        public void SetSpawnRate(int spawnRate)
        {
            SpawnRate = spawnRate;
        }

        // CORRECT
        public Player GetPlayer()
        {
            return Player;
        }

        // CORRECT
        public Random GetRandom()
        {
            return Random;
        }
    }
}
