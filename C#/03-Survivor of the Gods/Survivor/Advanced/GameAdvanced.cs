using System;

namespace Survivor
{
    public class GameAdvanced : GameIntermediate
    {
        protected int ViewRange;
        public GameAdvanced(Random random, int spawnRate, int daysLeft, int boardWidth, int boardHeight)
        :base(random, spawnRate, daysLeft, boardWidth, boardHeight)
        {
            ViewRange = 5;

            Player = new PlayerAdvanced(30, boardWidth / 2, boardHeight / 2);
            
            Gods = new God[5];
            Gods[0] = new Demeter(new Plum(2, 9), 
                new Animal(12), 4);
            Gods[1] = new HypnosAdvanced(new Coconut(4, 5), 
                new Banana(3, 7), 2);
            Gods[2] = new PoseidonAdvanced(new Banana(3, 7), 
                new Plum(2, 9), 2);
            Gods[3] = new Tyche(new Animal(12), 
                new Coconut(4, 5), 3);
            Gods[4] = new Zeus(new Banana(3, 7),
                new Animal(12), 3);
        }

        public int GetViewRange()
        {
            return ViewRange;

        }
        public void SetViewRange(int viewRange)
        {
            ViewRange = viewRange;

        }
        
        protected override bool GetAction()
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

                    case 'z':
                        PickOrDropInteraction();
                        break;

                    case 'p':
                        PlantInteraction();
                        break;
                    
                    case 'n':
                        return NextDayInteraction();

                    case 'q':
                        return false;
                }

                return true;
            }
        }

        protected void PlantInteraction()
        {
            ((PlayerAdvanced) Player)
                .Plant(Board[Player.GetCoordinates().x, Player.GetCoordinates().y]);
        }
        
        // FIXED
        protected override void UpdateBoard()
        {
            for (int x = 0; x < Board.GetLength(0); x++)
            {
                for (int y = 0; y < Board.GetLength(1); y++)
                {
                    Cell cell = Board[x,y];
                    cell.Update();
                    
                    if (cell.GetContent() != null && cell.GetContent() is Tree tree && tree.GetGrowth() == 0)
                        SpawnItem(x, y);
                }
            }
        }

        // FIXED
        private bool IsInRange(int x, int y)
        {
            int posX = Player.GetCoordinates().x;
            int posY = Player.GetCoordinates().y;

            int dx = posX - x;
            int dy = 2 * (posY - y);

            dx *= dx;
            dy *= dy;
            
            return  Math.Sqrt(dx + dy) <= ViewRange;
        }

        // CORRECT
        protected override void PrintBoard()
        {
            ConsoleColor bg = Console.BackgroundColor;
            ConsoleColor fg = Console.ForegroundColor;
            for (int y = 0; y < Board.GetLength(1); y++)
            {
                Console.Write("|");
                for (int x = 0; x < Board.GetLength(0); x++)
                {
                    if (!IsInRange(x, y))
                    {
                        Console.BackgroundColor = bg;
                        Console.ForegroundColor = fg;
                        Console.Write(' ');
                        continue;
                    }

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
    }
}