using System;

namespace Survivor
{
    public class GameIntermediate : Game
    {
        protected God[] Gods;

        // CORRECT
        public GameIntermediate(Random random, int spawnRate, int daysLeft, int boardWidth, int boardHeight)
            : base(random, spawnRate, daysLeft, boardWidth, boardHeight)
        {
            Player = new PlayerIntermediate(30, boardWidth / 2, boardHeight / 2);

            Gods = new God[4];
            Gods[0] = new Demeter(new Plum(2, 9),
                new Animal(12), 4);
            Gods[1] = new Hypnos(new Coconut(4, 5),
                new Banana(3, 7), 2);
            Gods[2] = new Poseidon(new Banana(3, 7),
                new Plum(2, 9), 2);
            Gods[3] = new Tyche(new Animal(12),
                new Coconut(4, 5), 3);
        }

        // CORRECT
        public void IncreaseDaysLeft(int days)
        {
            DaysLeft += days;
        }
        
        // DIDNT PASS
        protected void MakeOffering()
        {
            while (true)
            {
                Console.WriteLine("If you want to make an offering, " +
                                  "select the god to whom you want to sacrifice. " +
                                  "(quit with 'q').");

                char input = (char)Console.Read();
                if (input == 'q') return;

                const string digits = "0123456789";
                int iof = digits.IndexOf(input);
                
                if (iof >= Gods.Length || iof < 0) continue;
                ((PlayerIntermediate)Player).Sacrifice(
                    this, Gods[iof]);
                return;
            }
        }

        // FIXED
        protected override void SpawnItem(int x, int y)
        {
            int x2;
            int y2;
            Cell currentCell = Board[x, y];

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

            switch (currentCell)
            {
                case Sea:
                    cellAround.SetContent(new Coconut(4, 5));
                    break;
                case River:
                    cellAround.SetContent(new Plum(2, 9));
                    break;
                case Forest:
                    if (Random.Next(0, 5) == 0)
                        cellAround.SetContent(new Animal(12));
                    else
                        cellAround.SetContent(new Banana(3, 7));
                    break;
                default:
                    return;
            }
        }

        // RIGHT
        protected override bool NextDay()
        {
            bool survived = Player.SpendTheNight();
            if (!survived || DaysLeft <= 0)
            {
                PrintEnd(survived);
                return false;
            }

            DaysLeft--;
            MakeOffering();
            UpdateGods();
            UpdateBoard();
            Spawn();

            return true;
        }

        // CORRECT
        protected void UpdateGods()
        {
            foreach (God t in Gods)
            {
                t.Update();
                if (t.GetPatience() <= 0)
                    t.Disaster(this);
            }
        }

        // WRONG
        protected override bool GetAction()
        {
            while (true)
            {
                char input = (char)Console.Read();
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

                    case 'n':
                        return NextDayInteraction();

                    case 'q':
                        return false;
                }

                return true;
            }
        }

        // CORRECT
        protected void PickOrDropInteraction()
        {
            Cell cell = Board[Player.GetCoordinates().x, Player.GetCoordinates().y];
            if (cell.GetContent() != null)
                ((PlayerIntermediate)Player).PickUp(Board);
            else
                ((PlayerIntermediate)Player).Drop(Board);
        }

        // CORRECT
        protected new void UseInteraction()
        {
            Cell cell = Board[Player.GetCoordinates().x, Player.GetCoordinates().y];
            if (cell as River != null! && Player.GetThirst()) Player.Drink();
            if (cell.GetContent() == null) return;
            if (cell.GetContent().GetIsEdible() && Player.Eat(cell.GetContent())) cell.SetContent(null!);
            if (cell.GetContent() is Animal animal && !animal.GetIsEdible()) animal.Kill();
        }

        // CORRECT
        protected void PrintGods()
        {
            String sb = "";
            foreach (God god in Gods)
            {
                sb += "| " + god + ' ';
            }

            sb += '|';
            Console.WriteLine(sb);
        }

        // CORRECT
        protected void PrintInventory()
        {
            Item[] inventory = ((PlayerIntermediate)Player).GetInventory();
            string sb = "";

            foreach (var t in inventory)
            {
                sb += "| " + (t == null || t.ToString().Equals(" ") ? "" : t) + ' ';
            }

            sb += '|';
            Console.WriteLine(sb);
        }

        // CORRECT
        protected override void PrintAll()
        {
            PrintGods();
            base.PrintAll();
            PrintInventory();
        }
    }
}
