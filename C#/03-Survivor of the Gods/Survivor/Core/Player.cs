using System;

namespace Survivor
{
    public class Player
    {
        protected readonly int MaxEnergy;
        protected int Energy;
        protected bool Thirst;
        protected double Luck;
        protected (int x, int y) Coordinates;

        // CORRECT
        public Player(int maxEnergy, int x, int y)
        {
            MaxEnergy = maxEnergy;
            Energy = maxEnergy;
            Thirst = true;
            Luck = 1.0;
            Coordinates = (x, y);
        }

        // CORRECT
        public double GetLuck()
        {
            return Luck;
        }

        // CORRECT
        public void SetLuck(double luck)
        {
            Luck = luck;
        }

        // CORRECT
        public (int x, int y) GetCoordinates()
        {
            return Coordinates;
        }

        // CORRECT
        public int GetEnergy()
        {
            return Energy;
        }

        // CORRECT
        public bool GetThirst()
        {
            return Thirst;
        }

        // FIXED
        public virtual void Move(Game game, char key)
        {
            if (Energy <= 0) return;
            
            (int posX, int posY) = Coordinates;

            switch (key)
            {
                case 'w':
                    posY--;
                    break;
                case 's':
                    posY++;
                    break;
                case 'a':
                    posX--;
                    break;
                case 'd':
                    posX++;
                    break;
            }

            if (!(posX >= 0 && posY >= 0 &&
                  posX < game.GetBoard().GetLength(0) &&
                  posY < game.GetBoard().GetLength(1)))
                return;
            
            Cell cell = game.GetBoard()[posX, posY];

            if (cell is Sea)
                return;

            Coordinates = (posX, posY);

            if (Energy - cell.GetMoveCost() < 0) 
                return;
            Energy -= cell.GetMoveCost();
        }

        // CORRECT
        public bool SpendTheNight()
        {
            Energy -= 2;
            Console.WriteLine("You spend the night on the ground.");
            
            if (Energy > 0 && !Thirst)
            {
                Thirst = true;
                return true;
            }

            Thirst = true;
            return false;
        }
        
        // CORRECT
        public bool Eat(Item food)
        {
            if (food == null)
            {
                throw new ArgumentNullException("There is no food to eat.", new ArgumentNullException());
            }

            if (Energy + food.GetEnergyAmount() > MaxEnergy) 
                return false;

            Energy += food.GetEnergyAmount();
            return true;
        }
        
        // CORRECT
        public void Drink()
        {
            Thirst = false;
        }

        // CORRECT
        public override string ToString()
        {
            return "^";
        }
    }
}
