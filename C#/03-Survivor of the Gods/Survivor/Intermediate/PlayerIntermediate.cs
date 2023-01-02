using System;

namespace Survivor
{
    public class PlayerIntermediate : Player
    {
        protected Item[] Inventory; 
        protected readonly int SizeInventory;
        
        // CORRECT
        public PlayerIntermediate(int maxEnergy, int x, int y)
        :base(maxEnergy, x, y)
        {
            SizeInventory = 10;
            Inventory = new Item[SizeInventory];
        }
        
        // CORRECT
        public void SetEnergy(int energy)
        {
            Energy = energy;
        }

        // CORRECT
        public Item[] GetInventory()
        {
            return Inventory;
        }
        
        // CORRECT
        private static int SelectItem()
        {
            while (true)
            {
                Console.WriteLine("Choose a slot to select (0-9) or 'q' to cancel.");
                char input = (char) Console.Read();
                const string digits = "0123456789";

                if (digits.Contains(input))
                    return digits.IndexOf(input);
                if (input == 'q')
                    return -1;
            }
        }
        
        // CORRECT
        public void PickUp(Cell[,] board)
        {
            Cell cell = board[Coordinates.x, Coordinates.y];
            if (cell.GetContent() == null!)
            {
                throw new ArgumentNullException(
                    "The cell is empty.", new ArgumentNullException());
            }

            for (int i = 0; i < Inventory.Length; i++)
            {
                if (Inventory[i] == null)
                {
                    Inventory[i] = cell.GetContent();
                    cell.SetContent(null);
                }
            }
        }

        // WRONG
        public void Drop(Cell[,] board)
        {
            Cell cell = board[Coordinates.x, Coordinates.y];
            if (cell.GetContent() != null) return;

            int input = SelectItem();
            if (input == -1 || Inventory[input] == null) return;
            
            cell.SetContent(Inventory[input]);
            Inventory[input] = null!;
        }
        
        // WRONG
        public void Sacrifice(Game game, God god)
        {
            int input = SelectItem();
            if (input < 0 || input >= Inventory.Length || Inventory[input] == null) return;
            god.ReceiveOffering(game, Inventory[input]);
            Inventory[input] = null;
        }
    }
}
