namespace Survivor
{
    public class PlayerAdvanced : PlayerIntermediate
    {
        public PlayerAdvanced(int maxEnergy, int x, int y)
        :base(maxEnergy, x, y)
        {
           
        }
        
        public void Plant(Cell cell)
        {
            Item item = cell.GetContent();
            
            if (item != null! && item is Coconut or Banana or Plum)
                cell.SetContent(new Tree());
        }
    }
}
