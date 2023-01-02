namespace Survivor
{
    public class HypnosAdvanced : Hypnos
    {
        public HypnosAdvanced(Item favourite, Item hated, int maxPatience)
        :base(favourite, hated, maxPatience)
        {
            
        }
        
        public override void Disaster(Game game)
        {
            foreach (Cell cell in game.GetBoard())
            {
                cell.SetMoveCost(cell.GetMoveCost() + 1);
            }
            
            Patience = MaxPatience;
        }
    }
}