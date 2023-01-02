namespace Survivor
{
    public class Zeus : God
    {
        public Zeus(Item favourite, Item hated, int maxPatience)
        :base(favourite, hated, maxPatience)
        {
            
        }

        public override void Miracle(Game game)
        {
            foreach (Cell cell in game.GetBoard())
            {
                Item content = cell.GetContent();
                if (content != null! && content is Tree tree && tree.GetGrowth() > 0)
                {
                    tree.SetGrowth(tree.GetGrowth() - 1);
                }
            }

            Patience = MaxPatience;
        }
        
        public override void Disaster(Game game)
        {
            foreach (Cell cell in game.GetBoard())
            {
                Item content = cell.GetContent();
                if (content != null! && content is Tree)
                {
                    cell.SetContent(null!);
                }
            }

            Patience = MaxPatience;
        }

        public override string ToString()
        {
            return $"{GetType().Name} : {Patience}";
        }
    }
}
