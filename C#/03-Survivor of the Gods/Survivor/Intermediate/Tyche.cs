namespace Survivor
{
    public class Tyche : God
    {
        public Tyche(Item favourite, Item hated, int maxPatience)
            :base(favourite, hated, maxPatience)
        {
            
        }

        public override void Miracle(Game game)
        {
            game.GetPlayer().SetLuck(game.GetPlayer().GetLuck() + 0.25);
            Patience = MaxPatience;
        }
        
        public override void Disaster(Game game)
        {
            game.GetPlayer().SetLuck(game.GetPlayer().GetLuck() - 0.25);
            Patience = MaxPatience;
        }
        
        public override string ToString()
        {
            return $"{GetType().Name} : {Patience}";
        }
    }
}

