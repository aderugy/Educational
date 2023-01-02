namespace Survivor
{
    public class Hypnos : God
    {
        public Hypnos(Item favourite, Item hated, int maxPatience)
        :base(favourite, hated, maxPatience)
        { 
            
        }

        public override void Miracle(Game game)
        {
            PlayerIntermediate p = (PlayerIntermediate) game.GetPlayer();
            p.SetEnergy(p.GetEnergy() + 5);
            Patience = MaxPatience;
        }

        public override void Disaster(Game game)
        {
            PlayerIntermediate p = (PlayerIntermediate) game.GetPlayer();
            p.SetEnergy(p.GetEnergy() - 3);
            Patience = MaxPatience;
        }
        
        public override string ToString()
        {
            return $"{GetType().Name} : {Patience}";
        }
    }
}

