namespace Survivor
{
    public class Demeter : God
    {
        public Demeter(Item favourite, Item hated, int maxPatience)
        :base(favourite, hated, maxPatience)
        {
            
        }
        
        public override void Miracle(Game game)
        {
            game.SetSpawnRate(game.GetSpawnRate() + 5);
            Patience = MaxPatience;
        }

        public override void Disaster(Game game)
        {
            game.SetSpawnRate(game.GetSpawnRate() - 3);
            Patience = MaxPatience;
        }
        
        public override string ToString()
        {
            return $"{GetType().Name} : {Patience}";
        }
    }
}

