namespace Survivor
{
    public class Poseidon : God
    {
        public Poseidon(Item favourite, Item hated, int maxPatience)
        :base(favourite, hated, maxPatience)
        {
            
        }

        public override void Miracle(Game game)
        {
            int x = game.GetRandom().Next(0, game.GetBoard().GetLength(0));
            int y = game.GetRandom().Next(0, game.GetBoard().GetLength(1));

            if (game.GetBoard()[x, y] is Forest)
                game.GetBoard()[x, y] = new River(2, 3);

            Patience = MaxPatience;
        }
        
        public override void Disaster(Game game)
        {
            ((GameIntermediate) game).IncreaseDaysLeft(1);
            Patience = MaxPatience;
        }
        public override string ToString()
        {
            return $"{GetType().Name} : {Patience}";
        }
    }
}
