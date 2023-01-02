namespace Survivor
{
    public class PoseidonAdvanced : Poseidon
    {
        public PoseidonAdvanced(Item favourite, Item hated, int maxPatience)
        :base(favourite, hated, maxPatience)
        {
            
        }
        
        public override void Miracle(Game game)
        {
            GameAdvanced ga = (GameAdvanced)game;
            ga.SetViewRange(ga.GetViewRange() + 1);
            
            Patience = MaxPatience;
        }
    }
}