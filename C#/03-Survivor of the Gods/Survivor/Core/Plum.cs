namespace Survivor
{
    public class Plum : Item
    {
        public Plum(int expiry, int energyAmount)
            : base(expiry, energyAmount, true)
        {
            
        }

        public override void Update()
        {
            Expiry--;
        }
        
        public override string ToString()
        {
            return "P";
        }
    }
}