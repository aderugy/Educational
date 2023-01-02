namespace Survivor
{
    public class Coconut : Item
    {
        public Coconut(int expiry, int energyAmount) 
            : base(expiry, energyAmount, true)
        {
            
        }
        
        public override void Update()
        {
            Expiry--;
        }
        
        public override string ToString()
        {
            return "C";
        }
    }
}