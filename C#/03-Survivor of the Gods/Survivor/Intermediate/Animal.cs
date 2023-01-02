namespace Survivor
{
    public class Animal: Item
    {
        private bool _isDead;
        
        public Animal(int energyAmount)
        :base(1, energyAmount, false)
        {
            _isDead = false;
        }

        public void Kill()
        {
            _isDead = true;
            IsEdible = true;
        }
        
        public override void Update()
        {
            Expiry--;
        }

        public override string ToString()
        {
            return _isDead ? "D" : "A";
        }
    }
}