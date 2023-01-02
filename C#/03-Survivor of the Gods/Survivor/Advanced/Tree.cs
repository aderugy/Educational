namespace Survivor
{
    // CORRECT
    public class Tree : Item 
    {
        private int _growth;
        
        public Tree()
            :base(15, 0, false)
        {
            _growth = 10;
        }
        
        public int GetGrowth()
        {
            return _growth;

        }
        
        public void SetGrowth(int growth)
        {
            _growth = growth;

        }
        
        public override void Update()
        {
            Expiry--;
            if (_growth > 0)
                _growth--;
        }

        public override string ToString()
        {
            return "T";
        }
    }
}