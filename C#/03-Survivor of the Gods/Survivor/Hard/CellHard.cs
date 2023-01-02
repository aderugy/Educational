namespace Survivor
{
    public class CellHard : Cell
    {
        private int _ulysses;
        public CellHard()
        :base(0, 0)
        {        
            
        }
        
        public int GetUlysses()
        {
            return _ulysses;
        }

        public void SetUlysses(int ulysses)
        {
            _ulysses = ulysses;
        }
        
        public override void Update()
        {
            if (_ulysses > 0) _ulysses--;
        }

        public override string ToString()
        {
            return _ulysses > 0 ? "U" : " ";
        }
    }
}