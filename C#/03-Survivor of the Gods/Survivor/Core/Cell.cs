namespace Survivor
{
    public abstract class Cell : IUpdate
    {
        protected Item Content;
        protected int SpawnRange;
        protected int MoveCost;

        protected Cell(int moveCost, int spawnRange)
        {
            MoveCost = moveCost;
            SpawnRange = spawnRange;
            Content = null!;
        }

        public Item GetContent()
        {
            return Content;
        }
        
        public void SetContent(Item item)
        {
            Content = item;
        }
        
        public int GetSpawnRange()
        {
            return SpawnRange;
        }
        
        public int GetMoveCost()
        {
            return MoveCost;
        }

        public void SetMoveCost(int cost)
        {
            MoveCost = cost;
        }

        public virtual void Update()
        {
            if (Content == null!) return;
            
            Content.Update();
            if (Content.GetExpiry() <= 0)
                Content = null!;
        }

        public override string ToString()
        {
            return MoveCost.ToString();
        }
    }
}