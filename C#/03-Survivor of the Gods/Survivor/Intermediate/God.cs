using System;

namespace Survivor
{
    public abstract class God: IUpdate
    {
        protected Item Favourite;
        protected Item Hated;
        protected int Patience;
        protected int MaxPatience;

        protected God(Item favourite, Item hated, int maxPatience)
        {
            Favourite = favourite;
            Hated = hated;
            MaxPatience = maxPatience;
            Patience = maxPatience;
        }

        public int GetPatience()
        {
            return Patience;
        }
        
        public abstract void Miracle(Game game);
        public abstract void Disaster(Game game);
        
        public void ReceiveOffering(Game game, Item item)
        {
            if (item == null) throw new ArgumentNullException();
            if (item.GetType() == Favourite.GetType()) Miracle(game);
            if (item.GetType() == Hated.GetType()) Disaster(game);
        }

        public void Update()
        {
            Patience--;
        }
    }
}