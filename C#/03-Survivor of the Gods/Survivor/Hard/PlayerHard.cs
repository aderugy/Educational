namespace Survivor
{
    public class PlayerHard : PlayerAdvanced
    {

        public PlayerHard(int maxEnergy, int x, int y)
        :base(maxEnergy, x, y)
        {
            
        }
        
        public override void Move(Game game, char key)
        {
            if (Energy <= 0) return;
            
            (int posX, int posY) = Coordinates;

            switch (key)
            {
                case 'w':
                    posY--;
                    break;
                case 's':
                    posY++;
                    break;
                case 'a':
                    posX--;
                    break;
                case 'd':
                    posX++;
                    break;
            }

            if (!(posX >= 0 && posY >= 0 &&
                  posX < game.GetBoard().GetLength(0) &&
                  posY < game.GetBoard().GetLength(1)))
                return;

            Cell cell = game.GetBoard()[posX, posY];

            Coordinates = (posX, posY);
            Energy -= cell.GetMoveCost();
        }
    }
}