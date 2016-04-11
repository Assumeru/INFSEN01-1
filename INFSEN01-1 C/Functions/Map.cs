namespace Functions
{
    public class Map
    {
        public static char[][] SetTile(char[][] array, int x, int y, char value)
        {
            char[][] output = (char[][])array.Clone();
            output[y] = (char[])output[y].Clone();
            output[y][x] = value;
            return output;
        }
    }
}
