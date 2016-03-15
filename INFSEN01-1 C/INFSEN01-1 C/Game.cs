extern alias Func;

namespace INFSEN01_1
{
    class GameWrapper
    {
        private Func::Game.State state;

        public GameWrapper(string file)
        {
            string map = System.IO.File.ReadAllText(file);
            state = new Func::Game.State(map.Split('\n'), new Func::Game.Object(0, 0), true);
        }

        public void Run()
        {
            while(true)
            {
                string line = System.Console.ReadLine();
                var output = Func::Game.parseCommand(line, state);
                System.Console.WriteLine(output.Item1);
                state = output.Item2;
                if (!state.running)
                {
                    break;
                }
            }
        }
    }
}
