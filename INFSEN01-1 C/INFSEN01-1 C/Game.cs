extern alias Func;

using System;
using System.Threading;

namespace INFSEN01_1
{
    class GameWrapper
    {
        private const int SLEEP = 1000;
        private Func::Game.State state;
        private Thread thread;

        public GameWrapper(string file)
        {
            string map = System.IO.File.ReadAllText(file);
            state = new Func::Game.State(map.Split('\n'), new Func::Game.Object(0, 0, Func::Game.Direction.south), true, false);
        }

        public void Run()
        {
            //startBackgroundThread();
            while(true)
            {
                string line = Console.ReadLine();
                var output = Func::Game.parseCommand(line, state);
                Console.WriteLine(output.Item1);
                state = output.Item2;
                if (!state.running)
                {
                    break;
                }
            }
        }

        private void startBackgroundThread()
        {
            thread = new Thread(new ThreadStart(RunBackgroundThread));
            thread.Start();
        }

        private void RunBackgroundThread()
        {
            while (state.running)
            {
                if (!state.paused)
                {
                    state = Func::Game.runFrame(state);
                }
                Thread.Sleep(SLEEP);
            }
        }
    }
}
