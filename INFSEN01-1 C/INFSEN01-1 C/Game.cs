extern alias Func;

using System;
using System.Threading;


namespace INFSEN01_1
{
    class GameWrapper
    {
        private const int SLEEP = 2000;
        private Func::Game.State state;
        private Thread thread;

        public GameWrapper(string file, string monsterNamesFile, string monsterPrefixesFile)
        {
            string map = System.IO.File.ReadAllText(file);
            string[] monsterNames = System.IO.File.ReadAllText(monsterNamesFile).Replace("\r", string.Empty).Split('\n');
            string[] monsterPrefixes= System.IO.File.ReadAllText(monsterPrefixesFile).Replace("\r", string.Empty).Split('\n');
            state = Func::Game.createState(ConvertMap(map.Replace("\r", string.Empty).Split('\n')), Func::Game.createPlayer(0, 0, Func::Game.Direction.south), monsterPrefixes, monsterNames);
        }

        private char[][] ConvertMap(string[] map)
        {
            char[][] o = new char[map.Length][];
            for(int i = 0; i < map.Length; i++)
            {
                o[i] = map[i].ToCharArray();
            }
            return o;
        }

        public void Run()
        {
            startBackgroundThread();
            while(true)
            {
                string line = Console.ReadLine().ToLower().Trim();
                var output = Func::Game.parseCommand(line, state);
                string[] strippedOutput = output.Item1.Split('\n');
                foreach(string o in strippedOutput)
                {
                    Console.WriteLine(o);
                }
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
