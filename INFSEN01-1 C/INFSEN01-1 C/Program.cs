﻿namespace INFSEN01_1
{
    class Program
    {
        static void Main(string[] args)
        {
            new GameWrapper(args[0], args[1], args[2]).Run();
        }
    }
}
