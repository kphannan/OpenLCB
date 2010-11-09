                                                                      using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace interlock
{
    class Program
    {
        static string[] rpn = new string[121];
        static bool[] states = new bool[121];
        static string inputfile;
        static int inputptr;
        static int nexttoken;
        static int pexp_lever;
        static char pexp_nlock;
        static char pexp_ilock;
        static int highestlever;

        static SortedList<UInt32, String> memdata = new SortedList<UInt32, String>();

        static int token()
        {
            while (inputptr < inputfile.Length && (inputfile[inputptr] == ' ' || inputfile[inputptr] == '\t'
              || inputfile[inputptr] == '\r' || inputfile[inputptr] == '\'' || inputfile[inputptr] == '\"'))
            {
                    inputptr++;
            }

            if (inputptr >= inputfile.Length)
            {
                nexttoken = 0;
            }
            else if (inputfile[inputptr] >= '0' && inputfile[inputptr] <= '9')
            {
                int n = 0;
                while (inputptr < inputfile.Length && inputfile[inputptr] >= '0' && inputfile[inputptr] <= '9')
                {
                    n = n * 10 + (inputfile[inputptr++] - '0');
                }
                nexttoken = n + 1000;
            }
            else if (inputfile[inputptr] == 'o' && inputfile[inputptr + 1] == 'r')
            {
                nexttoken = '|';
                inputptr += 2;
            }
            else if (inputfile[inputptr] == 'm' && inputfile[inputptr + 1] == 's' && inputfile[inputptr + 2] == 'l')
            {
                nexttoken = '@';
                inputptr += 3;
            }
            else if (inputfile[inputptr] == '/')
            {
                nexttoken = ',';
                inputptr++;
            }
            else if (inputfile[inputptr] == '&')
            {
                nexttoken = '.';
                inputptr++;
            }
            else
            {
                nexttoken = inputfile[inputptr++];
            }
            return nexttoken;
        }

        static void ReadFile(string fileName)
        {
            System.IO.StreamReader sr = System.IO.File.OpenText(fileName);
            inputfile = sr.ReadToEnd().ToLower();
            inputptr = 0;
            sr.Close();
        }

        static void Error(int n, string s2)
        {
            string s1,s3 = "\'";
            for (int i = -10; i < 10; i++)
            {
                if (i==0)
                    s3 += " * ";
                if (inputptr+i >=0 && inputptr+i<inputfile.Length)
                    s3 += inputfile[inputptr + i];
            }
            s3 += '\'';
            if (nexttoken >= 1000)
                s1 = nexttoken.ToString();
            else if (nexttoken == 0)
                s1 = "end of file";
            else
                s1 = ((char)nexttoken).ToString();
            Console.WriteLine("Error " + n + " in line near " + s3 + " at \'" + s1 + "\', " + s2 + " expected.");
        }

        static void primary()
        {
            if (nexttoken < 1000)
            {
                Error(1, "lever number");
                token();
                return;
            }
            else if (nexttoken > 1120)
            {
                Error(1, "Lever number too high");
                token();
                return;
            }

            pexp_lever = nexttoken - 1000;
            token();
            pexp_ilock = 'r';
            pexp_nlock = 'n';
            if (nexttoken == 'r')
            {
                token();
                pexp_ilock = pexp_nlock = 'r';
            }
            else if (nexttoken == 'n')
            {
                token();
                pexp_ilock = pexp_nlock = 'n';
            }
        }

        // *****************************************************************************************************
        // Code Output
        // *****************************************************************************************************

        static void emitop(int lever, char op)
        {
            int endc = 0;
            if (rpn[lever].Length!=0)
                endc = rpn[lever][rpn[lever].Length-1];
            bool oper = (endc==0) || (endc==0xFF) || (endc==0x7F) || (endc==0xFE);

            if (op == '!') // optimize a NOT operation on a lever
            {
                if (!oper)
                {
                    rpn[lever] = rpn[lever].Substring(0,rpn[lever].Length-1) + (char)(endc ^ 0x80);
                    return;
                }
            }
            if (op == '&')
                endc = 0xFF;
            else if (op == '|')
                endc = 0x7F;
            else if (op == '!')
                endc = 0xFE;
            rpn[lever] += (char)endc;
         }

        static void emitlever(int lever, int l, char nr)
        {
            if (l <= 0 && l >= 121)
            {
                Error(20, "Lever number out of range");
                l = 120;
            }
            if (nr == 'r')
                l |= 0x80;
            rpn[lever] += (char)l;
        }

        // *****************************************************************************************************
        // Release by
        // *****************************************************************************************************

        static void releasewith(int lever)
        {
            int alever = pexp_lever;
            char anlock = pexp_nlock;
            char ailock = pexp_ilock;
            if (nexttoken != 'w')
                Error(2, "W");
            token();
            emitlever(alever, lever, 'r');
            emitlever(lever, alever, ailock);

            do
            {
                primary();
                emitlever(lever, pexp_lever, pexp_ilock);
                emitop(lever, '&');
                emitlever(alever, pexp_lever, pexp_nlock);
                emitop(alever, '&');
                emitlever(pexp_lever, lever, 'r');
                emitop(pexp_lever, '|');
                if (nexttoken == '.')
                    token();
            } while (nexttoken >= 1000);
            emitop(alever, '|');
        }
        
        static void releaseorend(int lever)
        {
            bool repeat = false;
            do
            {
                repeat = false;
                if (nexttoken == '(')
                {
                    token();
                    primary();
                    releasewith(lever);
                    if (nexttoken != ')')
                        Error(4, ")");
                    token();
                    if (nexttoken == '|')
                    {
                        repeat = true;
                        token();
                    }
                }
                else
                {
                    primary();
                    emitlever(lever, pexp_lever, pexp_ilock);
                    emitop(lever, '!');
                    emitop(lever, '&');
                    emitlever(pexp_lever, lever, 'r');
                    emitop(pexp_lever, '|');
                    if (nexttoken == '|')
                    {
                        repeat = true;
                        token();
                    }
                }
            } while (repeat);
        }

        static void releaseandexp(int lever)
        {
            bool repeat = false;
            do
            {
                repeat = false;
                if (nexttoken == '(')
                {
                    token();
                    if (nexttoken == '(') // (( p W p...) | ....)
                    {
                        token();
                        primary();
                        releasewith(lever);
                        if (nexttoken != ')')
                            Error(33, ") to end with");
                        token();
                        if (nexttoken != '|')
                            Error(34, "or");
                        token();
                        emitlever(lever, pexp_lever, pexp_ilock);
                        releaseorend(lever);
                        emitop(lever, '|');
                    }
                    else // ( p ?
                    {
                        primary();
                        if (nexttoken == ')') // ( p )
                        {
                            emitlever(lever, pexp_lever, pexp_ilock);
                            emitop(lever, '!');
                            emitop(lever, '|');
                            emitlever(pexp_lever, lever, 'r');
                            emitop(pexp_lever, '|');
                        }
                        else if (nexttoken == '@') // (p MSL)
                        {
                            emitlever(lever, pexp_lever, pexp_ilock);
                            emitop(lever, '!');
                            emitop(lever, '|');
                            token();
                        }
                        else if (nexttoken == '|') // ( p | ... )
                        {
                            emitlever(pexp_lever, lever, 'r');
                            emitop(pexp_lever, '|');
                            emitlever(lever, pexp_lever, pexp_ilock);
                            emitop(lever, '!');
                            token();
                            releaseorend(lever);
                            emitop(lever, '|');
                        }
                        else if (nexttoken == 'w') // ( p W p ... )
                        {
                            releasewith(lever);
                            emitop(lever, '|');
                        }
                        else
                        {
                            Error(5, ") | or W");
                        }
                    }
                    if (nexttoken == ')')
                        token();
                    else
                        Error(5, ")");
                }
                else if (nexttoken >= 1000)
                {
                    primary();
                    emitlever(lever, pexp_lever, pexp_ilock);
                    emitop(lever, '!');
                    emitop(lever, '|');
                    emitlever(pexp_lever, lever, 'r');
                    emitop(pexp_lever, '|');
                }
                if (nexttoken == '.')
                {
                    token();
                    repeat = true;
                }
            } while (repeat);
        }

        static void release(int lever)
        {
            if (nexttoken == 'x')
                token();
            if (nexttoken != ',' && nexttoken != '/' && nexttoken != '\n' && nexttoken != 0)
                releaseandexp(lever);
        }

        // *****************************************************************************************************
        // Locks normal
        // *****************************************************************************************************

        static void locksnormal(int lever)
        {
            bool repeat;
            
            if (nexttoken == 0 || nexttoken == ',' || nexttoken == '\n')
                return;

            do {
                if (nexttoken == '(')
                {
                    token();
                    primary();
                    if (nexttoken == ')')
                    {
                        emitlever(lever, pexp_lever, pexp_nlock);
                        emitop(lever, '!');
                        emitop(lever, '|');
                        token();
                    }
                    else if (nexttoken == 'w')
                    {
                        emitlever(lever, pexp_lever, pexp_nlock);
                        emitop(lever, '!');
                        token();
                        do
                        {
                            if (nexttoken == '.')
                                token();
                            primary();
                            emitlever(lever, pexp_lever, pexp_nlock);
                            emitop(lever, '&');
                        } while (nexttoken == '.' || nexttoken >= 1000);
                        emitop(lever, '|');
                        if (nexttoken == ')')
                            token();
                        else
                            Error(12, ")");
                    }
                }
                else if (nexttoken >= 1000)
                {
                    primary(); // get locks normal lever
                    emitlever(lever, pexp_lever, pexp_nlock);
                    emitop(lever, '!');
                    emitop(lever, '|');
                }
                else
                {
                    Error(6, "Lever or (");
                    token();
                }
                repeat = false;
                if (nexttoken == '.')
                {
                    repeat = true;
                    token();
                }
            } while (repeat);
        }

        // *****************************************************************************************************
        // Locks each way
        // *****************************************************************************************************

        static void lockseachway(int lever)
        {
            bool repeat;

            if (nexttoken == 0 || nexttoken == ',' || nexttoken == '\n')
                return;

            do
            {
                if (nexttoken == '(')
                {
                    token();
                    primary();
                    if (nexttoken == ')')
                    {
                        token();
                        emitlever(pexp_lever, lever, 'r');
                        emitop(pexp_lever, '|');
                    }
                    else if (nexttoken == 'w')
                    {
                        int alever = pexp_lever;
                        emitlever(alever, lever, 'r');
                        token();
                        primary();
                        emitlever(alever, pexp_lever, pexp_nlock);
                        emitop(alever, '&');
                        emitop(alever, '|');
                        emitlever(pexp_lever, lever, 'r');
                        emitop(pexp_lever, '|');
                        if (nexttoken == ')')
                            token();
                        else
                            Error(13, ")");
                    }
                }
                else if (nexttoken >= 1000)
                {
                    primary(); // get a locks both ways lever
                    emitlever(pexp_lever, lever, 'r');
                    emitop(pexp_lever, '|');
                }
                else
                {
                    Error(11, "Lever or (");
                    token();
                }
                repeat = false;
                if (nexttoken == '.')
                {
                    repeat = true;
                    token();
                }
            } while (repeat);
        }

        // *****************************************************************************************************
        // process the file
        // *****************************************************************************************************

        static void ProcessFile()
        {
            int lever = 0;
            // print first line
            int i = inputptr;
            string s = "";
            while (inputfile[i] != '\n')
                s += inputfile[i++];
            Console.WriteLine("> " + s);
            // skip first line
            token();
            while (nexttoken != '\n')
                token();
            // print 2nd line
            i = inputptr;
            s = "";
            while (inputfile[i] != '\n')
                s += inputfile[i++];
            Console.WriteLine("> " + s);
            // skip 2nd line
            token();
            while (nexttoken != '\n')
                token();

            while (nexttoken != 0)
            {
                // print the line
                i = inputptr; 
                s = "";
                while (i<inputfile.Length && inputfile[i] != '\n')
                    s += inputfile[i++];
                Console.WriteLine("> " + s);

                // process the line
                token();
                if (nexttoken == 0)
                    break;
                if (nexttoken < 1000)
                    Error(7, "Lever should be first thing on a line");
                else 
                    highestlever = lever = nexttoken - 1000;
                token();
                if (nexttoken != ',')
                    Error(8, "First column separator expected");
                token(); // skip ,
                release(lever);
                if (nexttoken != ',')
                    Error(9, "Second column separator expected");
                token(); // skip ,
                locksnormal(lever);
                if (nexttoken != ',')
                    Error(10, "Third column separator expected");
                token(); // skip ,
                lockseachway(lever);
                while (nexttoken != '\n' && nexttoken != 0)
                    token();
            }
        }

        // *****************************************************************************************************
        // emulation
        // *****************************************************************************************************

        static bool Locked(int lever)
        {
            bool[] stack = new bool[10];
            int sp = 1;
            int c = 0;
            stack[0] = false;
            string s = rpn[lever];
            int i = 0;
            while (i < s.Length)
            {
                c = s[i++];
                if (c == 0xFF) // AND op
                {
                    sp--;
                    stack[sp-1] &= stack[sp];
                }
                else if (c == 0x7F) // OR op
                {
                    sp--;
                    stack[sp - 1] |= stack[sp];
                }
                else if (c == 0xFE) // NOT op
                {
                    stack[sp - 1] = !stack[sp-1];
                }
                else if ((c & 0x80) == 0x80) // Reversed
                    stack[sp++] = states[c&0x7F];
                else // normal
                    stack[sp++] = !states[c];
            }
            return stack[0];
        }

        static void emulate()
        {
            int i;
            for (i=1; i<=highestlever; i++)
                states[i] = false;
            while(true) {
                // output lever states
                string s = "";
                string s2 = "";
                for (i = 1; i <= highestlever; i++)
                {
                    s += i.ToString("D2").Substring(0, 1);
                    s2 += i.ToString("D2").Substring(1, 1);
                }
                Console.WriteLine(s);
                Console.WriteLine(s2);
                s = "";
                for (i = 1; i <= highestlever; i++)
                    s += (states[i] ? "R" : "N");
                Console.WriteLine(s);
                s = "";
                for (i = 1; i <= highestlever; i++)
                    s += (Locked(i) ? "L" : "F");
                Console.WriteLine(s);
                // read lever to move or exit
                s = Console.ReadLine();
                int l = 0;
                i = 0;
                while (i<s.Length && s[i] >= '0' && s[i] <= '9')
                {
                    l = l * 10 + s[i] - '0';
                    i++;
                }
                if (l == 0) // exit ?
                    return;
                if (Locked(l))
                    Console.WriteLine(l.ToString() + " Locked");
                else {
                    states[l] = !states[l];
                    Console.WriteLine(l.ToString() + (states[l] ? " Reversed." : " Normal."));
                }
            }
        }
        
        // *****************************************************************************************************
        // main
        // *****************************************************************************************************

        static void Main(string[] args)
        {
            int i;
            highestlever = 0;
            if (args.GetUpperBound(0) < 1)
            {
                Console.WriteLine("Use: Interlock file1 file2");
                Console.WriteLine("     file1 is an Interlock table in CSV format.");
                Console.WriteLine("     file2 is an Intel hex file suitable to be downloaded to the CANACE3I.");
                return;
            }

            for (i = 0; i <= 120; i++)
            {
                rpn[i] = "";
            }
            
            ReadFile(args[0]);
            ProcessFile();

            StreamWriter sw = new StreamWriter(args[1]);

            sw.WriteLine("// Locking");
            for (i = 1; i <= highestlever; i++)
            {
                if (rpn[i].Length > 0)
                {
                    string s = "False ";
                    foreach (char c in rpn[i])
                    {
                        if (c == 0xFF)
                            s += "& ";
                        else if (c == 0x7F)
                            s += "| ";
                        else if (c == 0xFE)
                            s += "! ";
                        else if (((int)c&0x80) == 0x80)
                            s += ((int)c&0x7F).ToString() + "r ";
                        else
                            s += ((int)c).ToString() + "n ";
                    }
                    sw.WriteLine("// Locked " + i.ToString() + ":\t" + s);
                }
                else
                    sw.WriteLine("// Locked " + i.ToString() + ":\tFalse");
            }
            sw.WriteLine("");
            sw.WriteLine("// Reverse polish logic for locking");
            sw.WriteLine("// 0xFF = and, 0x7F = or, 0xFE = not, 0x00 marks the end of the equation");
            sw.WriteLine("// 1-120 = lever normal, add 128 for Reversed.");
            sw.WriteLine("BYTE * rom locking[" + (highestlever + 1).ToString() + "] = {");
            for (i = 0; i <= highestlever; i++)
            {
               string s = "";
               if (rpn[i].Length > 0)
               {
                   foreach (char c in rpn[i])
                       s += @"\x" + String.Format("{0:X2}", (int)c);
                   sw.WriteLine("    \"" + s + "\", // " + i.ToString());
               }
               else
                   sw.WriteLine("    \"\", // " + i.ToString());
            }
            sw.WriteLine("};");
            sw.Close();

            emulate();
        }
    }
}
