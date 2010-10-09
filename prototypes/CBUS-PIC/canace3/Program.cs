                                                                      using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace interlock
{
    class Program
    {
        static string[] rpn = new string[121];
        static string[] eqn = new string[121];
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
            else if (inputfile[inputptr] == 'O' && inputfile[inputptr + 1] == 'R')
            {
                nexttoken = '|';
                inputptr += 2;
            }
            else if (inputfile[inputptr] == 'M' && inputfile[inputptr + 1] == 'S' && inputfile[inputptr + 2] == 'L')
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
            inputfile = sr.ReadToEnd().ToUpper();
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

        static string LeverString(int l, char nr)
        {
            if (l <= 0 && l >= 121)
                Error(20, "Lever number out of range");
            return l.ToString() + nr.ToString();
        }

        static void AddTerm(int lever, string r, string e)
        {
            int l = rpn[lever].Length;
            rpn[lever] += r;
            if (l != 0)
            {
                rpn[lever] += "|";
                eqn[lever] += " or";
            }
            if (e.Length <= 4)
                eqn[lever] += " " + e;
            else
                eqn[lever] += " (" + e +")";
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
            pexp_ilock = 'N';
            pexp_nlock = 'R';
            if (nexttoken == 'R')
            {
                token();
                pexp_ilock = 'N';
                pexp_nlock = 'R';
            }
            else if (nexttoken == 'N')
            {
                token();
                pexp_ilock = 'R';
                pexp_nlock = 'N';
            }
        }

        // *****************************************************************************************************
        // Release by
        // *****************************************************************************************************

        static void releasewith(int lever, bool afterprimary)
        {
            string r = "", e = "";
            if (!afterprimary)
                primary();
            int alever = pexp_lever;
            char anlock = pexp_nlock;
            char ailock = pexp_ilock;
            AddTerm(alever, LeverString(lever, 'R')+LeverString(alever,anlock)+"&",
                LeverString(lever, 'R') + " and " + LeverString(alever,anlock));

            if (nexttoken != 'W')
                Error(2, "W");
            r += LeverString(alever, ailock);
            e += LeverString(alever, ailock);
            token();
            do
            {
                primary();
                r += LeverString(pexp_lever, pexp_ilock) + "|";
                e += " or " + LeverString(pexp_lever, pexp_ilock);
                AddTerm(pexp_lever, LeverString(lever, 'R') + LeverString(alever, anlock) + "&" + LeverString(pexp_lever, pexp_nlock) + "&",
                    LeverString(lever, 'R') + " and " + LeverString(alever, anlock) + " and " + LeverString(pexp_lever, pexp_nlock));
                if (nexttoken == '.')
                    token();
            } while (nexttoken >= 1000);
            AddTerm(lever, r, e);
        }
        
        static void releaseorend(int lever, string r, string e)
        {
            bool repeat = false;
            do
            {
                repeat = false;
                if (nexttoken == '(')
                {
                    token();
                    primary();
                    releasewith(lever,true);
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
                    r += LeverString(pexp_lever, pexp_ilock) + "&";
                    e += " and " + LeverString(pexp_lever, pexp_ilock);
                    AddTerm(pexp_lever, LeverString(lever, 'R') + LeverString(pexp_lever, pexp_nlock) + "&",
                        LeverString(lever, 'R') + " and " + LeverString(pexp_lever, pexp_nlock));
                    if (nexttoken == '|')
                    {
                        repeat = true;
                        token();
                    }
                }
            } while (repeat);
            AddTerm(lever, r, e);
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
                        releasewith(lever, true);
                        if (nexttoken != ')')
                            Error(33, ") to end with");
                        token();
                        if (nexttoken != '|')
                            Error(34, "or");
                        token();
                        releaseorend(lever, LeverString(pexp_lever, pexp_ilock), LeverString(pexp_lever, pexp_ilock));
                    }
                    else // ( p ?
                    {
                        primary();
                        if (nexttoken == ')') // ( p )
                        {
                            AddTerm(lever, LeverString(pexp_lever, pexp_ilock), LeverString(pexp_lever, pexp_ilock));
                            AddTerm(pexp_lever, LeverString(lever, 'R') + LeverString(pexp_lever, pexp_nlock) + "&",
                                 LeverString(lever, 'R') + " and " + LeverString(pexp_lever, pexp_nlock));
                        }
                        else if (nexttoken == '@') // (p MSL)
                        {
                            AddTerm(lever, LeverString(pexp_lever, pexp_ilock), LeverString(pexp_lever, pexp_ilock));
                            token();
                        }
                        else if (nexttoken == '|') // ( p | ... )
                        {
                            AddTerm(pexp_lever, LeverString(lever, 'R') + LeverString(pexp_lever, pexp_nlock) + "&",
                                 LeverString(lever, 'R') + " and " + LeverString(pexp_lever, pexp_nlock));
                            token();
                            releaseorend(lever, LeverString(pexp_lever, pexp_ilock), LeverString(pexp_lever, pexp_ilock));
                        }
                        else if (nexttoken == 'W') // ( p W p ... )
                        {
                            releasewith(lever, true);
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
                    AddTerm(lever, LeverString(pexp_lever, pexp_ilock), LeverString(pexp_lever, pexp_ilock));
                    AddTerm(pexp_lever, LeverString(lever, 'R') + LeverString(pexp_lever, pexp_nlock) + "&",
                         LeverString(lever, 'R') + " and " + LeverString(pexp_lever, pexp_nlock));
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
            if (nexttoken == 'X')
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
                        AddTerm(pexp_lever, LeverString(lever, 'R') + LeverString(pexp_lever, 'N') + "&",
                            LeverString(lever, 'R') + " and " + LeverString(pexp_lever, 'N'));
                        // AddTerm(lever, LeverString(pexp_lever, 'R') + LeverString(lever, 'N') + "&",
                        //    LeverString(pexp_lever, 'R') + " and " + LeverString(lever, 'N'));
                        token();
                    }
                    else if (nexttoken == 'W')
                    {
                        int alever = pexp_lever;
                        string r = LeverString(lever, 'R') + LeverString(alever, 'N') + "&";
                        string e = LeverString(lever, 'R') + " and " + LeverString(alever, 'N');
                        string rc = LeverString(alever, 'R') + LeverString(lever, 'N') + "&";
                        string ec = LeverString(alever, 'R') + " and " + LeverString(lever, 'N');
                        token();
                        do
                        {
                            if (nexttoken == '.')
                                token();
                            primary();
                            r += LeverString(pexp_lever, pexp_nlock) + "&";
                            e += " and " + LeverString(pexp_lever, pexp_nlock);
                            rc += LeverString(pexp_lever, pexp_nlock) + "&";
                            ec += " and " + LeverString(pexp_lever, pexp_nlock);

                        } while (nexttoken == '.' || nexttoken >= 1000);
                        AddTerm(alever, r, e);
                        // AddTerm(lever, rc, ec);
                        if (nexttoken == ')')
                            token();
                        else
                            Error(12, ")");
                    }
                }
                else if (nexttoken >= 1000)
                {
                    primary(); // get 1st locks normal lever
                    AddTerm(pexp_lever, LeverString(lever, 'R') + LeverString(pexp_lever, 'N') + "&",
                        LeverString(lever, 'R') + " and " + LeverString(pexp_lever, 'N'));
                    // AddTerm(lever, LeverString(pexp_lever, 'R') + LeverString(lever, 'N') + "&",
                    //    LeverString(pexp_lever, 'R') + " and " + LeverString(lever, 'N'));
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
                        AddTerm(pexp_lever, LeverString(lever, 'R'), LeverString(lever, 'R'));
                    }
                    else if (nexttoken == 'W')
                    {
                        int alever = pexp_lever;
                        string r = "";
                        string e = "";
                        r += LeverString(lever, 'R');
                        e += LeverString(lever, 'R');
                        token();
                        do
                        {
                            if (nexttoken == '.')
                                token();
                            primary();
                            r += LeverString(pexp_lever, pexp_nlock) + "&";
                            e += " and " + LeverString(pexp_lever, pexp_nlock);
                        } while (nexttoken == '.' || nexttoken >= 1000);
                        AddTerm(alever, r, e);
                        if (nexttoken == ')')
                            token();
                        else
                            Error(13, ")");
                    }
                }
                else if (nexttoken >= 1000)
                {
                    primary(); // get 1st locks normal lever
                    AddTerm(pexp_lever, LeverString(lever, 'R'), LeverString(lever, 'R'));
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
        // convert rpn strings
        // *****************************************************************************************************

        static string RpnToHex(string s)
        {
            string o = "";
            for (int i = 0; i < s.Length; i++)
            {
                if (s[i] >= '0' && s[i] <= '9')
                {
                    int n = 0;
                    while (s[i] >= '0' && s[i] <= '9')
                    {
                        n = n * 10 + s[i] - '0';
                        i++;
                    }
                    if (s[i] == 'N')
                        n += 0;
                    else if (s[i] == 'R')
                        n += 128;
                    else
                        Error(98, "Illegal character in rpn string");
                    o += (char)n;
                }
                else if (s[i] == '&')
                    o += (char)0xFF;
                else if (s[i] == '|')
                    o += (char)0x7F;
                else
                    Error(99, "Illegal character in rpn string");
            }
            return o + '\0';
        }

        static string RpnToHex2(string s)
        {
            string o = "";
            for (int i = 0; i < s.Length; i++)
            {
                if (s[i] >= '0' && s[i] <= '9')
                {
                    int n = 0;
                    while (s[i] >= '0' && s[i] <= '9')
                    {
                        n = n * 10 + s[i] - '0';
                        i++;
                    }
                    if (s[i] == 'N')
                        n += 0;
                    else if (s[i] == 'R')
                        n += 128;
                    else
                        Error(98, "Illegal character in rpn string");
                    o += @"\x"+String.Format("{0:X2}", n);
                }
                else if (s[i] == '&')
                    o += @"\xFF";
                else if (s[i] == '|')
                    o += @"\x7F";
                else
                    Error(99, "Illegal character in rpn string");
            }
            return o;
        }

        // *****************************************************************************************************
        // emulation
        // *****************************************************************************************************

        static bool Locked(int lever)
        {
            bool[] stack = new bool[10];
            int sp = 0;
            int c = 0;
            stack[0] = false;
            string s = RpnToHex(rpn[lever]);
            int i = 0;
            while ((c = s[i++]) != 0)
            {
                if (c == 0xFF)
                {
                    sp--;
                    stack[sp-1] &= stack[sp];
                }
                else if (c == 0x7F)
                {
                    sp--;
                    stack[sp-1] |= stack[sp];
                }
                else if ((c&0x80)==0x80) 
                    stack[sp++] = states[c&0x7F];
                else
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
                eqn[i] = "";
            }
            
            ReadFile(args[0]);
            ProcessFile();

            StreamWriter sw = new StreamWriter(args[1]);

            sw.WriteLine("// Locking");
            for (i = 1; i <= highestlever; i++)
            {
                if (eqn[i].Length > 0)
                    sw.WriteLine("// Locked " + i.ToString() + ":\t" + eqn[i]);
                else
                    sw.WriteLine("// Locked " + i.ToString() + ":\tNever");
                //if (rpn[i].Length > 0)
                //    sw.WriteLine("// " + i.ToString() + ":\t" + rpn[i]);
            }
            sw.WriteLine("");
            sw.WriteLine("// Reverse polish logic for locking");
            sw.WriteLine("// 0xFF = and, 0x7F = or, 0x00 marks the end of the equation");
            sw.WriteLine("// 1-120 = lever normal, add 128 for Reversed.");
            sw.WriteLine("BYTE * rom locking[" + (highestlever + 1).ToString() + "] = {");
            for (i = 0; i <= highestlever; i++)
            {
                if (rpn[i].Length > 0)
                    sw.WriteLine("    \"" + RpnToHex2(rpn[i])+"\", // " + i.ToString());
                else
                    sw.WriteLine("    \"\", // " + i.ToString());
            }
            sw.WriteLine("};");
            sw.Close();

            emulate();
        }
    }
}
