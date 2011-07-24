using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace HexMerge
{
    class Program
    {

        static SortedList<UInt32,String> memdata = new SortedList<UInt32,String>();

        static private UInt32 hv(char a, char b)
        {
            if (a >= 'a') a = (char)(a - 'a' + 10);
            else if (a >= 'A') a = (char)(a - 'A' + 10);
            else a = (char)(a - '0');
            if (b >= 'a') b = (char)(b - 'a' + 10);
            else if (b >= 'A') b = (char)(b - 'A' + 10);
            else b = (char)(b - '0');
            return (UInt32)(a * 16 + b);
        }

        static public string hex2(int a)
        {
            return String.Format("{0:X2}", a & 0x00FF);
        }

        static void ReadFile(string fileName)
        {
            System.IO.StreamReader sr = System.IO.File.OpenText(fileName);
            String f = sr.ReadToEnd().ToUpper();
            sr.Close();
            int l = f.Length;

            // read intel hex file
            UInt32 address = 0;
            int startptr;
            int i = 0;
            while(i < l)
            {
                startptr = i;
                if (f[i] == ':')
                {
                    i++;
                    // length
                    UInt32 recordlength = hv(f[i], f[i+1]);
                    UInt32 checksum = recordlength; 
                    i+=2;
                    // address high
                    UInt32 ra = hv(f[i], f[i + 1]);
                    i+=2;
                    checksum += ra;
                    // address low
                    UInt32 t = hv(f[i], f[i+1]);
                    i+=2;
                    checksum += t;
                    ra = ra * 256 + t;
                    address = (address & 0xFFFF0000) | (UInt32)ra;
                    // type
                    UInt32 rt = hv(f[i], f[i + 1]);
                    checksum += rt;
                    i+=2;
                    // calc checksum
                    for (int j = 0; j <= recordlength; j++)
                    {
                        checksum += hv(f[i+j*2], f[i+j*2 + 1]);
                    }
                    if ((checksum & 0xFF) != 0)
                    {
                        Console.WriteLine(fileName + " has a checksum error"); // check error
                        return;
                    }

                    if (rt == 0) // data record
                    {
                        try
                        {
                            memdata.Add(address, f.Substring((int)startptr, (int)recordlength * 2 + 11));
                        }
                        catch (ArgumentException)
                        {
                            if (memdata[address] != f.Substring((int)startptr, (int)recordlength * 2 + 11))
                            {
                                Console.WriteLine(String.Format("{0:X6}", address)
                                    + " Already exists:\r\n" + memdata[address]
                                    + "\r\n" + f.Substring((int)startptr, (int)recordlength * 2 + 11));
                            }
                        }
                    }
                    else if (rt == 1) // end of file
                        break;
                    else if (rt == 4) // high address bits
                    {
                        t = hv(f[i], f[i + 1]) * 256 + hv(f[i+2], f[i + 3]);
                        i += 4;
                        address = t<<16;
                    }
               }
                else i++;
            }

            // display program memory info
            long st = 0;
            long en = 0;
            i = 0;
            while(i < memdata.Count)
            {
                st = memdata.Keys[i];
                string rec = memdata.Values[i];
                en = st + hv(rec[1],rec[2]);
                i++;
                while (i < memdata.Count && en == memdata.Keys[i])
                {
                   rec = memdata.Values[i];
                   en += hv(rec[1],rec[2]);
                   i++;
                }
                Console.WriteLine("Program memory from " + String.Format("{0:X4}", st) + " to " + String.Format("{0:X4}", en-1));
            }
        }

        static void WriteFile(string filename, string comment)
        {
            StreamWriter sw = new StreamWriter(filename);
            UInt32 address = 0;
            // high address record
            String s = ":02000004" + String.Format("{0:X4}", (int)(address >> 16))
                + String.Format("{0:X2}",(0-(2+4+(address>>16)+(address>>24)))&0xFF);
            sw.WriteLine(s);
            for (int i = 0; i<memdata.Count; i++) {
                // high asddress change
                if ((address&0xFFFF0000) != (memdata.Keys[i]&0xFFFF0000)) {
                    address = memdata.Keys[i];
                    s = ":02000004" + String.Format("{0:X4}", (int)(address >> 16))
                        + String.Format("{0:X2}",(0-(2+4+(address>>16)+(address>>24)))&0xFF);
                    sw.WriteLine(s);
                }
                sw.WriteLine(memdata.Values[i]);
            }
            sw.WriteLine(":00000001FF");
            sw.WriteLine(comment + " " + DateTime.Now.ToLongDateString() + " " + DateTime.Now.ToLongTimeString());
            sw.Close();
        }

        static void Main(string[] args)
        {
            if (args.GetUpperBound(0) < 3)
            {
                Console.WriteLine("Use: Hexmerg file1 file2 file3 comment");
                Console.WriteLine("      file1 is an Intel hex file.");
                Console.WriteLine("      file2 is a second Intel hex file.");
                Console.WriteLine("      file3 is an Intel hex file produced by sorting");
                Console.WriteLine("          and merging the first 2 files.");
                Console.WriteLine("      comment is a text record added to the file.");

                return;
            }

            // clear memory image
            ReadFile(args[0]);
            ReadFile(args[1]);
            WriteFile(args[2], args[3]);
        }
    }
}
