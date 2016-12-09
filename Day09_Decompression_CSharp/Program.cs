using System;
using System.Linq;
using System.Numerics;
using System.IO;

namespace Day09_Decompression_CSharp
{
    class Program
    {
        static BigInteger explodeRecursive(char[] chars, BigInteger n)
        {
            BigInteger count = 0;
            for (int i = 0; i < chars.Length; i++)
            {
                if (chars[i] == '(')
                {
                    var marker = new String(chars.Skip(i + 1).TakeWhile(ch => ch != ')').ToArray());
                    var arr = marker.Split('x');
                    int nchars = Int32.Parse(arr[0]);
                    int skip = i + marker.Length + 2;
                    count += explodeRecursive(chars.Skip(skip).Take(nchars).ToArray(), Int32.Parse(arr[1]));
                    i = skip + nchars - 1;
                }
                else
                    count++;
            }
            return count * n;
        }

        static int explode(char[] chars)
        {
            int count = 0;
            for (int i = 0; i < chars.Length; i++)
            {
                if (chars[i] == '(')
                {
                    var marker = new String(chars.Skip(i + 1).TakeWhile(ch => ch != ')').ToArray());
                    var arr = marker.Split('x');
                    int nchars = Int32.Parse(arr[0]);
                    int skip = i + marker.Length + 2;
                    count += nchars * Int32.Parse(arr[1]);
                    i = skip + nchars - 1;
                }
                else
                    count++;
            }
            return count;
        }

        static void Main(string[] args)
        {
            var lines = File.ReadAllText("..\\..\\input.txt");
            var chars = lines.ToCharArray();
            Console.WriteLine("Bad format length:  " + explode(chars));
            Console.WriteLine("Good format length: " + explodeRecursive(chars, 1));
            Console.Read();
        }
    }
}
