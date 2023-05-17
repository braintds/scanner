using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace scanner
{
    public static class Extensions
    {
        public static string Filter(this string str, List<char> charsToRemove)
        {
            String chars = "[" + String.Concat(charsToRemove) + "]";
            return Regex.Replace(str, chars, String.Empty);
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            List<Char> listToRemove = new List<Char>() { ' ', '\n', '\t' };

            var expression = "(5.5 + 7 .AND. y + a * Z) .OR. .NOT. z / a - 4";
            Console.WriteLine(expression.Filter(listToRemove));
            Console.WriteLine(LexicalScanner.Scanner(expression.Filter(listToRemove)));

            Console.ReadKey();
        }

    }
}
