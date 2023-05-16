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

            var expression = "(5. .AND. y) .OR. .NOT. z";
            Console.WriteLine(expression.Filter(listToRemove));
            Console.WriteLine(lexicalScanner.scanner(expression.Filter(listToRemove)));

            Console.ReadKey();
        }

    }
}
