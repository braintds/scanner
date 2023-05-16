using System;
using System.Collections.Generic;
using System.Data.SqlTypes;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace scanner
{
    static class lexicalScanner
    {
        private enum Codes
        {
            ERRORcode = -1,
            identifierCode = 1,
            integerConstCode,
            doubleConstCode,
            equalCode,          //.EQ.
            notEqualCode,       //.NE.
            greaterThanCode,    //.GT.
            greaterEqualCode,   //.GE.
            lessThanCode,       //.LT.
            lessEqualCode,      //.LE.
            logicalNotCode,     //.NOT.
            logicalAndCode,     //.AND.
            logicalOrCode,      //.OR.
            LeftParenthesis,    // (
            RightParenthesis,   // )
        }

        private static Codes isOperator(string text)
        {
            if (text == "") return Codes.ERRORcode;

            switch (text.ToUpperInvariant())
            {
                case ".EQ.": return Codes.equalCode;
                case ".NE.": return Codes.notEqualCode;
                case ".GT.": return Codes.greaterThanCode;
                case ".GE.": return Codes.greaterEqualCode;
                case ".LT.": return Codes.lessThanCode;
                case ".LE.": return Codes.lessEqualCode;
                case ".NOT.": return Codes.logicalNotCode;
                case ".AND.": return Codes.logicalAndCode;
                case ".OR.": return Codes.logicalOrCode;
                default: return Codes.ERRORcode;
            }
        }

        private static Codes isNumber(string text)
        {
            if (text == "") return Codes.ERRORcode;

            int integerConst = 0;
            if (int.TryParse(text, out integerConst))
                return Codes.integerConstCode;

            if (Char.IsDigit(text[0]))
            {
                int i = 0;
                int countDot = 0;

                while (i < text.Length)
                {
                    if (text[i] == '.')
                    {
                        countDot++;
                    }
                    i++;
                }

                if (countDot == 1)
                    return Codes.doubleConstCode;
            }
            return 0;
        }

        private static Codes isIdentifier(string text)
        {
            if (text == "") return Codes.ERRORcode;

            if (!Char.IsLetter(text[0]))
                return Codes.ERRORcode;
            else
                foreach (char c in text)
                    if (c != '_' && !Char.IsDigit(c) && !Char.IsLetter(c))
                        return Codes.ERRORcode;

            return Codes.identifierCode;
        }

        private static Codes isBracket(string text)
        {
            if (text == "") return Codes.ERRORcode;
            switch (text)
            {
                case "(": return Codes.LeftParenthesis;
                case ")": return Codes.RightParenthesis;
                default: return Codes.ERRORcode;
            }
        }

        private static char getNext(string text, int currentPosition)
        {
            return text[currentPosition + 1];
        }

        public static string scanner(string inputString)
        {
            int i = 0;
            string answer = "";
            var parts = new Dictionary<string, string>();
            string subString = "";
            while (i < inputString.Length)
            {
                char c = inputString[i];
                if (c == ')' || c == '(') // это скобки
                {
                    i++;
                    parts.Add(i.ToString() + " " + c, "Code " + Convert.ToInt32(isBracket(c.ToString())));
                    continue;
                }

                if (c == '.') // может быть оператором
                {
                    subString = "";
                    int start = i + 1;
                    int countDot = 0;
                    while (i < inputString.Length && (char.IsLetter(inputString[i]) || inputString[i] == '.'))
                    {
                        if (countDot < 2)
                        {
                            subString += inputString[i];
                            if (inputString[i] == '.')
                                countDot++;
                        }
                        else
                        {
                            break;
                        }
                        i++;
                    }

                    if (subString.EndsWith("."))
                    {
                        parts.Add(start.ToString() + ":" + (i).ToString() + " " + subString, "Code " + Convert.ToInt32(isOperator(subString)));
                    }
                }

                if (Char.IsLetter(c)) //может быть идентификатором
                {
                    subString = "";
                    int start = i + 1;

                    while (i < inputString.Length && (Char.IsLetter(inputString[i]) || Char.IsDigit(inputString[i])))
                    {
                        subString += inputString[i];
                        i++;
                    }
                    parts.Add(start.ToString() + ":" + (i).ToString() + " " + subString, "Code " + Convert.ToInt32(isIdentifier(subString)));
                    
                }

                if (Char.IsDigit(c)) //может быть числом
                {
                    int start = i + 1;

                    while (i < inputString.Length && (Char.IsDigit(inputString[i]) || inputString[i] == '.'))
                    {
                        subString += inputString[i];
                        i++;
                    }

                    if (Char.IsLetter(getNext(inputString, i)))
                    {
                        subString = subString.Remove(subString.LastIndexOf('.'), 1);
                        i--;
                    }
                       

                    if (subString.EndsWith("."))
                    {
                        parts.Add(start.ToString() + ":" + (i).ToString() + " " + subString, "Code " + Convert.ToInt32(Codes.ERRORcode));
                        
                    }
                    else
                    {
                        i--;
                        parts.Add(start.ToString() + ":" + (i).ToString() + " " + subString, "Code " + Convert.ToInt32(isNumber(subString)));
                    }
                    
                }
            }

            foreach (KeyValuePair<string, string> pair in parts)
            {
                answer += pair.Key.PadRight(5) + " " + pair.Value + '\n';
            }
            return answer;
        }
    }
}
