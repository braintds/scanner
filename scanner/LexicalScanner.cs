using System;
using System.Collections.Generic;
using System.Data.SqlTypes;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace scanner
{
    public static class LexicalScanner
    {
        private enum Codes
        {
            ErrorCode = -1,
            IdentifierCode = 1,         // {a-z, A_Z, _}*
            IntegerConstCode,           // {0-9}*
            DoubleConstCode,            // {0-9}*{.} & {0-9}*
            EqualCode,                  // .EQ.
            NotEqualCode,               // .NE.
            GreaterThanCode,            // .GT.
            GreaterEqualCode,           // .GE.
            LessThanCode,               // .LT.
            LessEqualCode,              // .LE.
            LogicalNotCode,             // .NOT.
            LogicalAndCode,             // .AND.
            LogicalOrCode,              // .OR.
            LeftParenthesisCode,        // (
            RightParenthesisCode,       // )
            ArithmPlusCode,             // +
            ArithmMinusCode,            // - 
            ArithmMultiplicationCode,   // *
            ArithmDivisionCode,         // "/"
        }

        private static Codes IsArithmOperator(string text)
        {
            if (text.Length > 1)
            {
                return Codes.ErrorCode;
            }

            switch (text)
            {
                case "+": return Codes.ArithmPlusCode;
                case "-": return Codes.ArithmMinusCode;
                case "*": return Codes.ArithmMultiplicationCode;
                case "/": return Codes.ArithmDivisionCode;
                default: return Codes.ErrorCode;
            }
        }

        private static Codes IsOperator(string text)
        {
            if (text == "")
            {
                return Codes.ErrorCode;
            }

            switch (text.ToUpperInvariant())
            {
                case ".EQ.": return Codes.EqualCode;
                case ".NE.": return Codes.NotEqualCode;
                case ".GT.": return Codes.GreaterThanCode;
                case ".GE.": return Codes.GreaterEqualCode;
                case ".LT.": return Codes.LessThanCode;
                case ".LE.": return Codes.LessEqualCode;
                case ".NOT.": return Codes.LogicalNotCode;
                case ".AND.": return Codes.LogicalAndCode;
                case ".OR.": return Codes.LogicalOrCode;
                default: return Codes.ErrorCode;
            }
        }

        private static Codes IsNumber(string text)
        {
            if (text == "")
            {
                return Codes.ErrorCode;
            }

            int integerConst = 0;

            if (int.TryParse(text, out integerConst))
            {
                return Codes.IntegerConstCode;
            }
            // Проверяет первое число на то что это цифра. \ Checks if the first number is a digit
            if (Char.IsDigit(text[0]))
            {
                int i = 0;
                int countDot = 0;

                while (i < text.Length)
                {
                    if (text[i] == '.')
                    {
                        countDot++;         // Точка должна быть только одна \ There must be only one point
                    }
                    i++;
                }

                if (countDot == 1)
                {
                    return Codes.DoubleConstCode;
                }
            }
            // Если TryParse вернул false или в строке больше одной точки.
            // \ If TryParse returned false or there is more than one dot in the string.
            return Codes.ErrorCode;
        }

        private static Codes IsIdentifier(string text)
        {
            if (text == "")
            {
                return Codes.ErrorCode;
            }

            if (!Char.IsLetter(text[0]))
            {
                return Codes.ErrorCode;
            }
            else
            {
                foreach (char c in text)
                {
                    if (c != '_' && !Char.IsDigit(c) && !Char.IsLetter(c))
                    {
                        return Codes.ErrorCode;
                    }
                }
            }
            return Codes.IdentifierCode;
        }

        private static Codes IsBracket(string text)
        {
            if (text == "")
            {
                return Codes.ErrorCode;
            }

            switch (text)
            {
                case "(": return Codes.LeftParenthesisCode;
                case ")": return Codes.RightParenthesisCode;
                default: return Codes.ErrorCode;
            }
        }

        private static char GetNext(string text, int currentPosition)
        {
            return text[currentPosition + 1];
        }

        public static string Scanner(string inputString)
        {
            int i = 0;
            string answer = "";
            var parts = new Dictionary<string, string>();
            string subString = "";

            while (i < inputString.Length)
            {
                char c = inputString[i];

                // Это скобки. \ It's brackets.
                if (c == ')' || c == '(')
                {
                    i++;
                    parts.Add(i.ToString() + " " + c, "Code " + Convert.ToInt32(IsBracket(c.ToString())));
                    continue;
                }

                // Может быть оператором. \ Can be an operator.
                if (c == '.')
                {
                    subString = "";
                    int start = i + 1;
                    int countDot = 0;

                    while ((i < inputString.Length) && (char.IsLetter(inputString[i]) || inputString[i] == '.'))
                    {
                        if (countDot < 2)
                        {
                            subString += inputString[i];

                            if (inputString[i] == '.')
                            {
                                countDot++;
                            }
                        }
                        else
                        {
                            break;
                        }
                        i++;
                    }

                    if (subString.EndsWith("."))
                    {
                        parts.Add(start.ToString() + ":" + (i).ToString() + " " + subString, "Code " + Convert.ToInt32(IsOperator(subString)));
                    }

                    subString = "";
                }

                // Может быть идентификатором. \ Can be an identifier.
                if (Char.IsLetter(c))
                {
                    subString = "";
                    int start = i + 1;

                    while ((i < inputString.Length) && (Char.IsLetter(inputString[i]) || Char.IsDigit(inputString[i])))
                    {
                        subString += inputString[i];
                        i++;
                    }
                    parts.Add(start.ToString() + ":" + (i).ToString() + " " + subString, "Code " + Convert.ToInt32(IsIdentifier(subString)));

                }

                // Может быть числом. \ Can be a number.
                if (Char.IsDigit(c))
                {
                    subString = "";
                    int start = i + 1;

                    while ((i < inputString.Length) && (Char.IsDigit(inputString[i]) || inputString[i] == '.'))
                    {
                        subString += inputString[i];
                        i++;
                    }

                    if ((i < inputString.Length) && Char.IsLetter(GetNext(inputString, i)))
                    {
                        subString = subString.Remove(subString.LastIndexOf('.'), 1);
                        i--;
                    }

                    if (subString.EndsWith("."))
                    {
                        parts.Add(start.ToString() + ":" + (i).ToString() + " " + subString, "Code " + Convert.ToInt32(Codes.ErrorCode));
                    }
                    else
                    {
                        parts.Add(start.ToString() + ":" + (i).ToString() + " " + subString, "Code " + Convert.ToInt32(IsNumber(subString)));
                    }

                    subString = "";
                }

                // Это арифметический оператор. \ It is an arithmetic operator.
                if (c == '+' || c == '-' || c == '*' || c == '/')
                {
                    i++;
                    parts.Add(i.ToString() + " " + c, "Code " + Convert.ToInt32(IsArithmOperator(c.ToString())));
                    continue;
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
