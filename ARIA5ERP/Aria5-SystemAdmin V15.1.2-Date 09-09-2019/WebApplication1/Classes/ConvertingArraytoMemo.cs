using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Web;

namespace WebApplication1.Classes
{
    public class ConvertingArraytoMemo
    {

        public string ArrayToMemo(Array array, string ArrayName)
        {
            StringBuilder builder = new StringBuilder();
            int num = (array.Rank > 0) ? array.GetLength(0) : 0;
            int num2 = (array.Rank > 1) ? array.GetLength(1) : 0;
            ArrayName = ArrayName.PadRight(11, '\0');
            builder.Append("M" + '\x000f');

            builder.Append(string.Concat(new object[] { ArrayName, "A", new string('\0', 13), '\x0003', new string('\0', 6), (char)num, '\0', (char)num2, '\0' }));
            string format = string.Concat(new object[] { ArrayName, "{0}", new string('\0', 4), "{1}", new string('\0', 15), "{2}", '\0' });
            for (int i = 0; i < num; i++)
            {
                object obj2;
                if (num2 > 0)
                {
                    for (int j = 0; j < num2; j++)
                    {
                        obj2 = array.GetValue(i, j);
                        builder.Append(string.Format(format, this.GetFoxType(obj2).ToString(), (char)(this.GetLength(obj2) + 1), this.GetValue(obj2)));
                        if (obj2 is bool)
                        {
                            builder.Remove(builder.Length - 1, 1);
                        }
                    }
                }
                else
                {
                    obj2 = array.GetValue(i);
                    builder.Append(string.Format(format, this.GetFoxType(obj2).ToString(), (char)(this.GetLength(obj2) + 1), this.GetValue(obj2)));
                }
            }
            builder.Append('\x001a');
            return builder.ToString();
        }

        public char GetFoxType(object obj)
        {
            if (obj != null)
            {
                string fullName = obj.GetType().FullName;
                if (fullName == typeof(string).FullName)
                {
                    return 'C';
                }
                if (fullName == typeof(bool).FullName)
                {
                    return 'L';
                }
            }
            return 'L';
        }

        public int GetLength(object obj)
        {
            int num = 0;
            if (obj != null)
            {
                string fullName = obj.GetType().FullName;
                if (fullName == typeof(string).FullName)
                {
                    return Convert.ToString(obj).Length;
                }
                if (fullName == typeof(bool).FullName)
                {
                    num = 1;
                }
            }
            return num;
        }

        public string GetValue(object obj)
        {
            string str = "";
            if (obj != null)
            {
                string fullName = obj.GetType().FullName;
                if (fullName == typeof(bool).FullName)
                {
                    return (((bool)obj) ? new string('\x0001', 1) : new string('\0', 1));
                }
                if (fullName == typeof(string).FullName)
                {
                    str = Convert.ToString(obj);
                }
            }
            return str;
        }
        public Array MemoToArray(string Memo, out string ArrayName)
        {
            ArrayName = "";
            int num = 2;
            while (num < 13)
            {
                if (Memo[num] == '\0')
                {
                    break;
                }
                ArrayName = ArrayName + Memo[num];
                num++;
            }
            // position 34
            int num2 = Memo[0x22];
            // position 36
            int num3 = Memo[0x24];
            Array array = null;
            if (num3 == 0)
            {
                array = new object[num2];
            }
            else
            {
                array = new object[num2, num3];
            }
            bool flag = true;
            char ch = '\0';
            object obj2 = null;
            int num4 = 0x26;
            int index = 0;
            int num6 = 0;
            while (flag)
            {
                ch = Memo[num4 + 11];
                obj2 = null;
                if (ch == 'L')
                {
                    obj2 = Memo[num4 + 0x20] != '\0';
                    num4 += 0x20;
                }
                else
                {
                    obj2 = "";
                    for (num = num4 + 0x20; num < Memo.Length; num++)
                    {
                        if (Memo[num] != '\0')
                        {
                            obj2 = obj2.ToString() + Memo[num];
                        }
                        else
                        {
                            num4 = num;
                            break;
                        }
                    }
                }
                if (num3 == 0)
                {
                    array.SetValue(obj2, index);
                    index++;
                }
                else
                {
                    array.SetValue(obj2, index, num6);
                    if ((num6 + 1) == num3)
                    {
                        num6 = 0;
                        index++;
                    }
                    else
                    {
                        num6++;
                    }
                }
                if ((Memo[num4 + 1] == '\x001a') || ((index == (num2 - 1)) && (num6 == (num3 - 1))))
                {
                    flag = false;
                }
                else
                {
                    num4++;
                }
            }
            return array;
        }
    }
}