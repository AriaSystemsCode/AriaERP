//using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using WebApplication1.Classes;

namespace WebApplication1
{
    //ATA add new class to work with files array in memo fields 
  //  [NonPersistent]
   public class Files
    {

        //1
        public string Name { get; set; }

        //2
        public string Alias { get; set; }

        //3
        public string Index { get; set; }



        public static List<Files> Convertor(string value)
        {
            string arrayName;
            List<Files> FilesList = new List<Files>();
            if (value != null && value.Length > 38)
            {
                string x = HexString2Ascii((string)value);// System.Text.ASCIIEncoding.Convert(Encoding. .ASCIIEncoding.ASCII..GetString(value);
                Array array = new ConvertingArraytoMemo().MemoToArray(x, out arrayName);
                if (array.GetLength(0) > 0 && array.Rank == 2 && array.GetLength(1) == 7)
                {
                    for (int i = 0; i < array.GetLength(0); i++)
                    {
                        if (array.GetValue(i, 0) is Boolean || string.IsNullOrWhiteSpace((string)array.GetValue(i, 0))) continue;

                        Files File = new Files();

                        File.Name = ((string)array.GetValue(i, 0)).Trim();

                        File.Alias = (string)array.GetValue(i, 1).ToString();
                        File.Index = (string)array.GetValue(i, 2).ToString();

                        
                       
                        FilesList.Add(File);
                    }
                }
            }
            return FilesList;
        }

        public static string Convertor(List<Files> FilesList)
        {
            string result = null;
            if (FilesList != null && FilesList.Count > 0)
            {
                object[,] array = new object[FilesList.Count, 7];
                int index = 0;
                foreach (Files File in FilesList)
                {
                    array.SetValue(File.Name, index, 0);
                    array.SetValue(File.Alias, index, 1);
                    array.SetValue(File.Index, index, 2);
                    index++;
                }
                string arrayName = "LASELFILE"; //FilesList[0].FilterType == FilterType.VariableFilter ? "LAOGVRFLT" : "LAOGFXFLT";
                result = new ConvertingArraytoMemo().ArrayToMemo(array, arrayName);
                result = convertAsciiTextToHex(result);
            }
            return result;
        }

        private static string HexString2Ascii(string hexString)
        {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i <= hexString.Length - 2; i += 2)
            {
                sb.Append(Convert.ToString(Convert.ToChar(Int32.Parse(hexString.Substring(i, 2), System.Globalization.NumberStyles.HexNumber))));
            }
            return sb.ToString();
        }

        private static string convertAsciiTextToHex(String i_asciiText)
        {
            StringBuilder sBuffer = new StringBuilder();
            for (int i = 0; i < i_asciiText.Length; i++)
            {
                sBuffer.Append(Convert.ToInt32(i_asciiText[i]).ToString("x2"));
            }
            return sBuffer.ToString().ToUpper();
        }
    }
}
