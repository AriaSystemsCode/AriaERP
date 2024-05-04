using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Xpo;
using ArrayToMemo;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    [NonPersistent]
    public class Filter
    {
        //1
        public string Name { get; set; }

        //2
        public FilterVariableType Type { get; set; }

        //3
        public SystemDataType DataType { get; set; }

        //4
        public bool Not { get; set; }

        //5
        public FilterOperator Operator { get; set; }

        //6
        public string Value { get; set; }

        //7
        public FilterValueType ValueType { get; set; }


        public FilterType FilterType { get; set; }


        public static List<Filter> Convertor(string value)
        {
            string arrayName;
            List<Filter> filterList = new List<Filter>();
            if (value != null && value.Length > 38)
            {
                string x = HexString2Ascii((string)value);// System.Text.ASCIIEncoding.Convert(Encoding. .ASCIIEncoding.ASCII..GetString(value);
                Array array = new ArrayToMemo.Main().MemoToArray(x, out arrayName);
                if (array.GetLength(0) > 0 && array.Rank == 2 && array.GetLength(1) == 7)
                {
                    for (int i = 0; i < array.GetLength(0); i++)
                    {
                        if (array.GetValue(i, 0) is Boolean || string.IsNullOrWhiteSpace((string)array.GetValue(i, 0))) continue;

                        Filter filter = new Filter();

                        filter.Name = ((string)array.GetValue(i, 0)).Trim();

                        filter.Type = (string)array.GetValue(i, 1) == "F" ? FilterVariableType.F : FilterVariableType.E;

                        if (string.IsNullOrWhiteSpace(((string)array.GetValue(i, 2))))
                            filter.DataType = SystemDataType.C;
                        else
                            filter.DataType = (SystemDataType)Enum.Parse(typeof(SystemDataType), (string)array.GetValue(i, 2));

                        filter.Not = (bool)array.GetValue(i, 3) ? false : true;

                        filter.Operator = (FilterOperator)Enum.Parse(typeof(FilterOperator), ((string)array.GetValue(i, 4)).Trim().Replace(" ", ""), true);

                        filter.Value = array.GetValue(i, 5) is string ? ((string)array.GetValue(i, 5)).Trim() : null;

                        filter.ValueType = !string.IsNullOrWhiteSpace((string)array.GetValue(i, 6)) && ((string)array.GetValue(i, 6)).Trim() == "E" ? FilterValueType.E : FilterValueType.V;

                        filter.FilterType = arrayName == "LAOGVRFLT" ? FilterType.VariableFilter : FilterType.FixedFilter;
                        filterList.Add(filter);
                    }
                }
            }
            return filterList;
        }

        public static string Convertor(List<Filter> filterList)
        {
            string result = null;
            if (filterList != null && filterList.Count > 0)
            {
                object[,] array = new object[filterList.Count, 7];
                int index = 0;
                foreach (Filter filter in filterList)
                {
                    array.SetValue(filter.Name, index, 0);
                    array.SetValue(filter.Type == FilterVariableType.F ? "F" : "E", index, 1);
                    array.SetValue(filter.DataType.ToString(), index, 2);
                    array.SetValue(filter.Not ? false : true, index, 3);
                    array.SetValue(filter.Operator.ToString(), index, 4);
                    array.SetValue(filter.Value, index, 5);
                    //ATA add tostring function to value type as all enums handling before 
                    array.SetValue(filter.ValueType.ToString(), index, 6);
                    index++;
                }
                string arrayName = string.Empty;
                switch (filterList[0].FilterType)
                {
                    case FilterType.VariableFilter:
                        arrayName = "LAOGVRFLT";
                        break;
                    case FilterType.FixedFilter:
                        arrayName = "LAOGFXFLT";
                        break;
                    case FilterType.HeddienFilter:
                        arrayName = "LAOGHDFLT";
                        break;
                }
               // arrayName = filterList[0].FilterType == FilterType.VariableFilter ? "LAOGVRFLT" : "LAOGFXFLT";
                // arrayName = "LAOGHDFLT";
                result = new ArrayToMemo.Main().ArrayToMemo(array, arrayName);
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

    public enum FilterVariableType
    {
        [DisplayName("Field")]
        F,
        [DisplayName("Expression")]
        E
    }

    public enum FilterValueType
    {
        [DisplayName("Value")]
        V,
        [DisplayName("Expression")]
        E
    }

    public enum FilterOperator
    {
        Is,
        [DisplayName("In list")]
        InList,
        Like,
        Between,
        [DisplayName("Less Or Equal")]
        LessOrEqual,
        [DisplayName("Greater Or Equal")]
        GreaterOrEqual
    }

    public enum FilterType
    {
        [Persistent(" ")]
        [DisplayName("None")]
        None,
        [DisplayName("Fixed Filter")]
        [Persistent("F")]
        FixedFilter,
        [DisplayName("Variable Filter")]
        [Persistent("V")]
        VariableFilter,
        [DisplayName("Heddien Filter")]
        [Persistent("H")]
        HeddienFilter
    }

}
