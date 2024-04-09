using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
using System.Text.RegularExpressions;

namespace Aria.Utilities.ObjectDictionaryBuilder.Aria27ObjectDictionaryConverter
{
    public class VFPSchemaAdapter
    {
        public IndexColumn[] GetIndexColumns(string expression, EnumSort sort)
        {
            ArrayList indexColmns = new ArrayList();

            string[] foxFields = expression.Split('+');

            string fieldParameterPattern = @"(,[0-9]*)?\)";
            Regex parameterRegex = new Regex(fieldParameterPattern);

            string fieldFunctionPattern = @"(DTOS|STR)\(";
            Regex functionRegex = new Regex(fieldFunctionPattern);

            for (int index = 0; index < foxFields.Length; index++)
            {
                string field = foxFields[index];

                field = field.Trim().ToUpper();
                field = functionRegex.Replace(field, "");
                field = parameterRegex.Replace(field, "");

                indexColmns.Add(new IndexColumn(field, sort));
            }

            return (IndexColumn[])indexColmns.ToArray(typeof(IndexColumn));
        }
    }
}
