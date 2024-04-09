using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
using System.Text.RegularExpressions;

namespace Aria.Utilities.Aria40Converter.SystemFilesAdaptor
{
    public class VFPSchemaHelper
    {
        public static IndexField[] GetIndexFields(string expression, SortTypes sort)
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

                if (field.Contains("("))
                {
                    if (field.Contains(","))
                    {
                        field = foxFields[index].Substring(foxFields[index].LastIndexOf('(') + 1, (foxFields[index].Remove(foxFields[index].LastIndexOf(',')).LastIndexOf(',') - foxFields[index].LastIndexOf('(') - 1));
                        if (field.Contains(","))
                        {
                            field = field.Substring(0, (field.Remove(field.LastIndexOf(',')).LastIndexOf(',')));
                        }          
                    }
                    else
                        field = foxFields[index].Substring(foxFields[index].LastIndexOf('(') + 1, (foxFields[index].IndexOf(')') - foxFields[index].LastIndexOf('(') - 1));
                }

                IndexField indexField = new IndexField(field, sort);

                indexColmns.Add(indexField);
            }

            IndexField[] result = (IndexField[])indexColmns.ToArray(typeof(IndexField));

            for (int i = 0; i < result.Length; i++)
            {
                result[i].FieldName = result[i].FieldName.Split(',')[0];
            }

            return result;
        }
    }
}
