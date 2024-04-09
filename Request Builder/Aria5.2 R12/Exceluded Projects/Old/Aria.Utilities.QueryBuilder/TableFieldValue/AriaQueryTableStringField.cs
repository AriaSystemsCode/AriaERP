using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.QueryBuilder.TableFieldValue
{
    public class AriaQueryTableStringField : IAriaQueryTableFieldValue
    {
        private string _stringValue = "";
        public string StringValue
        {
            get { return _stringValue; }
            set { _stringValue = value; }
        }

        #region IAriaQueryTableFieldValue Members

        public string getValue(DatabaseType dataBaseType, Operator fieldOperator)
        {
            string stringField = "";

            switch (fieldOperator)
            {
                case Operator.EQUAL:
                    stringField += " = ";
                    break;

                case Operator.GREATER_THAN:
                    stringField += " > ";
                    break;

                case Operator.GREATER_THAN_OR_EQUAL:
                    stringField += " >= ";
                    break;

                case Operator.LESS_THAN:
                    stringField += " < ";
                    break;

                case Operator.LESS_THAN_OR_EQUAL:
                    stringField += " <= ";
                    break;
            }

            switch (dataBaseType)
            {
                case DatabaseType.FOX:
                    stringField += "'" + this.StringValue + "'";
                    break;

                case DatabaseType.SQL:
                    stringField += this.StringValue;
                    break;
            }

            return stringField;
        }

        public string getValue(DatabaseType dataBaseType)
        {
            return getValue(dataBaseType, Operator.EQUAL);
        }

        #endregion
    }
}
