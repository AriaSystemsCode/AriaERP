using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.QueryBuilder.TableFieldValue
{
    public class AriaQueryTableDateField : IAriaQueryTableFieldValue
    {
        private DateTime _date = new DateTime();
        public DateTime Date
        {
            get { return _date; }
            set { _date = value; }
        }

        #region IAriaQueryTableFieldValue Members

        public string getValue(DatabaseType dataBaseType)
        {
            return getValue(dataBaseType, Operator.EQUAL);

        }

        public string getValue(DatabaseType dataBaseType, Operator fieldOperator)
        {
            string date = "";

            switch (fieldOperator)
            {
                case Operator.EQUAL:
                    date += " = ";
                    break;

                case Operator.GREATER_THAN:
                    date += " > ";
                    break;

                case Operator.GREATER_THAN_OR_EQUAL:
                    date += " >= ";
                    break;

                case Operator.LESS_THAN:
                    date += " < ";
                    break;

                case Operator.LESS_THAN_OR_EQUAL:
                    date += " <= ";
                    break;
            }

            switch (dataBaseType)
            {
                case DatabaseType.FOX:
                    date += "DTOS(" + Date.ToString() + ")";
                    break;

                case DatabaseType.SQL:
                    date += Date.ToString();
                    break;

                default:
                    break;
            }

            return date;
        }

        #endregion
    }
}
