using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.QueryBuilder.TableFieldValue
{
    public class AriaQueryTableNumericField : IAriaQueryTableFieldValue
    {
        private double _numeric = 0.0;
        public double Numeric
        {
            get { return _numeric; }
            set { _numeric = value; }
        }

        #region IAriaQueryTableFieldValue Members

        public string getValue(DatabaseType dataBaseType, Operator fieldOperator)
        {
            string numeric = "";

            switch (fieldOperator)
            {
                case Operator.EQUAL:
                    numeric += " = ";
                    break;

                case Operator.GREATER_THAN:
                    numeric += " > ";
                    break;

                case Operator.GREATER_THAN_OR_EQUAL:
                    numeric += " >= ";
                    break;

                case Operator.LESS_THAN:
                    numeric += " < ";
                    break;

                case Operator.LESS_THAN_OR_EQUAL:
                    numeric += " <= ";
                    break;
            }

            switch (dataBaseType)
            {
                case DatabaseType.FOX:
                    numeric += this.Numeric.ToString();
                    break;

                case DatabaseType.SQL:
                    numeric += this.Numeric.ToString();
                    break;
            }

            return numeric;
        }

        public string getValue(DatabaseType dataBaseType)
        {
            return this.getValue(dataBaseType, Operator.EQUAL);
        }

        #endregion
    }
}
