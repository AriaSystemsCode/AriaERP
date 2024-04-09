using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.QueryBuilder.TableFieldValue
{
    public class AriaQueryTableJoinField : AriaQueryTableField, IAriaQueryTableFieldValue
    {
        public AriaQueryTableJoinField(AriaQueryTable fieldTable, string fieldName, IAriaQueryTableFieldValue fieldValue):base(fieldTable, fieldName, fieldValue)
        {
        }

        public override string getFullField()
        {
            return this.getValue(this.FieldTable.DatabaseType);
        }

        public override string getFullField(Operator fieldOperator)
        {
            return this.getValue(this.FieldTable.DatabaseType,fieldOperator);
        }

        #region IAriaQueryTableFieldValue Members

        public string getValue(DatabaseType dataBaseType, Operator fieldOperator)
        {
            string join = "";

            switch (fieldOperator)
            {
                case Operator.EQUAL:
                    join += " = ";
                    break;

                case Operator.GREATER_THAN:
                    join += " > ";
                    break;

                case Operator.GREATER_THAN_OR_EQUAL:
                    join += " >= ";
                    break;

                case Operator.LESS_THAN:
                    join += " < ";
                    break;

                case Operator.LESS_THAN_OR_EQUAL:
                    join += " <= ";
                    break;
            }

            return join + this.getFieldName();
        }

        public string getValue(DatabaseType dataBaseType)
        {
            return getValue(dataBaseType, Operator.EQUAL);
        }

        #endregion
    }
}
