using System;
using System.Collections.Generic;
using System.Text;
using Aria.Utilities.QueryBuilder.TableFieldValue;

namespace Aria.Utilities.QueryBuilder
{
    public class AriaQueryTableField
    {
        public AriaQueryTableField(AriaQueryTable fieldTable, string fieldName, IAriaQueryTableFieldValue fieldValue)
        {
            FieldTable = fieldTable;
            FieldName = fieldName;
            FieldValue = fieldValue;
        }

        private string _fieldName;
        public string FieldName
        {
            get { return _fieldName; }
            set { _fieldName = value; }
        }

        private IAriaQueryTableFieldValue _fieldValue;
        public IAriaQueryTableFieldValue FieldValue
        {
            get { return _fieldValue; }
            set { _fieldValue = value; }
        }

        private AriaQueryTable _fieldTable;
        public AriaQueryTable FieldTable
        {
            get { return _fieldTable; }
            set { _fieldTable = value; }
        }

        public string getFieldName()
        {
            string fieldName = this.FieldTable.TableName + "." + this.FieldName;

            return fieldName;
        }

        public virtual string getFullField()
        {
            string fullField = this.getFieldName() + this.FieldValue.getValue(FieldTable.DatabaseType);
            
            return fullField;
        }

        public virtual string getFullField(Operator fieldOperator)
        {
            string fullField = this.getFieldName() + this.FieldValue.getValue(FieldTable.DatabaseType, fieldOperator);

            return fullField;
        }
    }
}
