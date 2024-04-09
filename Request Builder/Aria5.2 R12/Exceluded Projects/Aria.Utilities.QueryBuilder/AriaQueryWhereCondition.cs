using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.QueryBuilder
{
    public class AriaQueryWhereCondition
    {
        public AriaQueryWhereCondition(AriaQueryTableField queryTableField, Operator queryOperator)
        {
            QueryTableField = queryTableField;
            QueryOperator = queryOperator;
        }

        private AriaQueryTableField _queryTableField;
        public AriaQueryTableField QueryTableField
        {
            get { return _queryTableField; }
            set { _queryTableField = value; }
        }

        private Operator _queryOperator;
        public Operator QueryOperator
        {
            get { return _queryOperator; }
            set { _queryOperator = value; }
        }
    }
}
