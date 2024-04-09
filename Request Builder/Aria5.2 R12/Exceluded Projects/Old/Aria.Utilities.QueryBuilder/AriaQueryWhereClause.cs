using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;

namespace Aria.Utilities.QueryBuilder
{
    public class AriaQueryWhereClause
    {
        private ArrayList _queryWhereConditions = new ArrayList();
        public ArrayList QueryWhereConditions
        {
            get { return _queryWhereConditions; }
            set { _queryWhereConditions = value; }
        }

        private ArrayList _operations = new ArrayList();
        public ArrayList Operations
        {
            get { return _operations; }
            set { _operations = value; }
        }
    }
}
