using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;

namespace Aria.Utilities.QueryBuilder
{
    public abstract class AriaQuery
    {
        private ArrayList _queryTables = new ArrayList();
        public ArrayList QueryTables
        {
            get { return _queryTables; }
            set { _queryTables = value; }
        }

        private AriaQueryWhereClause _queryWhereClause = new AriaQueryWhereClause();
        public AriaQueryWhereClause QueryWhereClause
        {
            get { return _queryWhereClause; }
            set { _queryWhereClause = value; }
        }
    }
}
