using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
using System.Data.SqlClient;
using System.Data;

namespace Aria.Utilities.QueryBuilder
{
    public class AriaSelectQuery : AriaQuery
    {
        private ArrayList _queryTableFields = new ArrayList();
        public ArrayList QueryTableFields
        {
            get { return _queryTableFields; }
            set { _queryTableFields = value; }
        }

        private ArrayList _queryTableNames = new ArrayList();
        public ArrayList QueryTableNames
        {
            get { return _queryTableNames; }
            set { _queryTableNames = value; }
        }

        private bool _isDistinct;
        public bool IsDistinct
        {
            get { return _isDistinct; }
            set { _isDistinct = value; }
        }

        private AriaQueryTableField _groupBy;
        public AriaQueryTableField GroupBy
        {
            get { return _groupBy; }
            set { _groupBy = value; }
        }

        private ArrayList _orderBy = new ArrayList();
        public ArrayList OrderBy
        {
            get { return _orderBy; }
            set { _orderBy = value; }
        }

        private bool _isJoin;
        public bool IsJoin
        {
            get { return _isJoin; }
            set { _isJoin = value; }
        }
    }
}
