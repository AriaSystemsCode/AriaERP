using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;

namespace Aria.Utilities.QueryBuilder
{
    public class AriaInsertQuery : AriaQuery
    {
        private ArrayList _queryTableFields = new ArrayList();
        public ArrayList QueryTableFields
        {
            get { return _queryTableFields; }
            set { _queryTableFields = value; }
        }
    }
}
