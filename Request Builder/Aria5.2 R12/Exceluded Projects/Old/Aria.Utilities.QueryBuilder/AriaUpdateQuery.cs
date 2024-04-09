using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;

namespace Aria.Utilities.QueryBuilder
{
    public class AriaUpdateQuery : AriaQuery
    {
        private ArrayList _queryTableFiled = new ArrayList();
        public ArrayList QueryTableFiled
        {
            get { return _queryTableFiled; }
            set { _queryTableFiled = value; }
        }
    }
}
