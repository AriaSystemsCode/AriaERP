using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
using System.Data;
using System.Data.SqlClient;

namespace Aria.Utilities.QueryBuilder
{
    public class AriaQueryBuilder
    {
        private AriaQuery _query;
        public AriaQuery Query
        {
            get { return _query; }
            set { _query = value; }
        }

        public string GetQuery()
        {
            AriaQueryOptimizer optimizer = new AriaQueryOptimizer();

            return optimizer.GetQuery(this.Query);
        }
    }
}
