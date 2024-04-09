using System;
using System.Collections.Generic;
using System.Text;
using Aria.Utilities.QueryBuilder;

namespace Aria.Utilities.QueryBuilder.TableFieldValue
{
    public class AriaQueryTableSubQueryField : AriaSelectQuery, IAriaQueryTableFieldValue
    {
        #region IAriaQueryTableFieldValue Members

        public string getValue(DatabaseType dataBaseType, Operator fieldOperator)
        {
            return getValue(dataBaseType);
        }

        public string getValue(DatabaseType dataBaseType)
        {
            AriaQueryBuilder queryBuilder = new AriaQueryBuilder();
            queryBuilder.Query = this;

            return " IN (" + queryBuilder.GetQuery() + ")";
        }

        #endregion
    }
}
