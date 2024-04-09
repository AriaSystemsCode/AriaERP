using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.QueryBuilder.TableFieldValue
{
    public interface IAriaQueryTableFieldValue
    {
        string getValue(DatabaseType dataBaseType,Operator fieldOperator);

        string getValue(DatabaseType dataBaseType);
    }
}
