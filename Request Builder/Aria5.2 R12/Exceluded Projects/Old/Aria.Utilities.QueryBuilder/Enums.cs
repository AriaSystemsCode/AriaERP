using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.QueryBuilder
{
    public enum DatabaseType
    {
        FOX,
        SQL
    }

    public enum Operator
    {
        EQUAL, 
        LESS_THAN, 
        GREATER_THAN, 
        LESS_THAN_OR_EQUAL, 
        GREATER_THAN_OR_EQUAL
    }

    public enum Operation
    {
        AND,
        OR
    }

    public enum JoinType
    {
        INNER,
        NATURAL,
        CROSS,
        LEFT_OUTER,
        RIGHT_OUTER,
        FULL_OUTER,
        LEFT,
        RIGHT,
        NONE
    }
}
