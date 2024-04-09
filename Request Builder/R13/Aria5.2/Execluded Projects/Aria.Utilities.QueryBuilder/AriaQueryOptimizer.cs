using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.QueryBuilder
{
    public class AriaQueryOptimizer
    {
        public string GetOptimizedQuery(AriaQuery query)
        {
            if (query is AriaSelectQuery)
                return GetOptimizedSelectQuery((AriaSelectQuery)query);

            else if (query is AriaUpdateQuery)
                return GetOptimizedUpdateQuery((AriaUpdateQuery)query);

            else if (query is AriaInsertQuery)
                return GetOptimizedInsertQuery((AriaInsertQuery)query);

            return GetOptimizedDeleteQuery((AriaDeleteQuery)query);
        }

        private string GetOptimizedSelectQuery(AriaSelectQuery query)
        {
            return GetSelectQuery(query);
        }

        private string GetOptimizedUpdateQuery(AriaUpdateQuery query)
        {
            return GetUpdateQuery(query);
        }

        private string GetOptimizedInsertQuery(AriaInsertQuery query)
        {
            return GetInsertQuery(query);
        }

        private string GetOptimizedDeleteQuery(AriaDeleteQuery query)
        {
            return GetDeleteQuery(query);
        }

        public string GetQuery(AriaQuery query)
        {
            if (query is AriaSelectQuery)
                return GetSelectQuery((AriaSelectQuery)query);

            else if (query is AriaUpdateQuery)
                return GetUpdateQuery((AriaUpdateQuery)query);

            else if (query is AriaInsertQuery)
                return GetInsertQuery((AriaInsertQuery)query);

            return GetDeleteQuery((AriaDeleteQuery)query);
        }

        private string GetSelectQuery(AriaSelectQuery query)
        {
            string queryText = "SELECT ";

            if (query.IsDistinct)
                queryText += "DISTINCT ";

            bool flag = false;
            foreach (AriaQueryTableField field in query.QueryTableFields)
            {
                if (flag)
                    queryText += ", ";

                queryText += field.getFieldName();
                flag = true;
            }

            queryText += " FROM ";

            flag = false;
            foreach (AriaQueryTable table in query.QueryTables)
            {
                if (query.IsJoin)
                {
                    if (flag)
                        queryText += " LEFT JOIN ";

                    if (table.DatabaseType == DatabaseType.FOX)
                    {
                        queryText += " OPENROWSET('MSDASQL','" + table.ConnectionString + "','SELECT * FROM " + table.TableName + "') AS " + table.TableAlias;

                        if (table.Filter != null)
                            queryText += " ON " + table.Filter;
                    }

                    else
                        queryText += table.TableName + " AS " + table.TableAlias;
                }

                else
                {
                    if (flag)
                        queryText += ", ";

                    queryText += table.TableName;
                }

                flag = true;
            }

            queryText += this.getOptimizedQueryWhereClause(query.QueryWhereClause);

            if (query.GroupBy != null)
            {
                queryText += " GROUP BY " + query.GroupBy.getFieldName();
            }

            if (query.OrderBy.Count > 0)
                queryText += " ORDER BY ";

            flag = false;
            foreach (AriaQueryTableField field in query.OrderBy)
            {
                if (flag)
                    queryText += ", ";

                queryText += field.getFieldName();
                flag = true;
            }

            return queryText;
        }

        private string GetUpdateQuery(AriaUpdateQuery query)
        {
            string queryText = "UPDATE " + ((AriaQueryTable)query.QueryTables[0]).TableName + " SET ";

            bool flag = false;
            foreach (AriaQueryTableField field in query.QueryTableFiled)
            {
                if (flag)
                    queryText += ", ";

                queryText += field.getFullField();
                flag = true;
            }

            queryText += this.getOptimizedQueryWhereClause(query.QueryWhereClause);

            return queryText;
        }

        private string GetInsertQuery(AriaInsertQuery query)
        {
            string queryText = "INSERT INTO " + ((AriaQueryTable)query.QueryTables[0]).TableName + " (";

            bool flag = false;
            foreach (AriaQueryTableField field in query.QueryTableFields)
            {
                if (flag)
                    queryText += ", ";

                queryText += field.getFieldName();
                flag = true;
            }

            queryText += ") VALUES(";

            flag = false;
            foreach (AriaQueryTableField field in query.QueryTableFields)
            {
                if (flag)
                    queryText += ", ";

                queryText += field.FieldValue;
                flag = true;
            }

            queryText += ")";

            return queryText;
        }

        private string GetDeleteQuery(AriaDeleteQuery query)
        {
            string queryText = "DELETE FROM " + ((AriaQueryTable)query.QueryTables[0]).TableName;

            queryText += this.getOptimizedQueryWhereClause(query.QueryWhereClause);

            return queryText;
        }
        
        private string getOptimizedQueryWhereClause(AriaQueryWhereClause whereClause)
        {
            if (whereClause.QueryWhereConditions.Count == 0)
                return "";

            string whereClauseStr = " WHERE ";

            int operationIndex = -1;
            foreach (AriaQueryWhereCondition condition in whereClause.QueryWhereConditions)
            {
                if (operationIndex >= 0)
                    whereClauseStr += " " + whereClause.Operations[operationIndex].ToString() + " ";

                whereClauseStr += condition.QueryTableField.getFullField(condition.QueryOperator);
                operationIndex++;
            }

            return whereClauseStr;
        }
    }
}
