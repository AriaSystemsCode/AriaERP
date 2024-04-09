using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Data;
using System.Data.SqlClient;

namespace Map
{
    class DataSetFromSQLCreator
    {

        public static DataSet Create(DataSet dataSet, SqlConnection con, Dictionary<string, string> TablesList)
        {
            if (dataSet == null)
                dataSet = new DataSet();
            SqlDataAdapter dataAdapter = new SqlDataAdapter("", con);
            SqlCommand cmd = con.CreateCommand();
            //foreach (string Table in TablesList.Keys)
            //{
            //    cmd.CommandText += "Select * from " + Table + " as " + Table + (TablesList[Table].Trim() != "" ? " Where " + TablesList[Table] : "");
            //    cmd.CommandText += "\r\nGO\r\n";
            //}
            ////cmd.CommandType = CommandType.StoredProcedure;
            //cmd.CommandText = "sp_executesql N'" + cmd.CommandText + "'";
            //dataAdapter.SelectCommand = cmd;
            //dataAdapter.Fill(dataSet);
            foreach (string Table in TablesList.Keys)
            {
                cmd.CommandText = "Select * from " + Table + (TablesList[Table].Trim() != "" ? " Where " + TablesList[Table] : "");
                dataAdapter.SelectCommand = cmd;
                DataTable table = new DataTable(Table);
                dataAdapter.Fill(table);
                dataSet.Tables.Add(table);
            }
            return dataSet;
        }
    }
}
