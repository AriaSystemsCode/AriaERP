using System;
using System.Collections.Generic;
using System.Text;
using System.Data.SqlClient;
using System.Data;
using System.Data.Common;
using System.Data.Odbc;
using System.Data.OleDb;

namespace AWS
{
    public class AmazonLog
    {

        private AMAZON_TRANSMISSION_LOG_TTableAdapters.AMAZON_TRANSMISSION_LOG_TTableAdapter _LogAdapter = null;

        public AMAZON_TRANSMISSION_LOG_TTableAdapters.AMAZON_TRANSMISSION_LOG_TTableAdapter LogAdapter
        {
            get
            {
                if (_LogAdapter == null)
                {
                    _LogAdapter = new AWS.AMAZON_TRANSMISSION_LOG_TTableAdapters.AMAZON_TRANSMISSION_LOG_TTableAdapter();
                    _LogAdapter.Connection = con;
                }
                return _LogAdapter;
            }
        }

        public SqlConnection con { get; set; }

        public AMAZON_TRANSMISSION_LOG_T.AMAZON_TRANSMISSION_LOG_TRow GetLatest()
        {
            SqlCommand cmd = con.CreateCommand();
            cmd.CommandText = "Select Top 1 * from AMAZON_TRANSMISSION_LOG_T ";
            cmd.CommandText += " where status='" + Status.Completed.ToString() + "' and hasnext=0 and downloaded >0 ";
            cmd.CommandText += " order by TRANSMISSION desc";
            AMAZON_TRANSMISSION_LOG_T.AMAZON_TRANSMISSION_LOG_TDataTable res = TypedDataConvertor<AMAZON_TRANSMISSION_LOG_T.AMAZON_TRANSMISSION_LOG_TDataTable>.convert(ExecuteDataTable(cmd));
            if (res.Count > 0)
                return res[0];
            return null;
        }

        public static DataTable ExecuteDataTable(DbCommand command)
        {
            DataTable dt = new DataTable("Results");
            DbDataAdapter dad = new SqlDataAdapter();
            if (command.GetType() == typeof(OdbcCommand))
                dad = new OdbcDataAdapter((OdbcCommand)command);
            else if (command.GetType() == typeof(OleDbCommand))
                dad = new OleDbDataAdapter((OleDbCommand)command);
            else if (command.GetType() == typeof(SqlCommand))
                dad = new SqlDataAdapter((SqlCommand)command);
            dad.Fill(dt);
            command.Connection.Close();
            return dt;
        }

        public bool insert(AMAZON_TRANSMISSION_LOG_T.AMAZON_TRANSMISSION_LOG_TRow logrow)
        {
            logrow.Table.Rows.Add(logrow);
            logrow.AcceptChanges();
            logrow.SetAdded();
            int x = LogAdapter.Update(logrow);
            return x > 0;
        }
    }
}