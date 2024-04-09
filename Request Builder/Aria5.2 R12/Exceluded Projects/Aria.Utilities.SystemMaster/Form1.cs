using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Data.SqlClient;

namespace Aria.Utilities.SystemMaster
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            DataSet y = new DataSet();

            DataTable x = new DataTable("AriaObject");
            x.Columns.Add("ParentObjectID$", typeof(string));
            x.Columns.Add("ObjectName#", typeof(string));
            x.Columns.Add("ObjectDescription", typeof(string));
            x.Columns.Add("ObjectType", typeof(string));
            x.Columns.Add("ActiveRevision", typeof(string));

            x.Rows.Add("Select ObjectID From AriaObject Where ObjectName = 'Aria4XP'", "Aria4XP.New1", "New1", "Data1", "001.000");

            //Update(x);

            y.Tables.Add(x);

            y.WriteXml(@"d:\System.Master.xml", XmlWriteMode.WriteSchema);


        }


        public void Update(DataTable table)
        {
            SqlConnection con = new SqlConnection(@"Data Source=.\SQLSERVER2008R2;Initial Catalog=System.Master;Integrated Security=True");
            con.Open();

            List<string> keys = new List<string>();
            List<object> keyValues = new List<object>();
            List<string> fields = new List<string>();
            List<object> values = new List<object>();

            for (int i = 0; i < table.Rows.Count; i++)
            {
                for (int col = 0; col < table.Columns.Count; col++)
                {
                    if (table.Columns[col].ColumnName.EndsWith("$"))
                    {
                        string subQuery = table.Rows[i][table.Columns[col].ColumnName].ToString();
                        SqlCommand subQueryCommand = new SqlCommand(subQuery, con);

                        fields.Add(table.Columns[col].ColumnName.Replace("$", ""));
                        values.Add(subQueryCommand.ExecuteScalar());
                    }
                    else if (table.Columns[col].ColumnName.EndsWith("#"))
                    {
                        keys.Add(table.Columns[col].ColumnName.Replace("#", ""));
                        keyValues.Add(table.Rows[i][table.Columns[col].ColumnName.Replace("$", "")]);

                        fields.Add(table.Columns[col].ColumnName.Replace("#", ""));
                        values.Add(table.Rows[i][table.Columns[col].ColumnName.Replace("$", "")]);
                    }
                    else
                    {
                        fields.Add(table.Columns[col].ColumnName);
                        values.Add(table.Rows[i][table.Columns[col].ColumnName]);
                    }
                }
            }

            string selectSql = "SELECT Count(*) FROM " + table.TableName + " WHERE ";
            for (int i = 0; i < keys.Count; i++)
            {
                if (i != 0) selectSql += " AND ";
                selectSql += keys[i] + " = @" + keys[i];
            }

            SqlCommand selectCommand = new SqlCommand(selectSql, con);

            for (int i = 0; i < keys.Count; i++)
            {
                selectCommand.Parameters.Add(new SqlParameter(keys[i], keyValues[i]));
            }

            object selectResult = selectCommand.ExecuteScalar();
            if (selectResult == DBNull.Value || Convert.ToInt32(selectResult) == 0)
            {
                string insertSql = "INSERT INTO " + table.TableName + " ";

                insertSql += "(";

                for (int i = 0; i < fields.Count; i++)
                {
                    if (i != 0) insertSql += ",";
                    insertSql += fields[i];
                }
                insertSql += ") VALUES (";

                for (int i = 0; i < fields.Count; i++)
                {
                    if (i != 0) insertSql += ",";
                    insertSql += "@" + fields[i];
                }
                insertSql += ")";

                SqlCommand insertCommand = new SqlCommand(insertSql, con);

                for (int i = 0; i < fields.Count; i++)
                {
                    insertCommand.Parameters.Add(new SqlParameter(fields[i], values[i]));
                }

                insertCommand.ExecuteNonQuery();
            }
            else
            {
                string updateSql = "UPDATE " + table.TableName + " SET ";

                for (int i = 0; i < fields.Count; i++)
                {
                    if (i != 0) updateSql += ",";
                    updateSql += fields[i] + " = @" + fields[i];
                }
                updateSql += " WHERE ";

                for (int i = 0; i < keys.Count; i++)
                {
                    if (i != 0) updateSql += " AND ";
                    updateSql += keys[i] + " = @" + keys[i];
                }

                SqlCommand updateCommand = new SqlCommand(updateSql, con);

                for (int i = 0; i < fields.Count; i++)
                {
                    updateCommand.Parameters.Add(new SqlParameter(fields[i], values[i]));
                }

                updateCommand.ExecuteNonQuery();
            }

            con.Close();
        }
    }
}
