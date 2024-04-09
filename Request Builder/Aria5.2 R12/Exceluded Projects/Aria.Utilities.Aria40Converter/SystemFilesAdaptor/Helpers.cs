using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Utilities.Aria40Converter;
using Aria.Environment;
using Aria.DataTypes.ObjectDictionary;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.Data;
using Aria.Xml;
using System.Text.RegularExpressions;
using System.Data;
using Aria.DataTypes.Settings;
using Aria.DataTypes;
using Aria.Utilities.Aria40Converter.SystemFilesAdaptor;
using System.Data.Odbc;
using System.Xml;
using System.IO;
using System.Xml.Linq;

namespace Aria.Utilities.Aria40Converter.SystemFilesAdaptor
{
    class Helpers
    {
        public static void AppendRecordToException(string mergeFileName, DataRow sourceRow, string primaryKey, string reason)
        {
            if (!File.Exists(mergeFileName))
            {
                DataTable tempTable = sourceRow.Table.Copy();
                tempTable.Rows.Clear();
                tempTable.Columns.Add("ModeficationType", typeof(string));
                tempTable.Columns.Add("_Reason", typeof(string));
                tempTable.WriteXml(mergeFileName, XmlWriteMode.WriteSchema);
            }

            DataSet ds = new DataSet();
            ds.ReadXml(mergeFileName);
            DataTable table = ds.Tables[0];

            string sourceKey = "";
            for(int i = 0; i < primaryKey.Split(',').Length; i++)
            {
                sourceKey += sourceRow[primaryKey.Split(',')[i].Trim()].ToString();
            }

            DataRow modifyRow = null;

            foreach (DataRow targetRow in table.Rows)
            {
                string targetKey = "";
                for (int i = 0; i < primaryKey.Split(',').Length; i++)
                {
                    targetKey += targetRow[primaryKey.Split(',')[i].Trim()].ToString();
                }

                if (sourceKey == targetKey)
                {
                    modifyRow = targetRow;
                }
            }

            if (modifyRow == null)
            {
                modifyRow = table.NewRow();
                table.Rows.Add(modifyRow);
            }

            modifyRow["ModeficationType"] = "Remove";

            for (int i = 0; i < sourceRow.Table.Columns.Count; i++)
            {
                modifyRow[sourceRow.Table.Columns[i].ColumnName] = sourceRow[sourceRow.Table.Columns[i].ColumnName];
            }

            if (modifyRow["_Reason"].ToString().Trim() != "") modifyRow["_Reason"] += ", ";
            modifyRow["_Reason"] += reason;

            table.WriteXml(mergeFileName, XmlWriteMode.WriteSchema);
        }

        public static void AppendRecordToException(string mergeFileName, DataTable sourceTable, string primaryKey, string reason, bool noCheck)
        {
            if (!File.Exists(mergeFileName))
            {
                DataTable tempTable = sourceTable.Copy();
                tempTable.Rows.Clear();
                tempTable.Columns.Add("ModeficationType", typeof(string));
                tempTable.Columns.Add("_Reason", typeof(string));
                tempTable.WriteXml(mergeFileName, XmlWriteMode.WriteSchema);
            }

            DataSet ds = new DataSet();
            ds.ReadXml(mergeFileName);
            DataTable table = ds.Tables[0];

            foreach (DataRow sourceRow in sourceTable.Rows)
            {
                string sourceKey = "";
                for (int i = 0; i < primaryKey.Split(',').Length; i++)
                {
                    sourceKey += sourceRow[primaryKey.Split(',')[i].Trim()].ToString();
                }

                DataRow modifyRow = null;

                if (!noCheck)
                {
                    foreach (DataRow targetRow in table.Rows)
                    {
                        string targetKey = "";
                        for (int i = 0; i < primaryKey.Split(',').Length; i++)
                        {
                            targetKey += targetRow[primaryKey.Split(',')[i].Trim()].ToString();
                        }

                        if (sourceKey == targetKey)
                        {
                            modifyRow = targetRow;
                        }
                    }
                }

                if (modifyRow == null)
                {
                    modifyRow = table.NewRow();
                    table.Rows.Add(modifyRow);
                }

                modifyRow["ModeficationType"] = "Remove";

                for (int i = 0; i < sourceRow.Table.Columns.Count; i++)
                {
                    modifyRow[sourceRow.Table.Columns[i].ColumnName] = sourceRow[sourceRow.Table.Columns[i].ColumnName];
                }

                if (modifyRow["_Reason"].ToString().Trim() != "") modifyRow["_Reason"] += ", ";
                modifyRow["_Reason"] += reason;
            }

            table.WriteXml(mergeFileName, XmlWriteMode.WriteSchema);
        }

        public static DataTable MergeTables(DataTable Source, string xmlFilePath, string[] primaryKey)
        {
            if (File.Exists(xmlFilePath))
            {
                DataSet dt = new DataSet();
                dt.ReadXml(xmlFilePath, XmlReadMode.Auto);

                if (dt.Tables.Count > 0)
                {
                    foreach (DataRow row in dt.Tables[0].Rows)
                    {
                        //Remove
                        if (row["ModeficationType"].ToString() == "Remove")
                        {
                            string filter = "";

                            for (int i = 0; i < primaryKey.Length; i++)
                            {
                                if (i > 0)
                                {
                                    filter += " AND ";
                                }

                                filter += primaryKey[i] + " = '" + row[primaryKey[i]].ToString().ToUpper().Trim().Replace("'", "''") + "'";
                            }

                            if (Source.Select(filter) != null && Source.Select(filter).Count() > 0)
                            {
                                foreach (DataRow rows in Source.Select(filter))
                                {
                                    Source.Rows.Remove(rows);
                                }
                            }
                        }
                        //Add
                        else if (row["ModeficationType"].ToString() == "Add")
                        {
                            DataRow sourceRow = Source.NewRow();

                            for (int i = 0; i < Source.Columns.Count; i++)
                            {
                                sourceRow[Source.Columns[i].ColumnName] = row[Source.Columns[i].ColumnName];
                            }
                            Source.Rows.Add(sourceRow);
                        }
                        //Modify
                        else if (row["ModeficationType"].ToString() == "Modify")
                        {
                            string filter = "";

                            for (int i = 0; i < primaryKey.Length; i++)
                            {
                                if (i > 0)
                                {
                                    filter += " AND ";
                                }

                                filter += primaryKey[i] + " = '" + row[primaryKey[i]] + "'";
                            }
                            if (Source.Select(filter).Count() > 0)
                            {
                                for (int i = 0; i < Source.Columns.Count; i++)
                                {
                                    Source.Select(filter).First()[Source.Columns[i].ColumnName] = row[Source.Columns[i].ColumnName];
                                }
                            }
                        }
                    }
                }

                return Source;
            }
            else
            {
                return Source;
            }
        }
    }
}
