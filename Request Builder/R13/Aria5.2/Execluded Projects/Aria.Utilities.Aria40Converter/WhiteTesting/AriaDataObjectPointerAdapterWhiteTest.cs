using Aria.Utilities.Aria40Converter.SystemFilesAdaptor;
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
using System.Data.Odbc;
using System.Xml;
using System.Collections;
using Aria.Utilities.Aria40Converter.Helpers;
using System.Windows.Forms;
using Aria.Data.DataTypes;


namespace Aria.Utilities.Aria40Converter.WhiteTesting
{
    public class AriaDataObjectPointerAdapterWhiteTest
    {
        public List<string> GetPaths(ArrayList xx, List<string> pathes, string parentObject)
        {
            foreach (AriaDataPath DataPath in xx)
            {
                if (DataPath.ChildDataPaths.Count == 0)
                {
                    if (parentObject.Length > 0)
                        pathes.Add(parentObject + "." + DataPath.DataPath);
                    else
                        pathes.Add(DataPath.DataPath);
                }
                else
                    GetPaths(DataPath.ChildDataPaths, pathes, DataPath.DataPath);
            }

            return pathes;
        }

        public DataTable ariaDataObjectPointerAdapter(string clientID, string companyID)
        {
            DataTable table = new DataTable();
            table.Columns.Add("No.");
            table.Columns.Add("Object Name");
            table.Columns.Add("Data Path");
            table.Columns.Add("Result");
            table.Columns.Add("Error Category");
            table.Columns.Add("Error Message");
            
            AriaDbConnection connection = new AriaDbConnection();
            connection.CompanyName = companyID;

            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            ArrayList ariaobjects = objectDictionary.LoadAriaObjects(connection, clientID);

            foreach (AriaObject ariaObject in ariaobjects)
            {
                if (ariaObject.ObjectName.StartsWith("Aria4XP.") && 
                    ariaObject.ObjectType == AriaObjectTypes.Data && 
                    ariaObject.ObjectName.Split('.').Count() == 2)
                {
                    AriaDataObjectSettings ariaobjectRevision = (AriaDataObjectSettings)objectDictionary.LoadAriaObjectRevision(connection, ariaObject.ObjectName, "000001", "").ObjectRevisionSettings;
                        
                    AriaDataObjectPointerSettings pointerSettings = new AriaDataObjectPointerSettings();
                    pointerSettings.DataObjectName = ariaObject.ObjectName;
                    pointerSettings.DataObjectRevision = "000001";

                    AriaObjectDataPathsExplorer dataPathsExplorer = new AriaObjectDataPathsExplorer();
                    ArrayList PathTree = dataPathsExplorer.GetDataPathsTree(ariaObject.ObjectName, clientID);

                    List<string> dataPaths = new List<string>();
                    dataPaths = GetPaths(PathTree, dataPaths, "");

                    ArrayList ariaObjectProp = objectDictionary.LoadAriaObjectProperties(connection, ariaObject.ObjectName, "000001", false, "");

                    List<string> primaryKeyFields = new List<string>();
                    foreach (AriaObjectProperty pro in ariaObjectProp)
                    {
                        if (((AriaFieldSettings)pro.PropertySettings).IsPrimaryKey == true)
                        {
                            primaryKeyFields.Add(((AriaFieldSettings)pro.PropertySettings).FieldName);
                        }
                    }

                    try
                    {
                        AriaDbCommand commandSingleRecord;
                        if (ariaobjectRevision.DatabaseType == AriaDatabaseTypes.Aria40Data)
                        {
                            commandSingleRecord = new AriaDbCommand("Select TOP 1 * FROM " + ariaobjectRevision.TableName, connection, ariaobjectRevision.DatabaseType, clientID);
                        }
                        else
                        {
                            commandSingleRecord = new AriaDbCommand("Select TOP 1 * FROM " + ariaobjectRevision.TableName + " ORDER BY 1", connection, ariaobjectRevision.DatabaseType, clientID);
                        }

                        DataTable selectedtable = commandSingleRecord.GetDataTable();

                        if (selectedtable.Rows.Count > 0)
                        {
                            try
                            {
                                AriaDataObjectPointer pointerData = new AriaDataObjectPointer();
                                foreach (string key in primaryKeyFields)
                                {
                                    if (selectedtable.Rows[0][key.Trim()] == DBNull.Value) throw new Exception("One or more key field(s) is null");
                                    pointerData.AddKeyField(key.Trim(), selectedtable.Rows[0][key.Trim()]);
                                }

                                AriaDataObjectPointerAdapter AriaDataObjectPointer = default(AriaDataObjectPointerAdapter);
                                string error = "";
                                AriaDataObjectPointer = new AriaDataObjectPointerAdapter(connection, pointerSettings, pointerData, clientID, ref error);
                                AriaDataObjectPointer.TestingMode = true;

                                if (error != "")
                                {
                                    table.Rows.Add(ariaObject.ObjectID, ariaObject.ObjectName, "", "", "ErrorWhileCreateDataAccessObject", error);
                                }

                                foreach (string path in dataPaths)
                                {
                                    string realpath = "";
                                    if (path.Split('.').Length > 1)
                                    {
                                        realpath = ariaObject.ObjectName + "." + path.Split('.')[0];
                                    }
                                    else
                                    {
                                        realpath = ariaObject.ObjectName;
                                    }

                                    if (AriaDataObjectPointer.LoadedRecords.Where(p => p.Key == realpath &&
                                                                                  p.Value != null &&
                                                                                  p.Value.Rows.Count > 0).Count() >= 1)
                                    {
                                        try
                                        {
                                            string result = AriaDataObjectPointer.GetData(path, clientID).ToString().RemoveSpecialChar().TrimEnd();
                                            table.Rows.Add(ariaObject.ObjectID, ariaObject.ObjectName, path, result, "", "");
                                        }
                                        catch (Exception ex)
                                        {
                                            table.Rows.Add(ariaObject.ObjectID, ariaObject.ObjectName, "", "", "ErrorWhileGetDataPath", ex.Message);
                                        }
                                    }
                                    else
                                    {
                                        table.Rows.Add(ariaObject.ObjectID, ariaObject.ObjectName, path, "", "WariningDataPath (No Data for Related Record)", "");
                                    }
                                }
                            }
                            catch (Exception ex)
                            {
                                foreach (string path in dataPaths)
                                {
                                    if (ex.Message == "One or more key field(s) is null")
                                    {
                                        table.Rows.Add(ariaObject.ObjectID, ariaObject.ObjectName, "", "", "WariningDataPath (One or more key field(s) is null.)", ex.Message);
                                    }
                                    else
                                    {
                                        table.Rows.Add(ariaObject.ObjectID, ariaObject.ObjectName, "", "", "WariningDataPath (Error While Get Primary Key)", ex.Message);
                                    }
                                }
                            }
                        }
                        else
                        {
                            foreach (string path in dataPaths)
                            {
                                table.Rows.Add(ariaObject.ObjectID, ariaObject.ObjectName, "", "", "WariningDataPath (Empty Table)", "");
                            }
                        }
                    }
                    catch(Exception ex)
                    {
                        foreach (string path in dataPaths)
                        {
                            table.Rows.Add(ariaObject.ObjectID, ariaObject.ObjectName, "", "", "WariningDataPath (Error While Get First Record)", ex.Message);
                        }
                    }
                }
            }

            return table;
        }
    }
}