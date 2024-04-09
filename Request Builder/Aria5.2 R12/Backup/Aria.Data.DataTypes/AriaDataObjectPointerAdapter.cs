using System.Data;
using System.Collections.Generic;
using Aria.DataTypes;
using Aria.DataTypes.Settings;
using Aria.DataTypes.ObjectDictionary;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using System;
using System.Collections;
using System.IO;
using Aria.HelperClass.SAAS;
using System.Diagnostics;

namespace Aria.Data.DataTypes
{
    public class AriaDataObjectPointerAdapter : AriaDataTypeAdapter
    {
        // T20110803.0001 MAH 8/2/2011
        private Dictionary<string, AriaDbCommand> _loadedCommand = new Dictionary<string, AriaDbCommand>();
        public bool TestingMode;
        // T20110803.0001 MAH 8/2/2011 End

        private Dictionary<string, DataTable> _loadedRecords = new Dictionary<string, DataTable>();
        public Dictionary<string, DataTable> LoadedRecords
        {
            get { return _loadedRecords; }
            set { _loadedRecords = value; }
        }

        private string _objectName = "";


        // For Testing only
        public AriaDataObjectPointerAdapter(AriaDbConnection connection, AriaDataTypeSettings settings, AriaDataType value, string clientId, ref string error)
            : base(connection, settings, value)
        {

            TestingMode = true;
            //[ADD]Ahmed Maher -Date: 30/03/2010
            AriaLogManage LogM = new AriaLogManage();
            //[END]
            try
            {
                AriaDataObjectPointerSettings pointerSettings = (AriaDataObjectPointerSettings)settings;
                AriaDataObjectPointer pointerData = (AriaDataObjectPointer)Value;

                AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
                AriaObject ariaObject = objectDictionary.LoadAriaObjectByName(_connection, pointerSettings.DataObjectName, clientId);
                AriaObjectRevision ariaObjectRevision = objectDictionary.LoadActiveRevision(_connection, pointerSettings.DataObjectName, clientId);

                _objectName = pointerSettings.DataObjectName;

                AriaDataObjectSettings DataObjectSettings = (AriaDataObjectSettings)ariaObjectRevision.ObjectRevisionSettings;
                string filter = DataObjectSettings.Filter;
                //string sqlCommand = "SELECT * FROM " + DataObjectSettings.TableName + " WHERE " + filter;
                string ctable = "";
                ctable = filter.Contains(".") ? filter.Substring(0, filter.IndexOf(".")) : DataObjectSettings.TableName;

                // T20110803.0001 MAH 8/2/2011
                // string sqlCommand = "SELECT * FROM " + DataObjectSettings.TableName + " AS " + ctable + " WHERE " + filter;
                string sqlCommand = "SELECT * FROM " + DataObjectSettings.TableName + " WHERE " + filter;
                // T20110803.0001 MAH 8/2/2011
                sqlCommand = sqlCommand.ToUpper();

                AriaDbCommand command = new AriaDbCommand(sqlCommand, connection, DataObjectSettings.DatabaseType, clientId);


                for (int feildIndex = 0; feildIndex < pointerData.KeyFields.Count; feildIndex++)
                {
                    //[Modified]Ahmed Maher -Date: 31/03/2010 -Notify Error
                    //command.Parameters.Add(new AriaDbParameter(((AriaDataObjectPointerKeyField)pointerData.KeyFields[feildIndex]).FieldName, ((AriaDataObjectPointerKeyField)pointerData.KeyFields[feildIndex]).Value));
                    // sqlCommand = sqlCommand.Replace("@" + ((AriaDataObjectPointerKeyField)pointerData.KeyFields[feildIndex]).FieldName.ToUpper() + "@", ((AriaDataObjectPointerKeyField)pointerData.KeyFields[feildIndex]).Value.ToString());
                    // T20110803.0001 MAH 8/2/2011
                    // sqlCommand = sqlCommand.Replace("@" + ((AriaDataObjectPointerKeyField)pointerData.KeyFields[feildIndex]).FieldName.ToUpper() + "@", ((AriaDataObjectPointerKeyField)pointerData.KeyFields[feildIndex]).Value.ToString().ToUpper());
                    command.Parameters.Add(((AriaDataObjectPointerKeyField)pointerData.KeyFields[feildIndex]).FieldName.ToUpper().Trim(), ((AriaDataObjectPointerKeyField)pointerData.KeyFields[feildIndex]).Value);
                    // T20110803.0001 MAH 8/2/2011


                    //[END]
                }
                //Trace
                //[END]
                AriaDataProvider dataProvider = new AriaDataProvider();

                LoadedRecords[ariaObject.ObjectName] = dataProvider.GetDataTable(command);

                // T20110803.0001 MAH 8/2/2011
                _loadedCommand.Add(ariaObject.ObjectName, command);
                // T20110803.0001 MAH 8/2/2011 End

                LoadRelatedData(ariaObject, clientId);
            }
            catch (Exception Ex)
            {
                EventLog.WriteEntry("Request Data Error1", Ex.Message, EventLogEntryType.Information);
                EventLog.WriteEntry("Request Data Error1", Ex.GetBaseException().Message, EventLogEntryType.Information);

                //[Modified]Ahmed Maher -Date: 30/03/2010
                //LogM.AddLog("Error", "Aria.Data.DataTypes.AriaDataObjectPointerAdapter.AriaDataObjectPointerAdapter", "Ex = " + Ex.Message, clientId);
                //[END]

                error = Ex.Message;
            }
        }

        public AriaDataObjectPointerAdapter(AriaDbConnection connection, AriaDataTypeSettings settings, AriaDataType value, string clientId)
            : base(connection, settings, value)
        {

            //[ADD]Ahmed Maher -Date: 30/03/2010
            AriaLogManage LogM = new AriaLogManage();
            //[END]
            try
            {
                AriaDataObjectPointerSettings pointerSettings = (AriaDataObjectPointerSettings)settings;
                AriaDataObjectPointer pointerData = (AriaDataObjectPointer)Value;

                AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
                AriaObject ariaObject = objectDictionary.LoadAriaObjectByName(_connection, pointerSettings.DataObjectName, clientId);
                AriaObjectRevision ariaObjectRevision = objectDictionary.LoadActiveRevision(_connection, pointerSettings.DataObjectName, clientId);

                _objectName = pointerSettings.DataObjectName;

                AriaDataObjectSettings DataObjectSettings = (AriaDataObjectSettings)ariaObjectRevision.ObjectRevisionSettings;
                string filter = DataObjectSettings.Filter;
                //string sqlCommand = "SELECT * FROM " + DataObjectSettings.TableName + " WHERE " + filter;
                string ctable = "";
                ctable = filter.Contains(".") ? filter.Substring(0, filter.IndexOf(".")) : DataObjectSettings.TableName;

                // T20110803.0001 MAH 8/2/2011
                // string sqlCommand = "SELECT * FROM " + DataObjectSettings.TableName + " AS " + ctable + " WHERE " + filter;
                string sqlCommand = "SELECT * FROM " + DataObjectSettings.TableName + " WHERE " + filter;
                // T20110803.0001 MAH 8/2/2011
                sqlCommand = sqlCommand.ToUpper();

                AriaDbCommand command = new AriaDbCommand(sqlCommand, connection, DataObjectSettings.DatabaseType, clientId);


                for (int feildIndex = 0; feildIndex < pointerData.KeyFields.Count; feildIndex++)
                {
                    //[Modified]Ahmed Maher -Date: 31/03/2010 -Notify Error
                    //command.Parameters.Add(new AriaDbParameter(((AriaDataObjectPointerKeyField)pointerData.KeyFields[feildIndex]).FieldName, ((AriaDataObjectPointerKeyField)pointerData.KeyFields[feildIndex]).Value));
                    // sqlCommand = sqlCommand.Replace("@" + ((AriaDataObjectPointerKeyField)pointerData.KeyFields[feildIndex]).FieldName.ToUpper() + "@", ((AriaDataObjectPointerKeyField)pointerData.KeyFields[feildIndex]).Value.ToString());
                    // T20110803.0001 MAH 8/2/2011
                    // sqlCommand = sqlCommand.Replace("@" + ((AriaDataObjectPointerKeyField)pointerData.KeyFields[feildIndex]).FieldName.ToUpper() + "@", ((AriaDataObjectPointerKeyField)pointerData.KeyFields[feildIndex]).Value.ToString().ToUpper());
                    command.Parameters.Add(((AriaDataObjectPointerKeyField)pointerData.KeyFields[feildIndex]).FieldName.ToUpper().Trim(), ((AriaDataObjectPointerKeyField)pointerData.KeyFields[feildIndex]).Value);
                    // T20110803.0001 MAH 8/2/2011


                    //[END]
                }
                //Trace
                //[END]
                AriaDataProvider dataProvider = new AriaDataProvider();

                LoadedRecords[ariaObject.ObjectName] = dataProvider.GetDataTable(command);

                // T20110803.0001 MAH 8/2/2011
                _loadedCommand.Add(ariaObject.ObjectName, command);
                // T20110803.0001 MAH 8/2/2011 End

                LoadRelatedData(ariaObject, clientId);
            }
            catch (Exception Ex)
            {
                EventLog.WriteEntry("Request Data Error1", Ex.Message, EventLogEntryType.Information);
                EventLog.WriteEntry("Request Data Error1", Ex.GetBaseException().Message, EventLogEntryType.Information);

                //[Modified]Ahmed Maher -Date: 30/03/2010
                //LogM.AddLog("Error", "Aria.Data.DataTypes.AriaDataObjectPointerAdapter.AriaDataObjectPointerAdapter", "Ex = " + Ex.Message, clientId);
                //[END]

                if (TestingMode) throw Ex;
            }
        }

        private void LoadRelatedData(AriaObject ariaObject, string clientId)
        {
            string errors = "";


            //[ADD]Ahmed Maher -Date: 30/03/2010
            AriaLogManage LogM = new AriaLogManage();
            //[END]
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            ArrayList childrenObjects = objectDictionary.LoadAriaObjectChildObjectsOfType(_connection, ariaObject.ObjectName, AriaObjectTypes.RelatedData, clientId);
            for (int childIndex = 0; childIndex < childrenObjects.Count; childIndex++)
            {
                try
                {
                    AriaObjectRevision ariaObjectRevision = objectDictionary.LoadActiveRevision(_connection, ((AriaObject)childrenObjects[childIndex]).ObjectName, clientId);

                    AriaRelatedDataObjectSettings RelatedDataObjectSettings = (AriaRelatedDataObjectSettings)ariaObjectRevision.ObjectRevisionSettings;

                    AriaDataObjectSettings DataObjectSettings = (AriaDataObjectSettings)
                                            objectDictionary.LoadActiveRevision(_connection, RelatedDataObjectSettings.
                                                                                DataObjectName, clientId).ObjectRevisionSettings;

                    string filter = RelatedDataObjectSettings.Filter.Split(new char[] { '|' })[0];
                    string ctable = "";
                    ctable = filter.Contains(".") ? filter.Substring(0, filter.IndexOf(".")) : DataObjectSettings.TableName;

                    //string sqlCommand = "SELECT * FROM " + DataObjectSettings.TableName + " WHERE " + filter;
                    // T20110803.0001 MAH 8/2/2011
                    // string sqlCommand = "SELECT * FROM " + DataObjectSettings.TableName + " AS " + ctable + " WHERE " + filter;
                    string sqlCommand = "SELECT * FROM " + DataObjectSettings.TableName + " WHERE " + filter;
                    // T20110803.0001 MAH 8/2/2011 End
                    AriaDbCommand command = new AriaDbCommand(sqlCommand, _connection, DataObjectSettings.DatabaseType, clientId);

                    DataTable table = LoadedRecords[ariaObject.ObjectName];
                    for (int columnIndex = 0; columnIndex < table.Columns.Count; columnIndex++)
                    {
                        // T20110803.0001 MAH 8/2/2011
                        //if (!filter.ToUpper().Contains("@" + table.Columns[columnIndex].ColumnName.ToUpper() + "@"))
                        //    continue;
                        if (!filter.ToUpper().Contains("@" + table.Columns[columnIndex].ColumnName.ToUpper()))
                            continue;
                        // T20110803.0001 MAH 8/2/2011 End

                        command.Parameters.Add(new AriaDbParameter(table.Columns[columnIndex].ColumnName.Trim(), table.Rows[0][columnIndex]));
                    }

                    AriaDataProvider dataProvider = new AriaDataProvider();

                    LoadedRecords[((AriaObject)childrenObjects[childIndex]).ObjectName] = dataProvider.GetDataTable(command);

                    // T20110803.0001 MAH 8/2/2011
                    _loadedCommand.Add(((AriaObject)childrenObjects[childIndex]).ObjectName, command);
                    // T20110803.0001 MAH 8/2/2011 End

                    LoadRelatedData((AriaObject)childrenObjects[childIndex], clientId);
                }
                catch (Exception EX)
                {
                    //[Add]Ahmed Maher -Date: 30/03/2010
                    //LogM.AddLog("Error", "Aria.Data.DataTypes.AriaDataObjectPointerAdapter.LoadRelatedData", "Ex = " + EX.Message, clientId);
                    //[END]

                    errors = EX.Message;
                }
            }

            if (errors != "") if (TestingMode) throw new Exception(errors);
        }

        public object GetCalculatedData(string dataPath, string fieldName, string clientId)
        {
            string objectName = dataPath.Substring(0, dataPath.LastIndexOf("."));

            AriaDbCommand command = _loadedCommand[objectName];

            string oldCommand = command.CommandText;

            try
            {
                if (fieldName.Contains("|")) fieldName = fieldName.Split('|')[0];

                command.CommandText = command.CommandText.Replace("*", fieldName + " AS CalculatedField ");

                AriaDataProvider dataProvider = new AriaDataProvider();
                DataTable result = dataProvider.GetDataTable(command);

                if (result.Rows.Count > 0)
                {
                    command.CommandText = oldCommand;
                    return result.Rows[0]["CalculatedField"];
                }
                else
                {
                    command.CommandText = oldCommand;
                    return "";
                }
            }
            catch(Exception ex)
            {
                if (TestingMode) throw ex;
            }

            command.CommandText = oldCommand;

            return "";
        }

        public override object GetData(string dataPath, string clientId)
        {
            //[ADD]Ahmed Maher -Date: 30/03/2010
            AriaLogManage LogM = new AriaLogManage();
            //[END]
            try
            {
                dataPath = _objectName + "." + dataPath;
                int tempIndex = dataPath.LastIndexOf('.');
                string objectName = dataPath.Substring(0, tempIndex);
                string propertyName = dataPath.Substring(tempIndex + 1, dataPath.Length - tempIndex - 1);
                if (LoadedRecords[objectName].Rows.Count == 0) return "";

                AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

                string objectRevision = objectDictionary.LoadActiveRevision(_connection, objectName, clientId).ObjectRevision;

                AriaObjectProperty objectProperty = objectDictionary.LoadAriaObjectProperty(_connection, objectName, objectRevision, propertyName, clientId);
                AriaDataTypeSettings objectPropertSettings = null;

                bool isFeild = objectProperty.PropertyType.Equals(Aria.DataTypes.AriaDataTypes.AriaField);
                if (LoadedRecords.ContainsKey(objectName))
                {
                    if (isFeild)
                    {
                        objectPropertSettings = (AriaFieldSettings)objectProperty.PropertySettings;

                        // T20110803.0001 MAH 8/2/2011
                        string fieldName = ((AriaFieldSettings)objectPropertSettings).FieldName;

                        if (fieldName.Contains("(") ||
                            fieldName.Contains("+") || fieldName.Contains("-") || fieldName.Contains("*") || fieldName.Contains("/"))
                        {
                            return GetCalculatedData(dataPath, fieldName, clientId);
                        }
                        // T20110803.0001 MAH 8/2/2011 End

                        return LoadedRecords[objectName].Rows[0][((AriaFieldSettings)objectPropertSettings).FieldName.TrimEnd()];
                    }

                    else
                    {
                        try
                        {
                            objectPropertSettings = (AriaRelatedFieldSettings)objectProperty.PropertySettings;

                            // T20110803.0001 MAH 8/2/2011
                            string fieldName = ((AriaRelatedFieldSettings)objectPropertSettings).FieldName;

                            if (fieldName.Contains("(") ||
                                fieldName.Contains("+") || fieldName.Contains("-") || fieldName.Contains("*") || fieldName.Contains("/"))
                            {
                                return GetCalculatedData(dataPath, fieldName, clientId);
                            }
                            // T20110803.0001 MAH 8/2/2011 End

                            return LoadedRecords[objectName].Rows[0][((AriaRelatedFieldSettings)objectPropertSettings).FieldName.TrimEnd()];
                        }
                        catch(Exception ex)
                        {
                            EventLog.WriteEntry("Request Data Error2", ex.Message, EventLogEntryType.Information);
                            EventLog.WriteEntry("Request Data Error2", ex.GetBaseException().Message, EventLogEntryType.Information);

                            objectPropertSettings = (AriaFieldSettings)objectProperty.PropertySettings;
                            if (TestingMode) throw ex;
                            return LoadedRecords[objectName].Rows[0][((AriaFieldSettings)objectPropertSettings).FieldName.TrimEnd()];
                        }
                    }
                }
                else
                {
                    // T20110803.0001 MAH 8/2/2011
                    // return null;
                    return "";
                    // T20110803.0001 MAH 8/2/2011 End
                }
            }
            catch (Exception Ex)
            {
                EventLog.WriteEntry("Request Data Error3", Ex.Message, EventLogEntryType.Information);
                EventLog.WriteEntry("Request Data Error3", Ex.GetBaseException().Message, EventLogEntryType.Information);

                //[Modified]Ahmed Maher -Date: 30/03/2010
                //LogM.AddLog("Error", "Aria.Data.DataTypes.AriaDataObjectPointerAdapter.GetData", Ex.Message, clientId);
                if (TestingMode) throw Ex;
                //[END]
            }

            // T20110803.0001 MAH 8/2/2011
            // return new object();
            return "";
            // T20110803.0001 MAH 8/2/2011 End
        }

        public override void SetData(string dataPath, object value)
        {
            throw new System.Exception("The method or operation is not implemented.");
        }
    }
}
