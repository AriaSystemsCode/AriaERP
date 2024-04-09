using System;
using System.Collections.Generic;
using System.Text;
using Aria.Utilities.ObjectDictionaryBuilder.Aria27ObjectDictionaryConverter;
using Aria.Environment;
using Aria.DataTypes.ObjectDictionary;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.Data;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Xml;
using System.Text.RegularExpressions;
using System.Data;
using Aria.DataTypes.Settings;
using Aria.DataTypes;

namespace Aria.Utilities.ObjectDictionaryBuilder
{
    /// <summary>
    /// This class contains all information that used to generate Object dictionary
    /// </summary>
    public class AriaObjectDictionaryBuilder
    {
        public void BuildDataObjects(string clientId)
        {
            // Add Parent Notes
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
            objectDictionary.SaveAriaObject(new AriaDbConnection("", ""), "Aria4XP", AriaObjectTypes.Package, 0, "001.000", clientId);

            int RootObjectId = objectDictionary.LoadAriaObjectByName(new AriaDbConnection("", ""), "Aria4XP", clientId).ObjectID;
            
            // Add Child Nodes
            AriaEnviromentVariables enviromentVariables = new AriaEnviromentVariables();
            //T20100512.0026 Hassan 2010 05 23 [Begin]
            enviromentVariables.ClientID = clientId;
            enviromentVariables.ConnectionsRefresh();
            //T20100512.0026 Hassan 2010 05 23 [END]

            Aria27SchemaInformation schema = new Aria27SchemaInformation(@"Driver={Microsoft Visual FoxPro Driver};sourcedb=C:\MY WORK\ARIA27\SYSFILES;sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes");

            Table[] tables = schema.GetTables();

            foreach (Table table in tables)
            {
                Index[] indices = schema.GetIndexes(table.TableName);

                Index primary = null;
                foreach (Index index in indices)
                {
                    if (index.Type.Equals(EnumIndexType.Index))
                        continue;

                    primary = index;
                    break;
                }

                if (primary == null)
                    continue;

                table.Description = "Aria4XP." + removeSpecialCharacters(table.Description);
                objectDictionary.SaveAriaObject(new AriaDbConnection("", ""), table.Description, AriaObjectTypes.Data, RootObjectId, "001.000", clientId);
                AriaObject ariaObject = objectDictionary.LoadAriaObjectByName(new AriaDbConnection("", ""), table.Description, clientId);

                AriaDataObjectSettings settings = new AriaDataObjectSettings();
                settings.DatabaseType = (table.DatabaseType.Equals(DatabaseType.Aria27Data) ? AriaDatabaseTypes.Aria27Data : AriaDatabaseTypes.Aria40Data);
                
                IndexColumn[] columns = schema.GetIndexColumns(table.TableName, primary.IndexName);

                string filter = schema.getIndexExpression(table.TableName, primary.IndexName);
                for (int index = 0; index < columns.Length; index++)
                {
                    filter = filter.Replace(columns[index].ColumnName, "'@" + columns[index].ColumnName + "@'");
                }

                settings.Filter = schema.getIndexExpression(table.TableName, primary.IndexName) + " = " + filter;

                settings.ModificationType = AriaModificationTypes.Add;
                settings.ParentDataObjectRevision = "";
                settings.TableName = table.TableName;

                AriaXmlSerializer xmlSerializer = new AriaXmlSerializer();
                string xmlSettings = xmlSerializer.ConvertToXml(settings);

                objectDictionary.SaveAriaObjectRevision(new AriaDbConnection("", ""), ariaObject.ObjectID, "001.000", xmlSettings, clientId);

                buildObjectProperties(ariaObject.ObjectName, table.TableName, columns, clientId);
            }
        }

        private string removeSpecialCharacters(string text)
        {
            string result = "";

            foreach (char c in text.ToCharArray())
            {
                if (!char.IsLetterOrDigit(c))
                    continue;

                result += c;
            }

            return result;
        }

        public void buildObjectProperties(string objectName, string tableName, IndexColumn[] indexColumns,string  clientId)
        {
            AriaDbCommand command = new AriaDbCommand("SELECT SYDFIELD.*, sydflfld.nfld_pos FROM SYDFIELD join sydflfld on (SYDFIELD.cfld_name = sydflfld.cfld_name AND sydflfld.cFile_nam = '" + tableName + "') WHERE SYDFIELD.cfld_name IN (SELECT cfld_name FROM sydflfld WHERE cFile_nam = '" + tableName + "') ORDER BY nfld_pos", new AriaDbConnection("Aria", ""), Aria.Environment.AriaDatabaseTypes.Aria27SystemFiles, clientId);
            DataTable table = command.GetDataTable();

            for (int index = 0; index < table.Rows.Count; index++)
            {
                DataRow row = table.Rows[index];

                AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();
                AriaObject ariaObject = dictionary.LoadAriaObjectByName(new Aria.Data.AriaDbConnection("", ""), objectName, clientId);

                string xmlString = "";

                AriaFieldSettings fieldSettings = new AriaFieldSettings();
                fieldSettings.Code = (string)row["mcodeinfo"];
                if ((string)row["cdata_typ"] == "C") fieldSettings.DataType = AriaStandardDataTypes.String;
                if ((string)row["cdata_typ"] == "N") fieldSettings.DataType = AriaStandardDataTypes.Numeric;
                if ((string)row["cdata_typ"] == "D") fieldSettings.DataType = AriaStandardDataTypes.Date;
                if ((string)row["cdata_typ"] == "L") fieldSettings.DataType = AriaStandardDataTypes.Logical;
                if ((string)row["cdata_typ"] == "M") fieldSettings.DataType = AriaStandardDataTypes.Memo;
                if ((string)row["cdata_typ"] == "G") fieldSettings.DataType = AriaStandardDataTypes.Binary;

                fieldSettings.DecimalPlaces = (int)(Decimal)row["nfld_dec"];
                fieldSettings.FieldName = (string)row["cfld_name"];
                fieldSettings.HasReleatedField = (bool)row["lrltfields"];
                fieldSettings.Head = (string)row["cfld_head"];
                fieldSettings.IsReleatedField = (bool)row["lrelated"];
                fieldSettings.Mask = (string)row["cpict_str"];
                fieldSettings.Message = (string)row["cfld_msg"];
                fieldSettings.ReleatedFields = ((string)row["mrltfields"]).Split('|');

                for(int colIndex =0 ; colIndex < indexColumns.Length; colIndex++)
                {
                    if (indexColumns[colIndex].ColumnName == fieldSettings.FieldName.TrimEnd().ToUpper())
                    {
                        fieldSettings.IsPrimaryKey = true;
                    }
                }
                

                if (((string)row["mventries"]).IndexOf("~") > 0)
                {
                    string[] description = ((string)row["mventries"]).Substring(0, ((string)row["mventries"]).IndexOf("~")).Split('|');
                    string[] values = ((string)row["mventries"]).Substring(((string)row["mventries"]).IndexOf("~") + 1).Split('|');

                    for (int validIndex = 0; validIndex < values.Length; validIndex++)
                    {
                        values[validIndex] += "|" + description[validIndex];
                    }

                    fieldSettings.ValidEntries = values;
                    fieldSettings.ValidEntry = true;
                }
                else
                {
                    fieldSettings.ValidEntry = false;
                }
                
                fieldSettings.ValidExpression = (string)row["mvald_str"];
                fieldSettings.Width = (int)(decimal)row["nfld_wdth"];

                AriaXmlSerializer xml = new AriaXmlSerializer();
                xmlString = xml.ConvertToXml(fieldSettings);

                if (removeSpecialCharacters(fieldSettings.Head).TrimEnd() != "")
                {
                    dictionary.SaveAriaObjectProperty(new AriaDbConnection("Aria", ""), ariaObject.ObjectID, ariaObject.ActiveRevision, removeSpecialCharacters(fieldSettings.Head), AriaModificationTypes.Add, AriaDataTypes.AriaField, xmlString, clientId);
                }
            }
        }
    }
}
