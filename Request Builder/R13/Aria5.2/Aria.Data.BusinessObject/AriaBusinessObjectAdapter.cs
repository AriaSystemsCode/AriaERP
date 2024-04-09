using System;
using System.Collections.Generic;
using System.Text;
using System.Data;
using Aria.DataTypes;
using Aria.Data;
using Aria.DataTypes.ObjectDictionary;
using Aria.EnterpriseServices.ObjectDictionary;
using System.Collections;
using Aria.DataTypes.Settings;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Environment;


namespace Aria.Data.BusinessObject
{
    /// <summary>
    /// Used to get information about Business object (property related object name)
    /// </summary>
    public class AriaBusinessObjectAdapter
    {
        static string styleSeg = "";
        static string colorSeg = "";
        static string sizeSeg = "";

        public ArrayList LoadAriaObjectRelatedProperties(AriaDbConnection connection, string objectName, string objectRevision, bool differenceOnly,string  clientId)
        {
            string commandText = "SELECT AriaObjectProperty.* FROM AriaObjectProperty LEFT JOIN AriaObject ON " +
                                        "(AriaObjectProperty.ObjectID = AriaObject.ObjectID) " +
                                            "WHERE SUBSTRING(ObjectName, 1, " + objectName.Length + ") = @ObjectName AND " +
                                                "ObjectName <> @ObjectName AND " +
                                                    "ObjectRevision " + (differenceOnly ? "=" : "<=") + " @ObjectRevision " +
                                                        "ORDER BY AriaObjectProperty.PropertyName, AriaObjectProperty.ObjectRevision";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));
            command.Parameters.Add(new AriaDbParameter("ObjectRevision", objectRevision));

            AriaDataProvider dataProvider = new AriaDataProvider();
            ArrayList properties = dataProvider.GetObjectList(command, typeof(AriaObjectProperty));

            ArrayList targetProperties = new ArrayList();

            for (int index = 0; index < properties.Count; index++)
            {
                AriaObjectProperty property = (AriaObjectProperty)properties[index];
                if (((AriaRelatedFieldSettings)property.PropertySettings).Message.Trim() == "InHeader")
                {
                    targetProperties.Add(properties[index]);
                }
            }

            AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

            dictionary.MergeModification(targetProperties, "PropertyName", "ModificationType");

            return targetProperties;
        }

        public void SetStyleCodeStructure(string companyId,string clientId)
        {
            if (styleSeg != "") return;

            AriaDbCommand command = new AriaDbCommand("", new AriaDbConnection("", companyId), AriaDatabaseTypes.Aria27Data, clientId);
            command.CommandText = "SELECT * FROM icistru WHERE  citemrecty = 'U' ORDER BY cisegno";

            AriaDataProvider provider = new AriaDataProvider();
            DataTable styleCodeStructure = provider.GetDataTable(command);

            int styleLength = 0, colorLength = 0, sizeLength = 0;
            string styleSep = "", colorSep = "";

            for (int index = 0; index < styleCodeStructure.Rows.Count; index++)
            {
                if (((string)styleCodeStructure.Rows[index]["cisegtype"]).TrimEnd() == "F")
                {
                    styleLength = Convert.ToInt32(styleCodeStructure.Rows[index]["nisegsize"]);
                    styleSep = ((string)styleCodeStructure.Rows[index]["cisegsepr"]).TrimEnd();
                }


                if (((string)styleCodeStructure.Rows[index]["cisegtype"]).TrimEnd() == "C")
                {
                    colorLength = Convert.ToInt32(styleCodeStructure.Rows[index]["nisegsize"]);
                    colorSep = ((string)styleCodeStructure.Rows[index]["cisegsepr"]).TrimEnd();
                }

                if (((string)styleCodeStructure.Rows[index]["cisegtype"]).TrimEnd() == "S")
                {
                    sizeLength = Convert.ToInt32(styleCodeStructure.Rows[index]["nisegsize"]);
                }
            }

            styleSeg = "";
            colorSeg = "";
            sizeSeg = "";

            styleSeg = "SUBSTR(Style, 1, " + styleLength.ToString() + ")";
            colorSeg = "SUBSTR(Style, " + Convert.ToString((styleLength + 1 + styleSep.Trim().Length)) + ", " + colorLength.ToString() + ")";
            sizeSeg = "SUBSTR(Style, " + Convert.ToString((styleLength + 1 + styleSep.Trim().Length + colorLength + colorSep.Trim().Length)) + ", " + sizeLength.ToString() + ")";

        }

        public DataTable GetSystemObject(string userId, string password, string companyId, string objectName, AriaDataObjectPointer pointer, string clientId)
        {
            AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

            AriaObjectRevision objectRevision = dictionary.LoadActiveRevision(new AriaDbConnection("Aria", ""), objectName, clientId);

            ArrayList properties = dictionary.LoadAriaObjectProperties(new AriaDbConnection("Aria", ""), objectName, "1.0", false, clientId);

            ArrayList relatedProperties = LoadAriaObjectRelatedProperties(new AriaDbConnection("Aria", ""), objectName, "1.0", false, clientId);

            string sqlCommand = "SELECT ";

            for (int index = 0; index < properties.Count; index++)
            {
                AriaObjectProperty property = (AriaObjectProperty)properties[index];
                sqlCommand += ((AriaDataObjectSettings)objectRevision.ObjectRevisionSettings).TableName.Trim() + "." + ((AriaFieldSettings)property.PropertySettings).FieldName + " AS " + property.PropertyName;
                if (index != properties.Count - 1)
                {
                    sqlCommand += ", ";
                }
            }

            for (int index = 0; index < relatedProperties.Count; index++)
            {
                AriaObjectProperty property = (AriaObjectProperty)relatedProperties[index];

                sqlCommand += ", ";
                sqlCommand += ((AriaRelatedFieldSettings)property.PropertySettings).FieldName + " AS " + property.PropertyName;
            }

            sqlCommand += " FROM " + ((AriaDataObjectSettings)objectRevision.ObjectRevisionSettings).TableName + " ";

            ArrayList relatedObjects = dictionary.LoadAriaObjectChildObjectsOfType(new AriaDbConnection("Aria", ""), objectName, AriaObjectTypes.RelatedData, clientId);

            for (int index = 0; index < relatedObjects.Count; index++)
            {
                AriaObject ariaRelatedObject = (AriaObject)relatedObjects[index];

                AriaObjectRevision relatedRevision = dictionary.LoadActiveRevision(new AriaDbConnection("Aria", ""), ariaRelatedObject.ObjectName, clientId);
                AriaRelatedDataObjectSettings relatedObjectSettings = (AriaRelatedDataObjectSettings)relatedRevision.ObjectRevisionSettings;

                AriaDataObjectSettings relatedObjectObjectSettings = (AriaDataObjectSettings)dictionary.LoadActiveRevision(new AriaDbConnection("Aria", ""), relatedObjectSettings.DataObjectName, clientId).ObjectRevisionSettings;

                sqlCommand += " LEFT JOIN " + relatedObjectObjectSettings.TableName + " " + ariaRelatedObject.ObjectName.Replace(objectName + ".", "") + " ON (";

                sqlCommand += relatedObjectSettings.Filter;
                sqlCommand += ")";
            }

            sqlCommand += " WHERE " + ((AriaDataObjectSettings)objectRevision.ObjectRevisionSettings).Filter;

            AriaDbCommand command = new AriaDbCommand(sqlCommand, new AriaDbConnection("Aria", companyId), Aria.Environment.AriaDatabaseTypes.Aria27SystemFiles, clientId);

            for (int index = 0; index < pointer.KeyFields.Count; index++)
            {
                command.Parameters.Add(((AriaDataObjectPointerKeyField)pointer.KeyFields[index]).FieldName,
                                        ((AriaDataObjectPointerKeyField)pointer.KeyFields[index]).Value);
            }

            DataTable table = command.GetDataTable();

            return table;
        }

        public Hashtable GetBusinessObjectFieldsMap(string userId, string password, string objectName,string clientId)
        {
            Hashtable result = new Hashtable();

            AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

            ArrayList properties = dictionary.LoadAriaObjectProperties(new AriaDbConnection("Aria", ""), objectName, "1.0", false, clientId);

            for (int index = 0; index < properties.Count; index++)
            {
                AriaObjectProperty property = (AriaObjectProperty)properties[index];

                result[property.PropertyName.Trim().ToLower()] = ((AriaFieldSettings)property.PropertySettings).FieldName.Trim().ToLower();
            }

            return result;
        }

        public ArrayList GetBusinessObjectSchema(string userId, string password, string objectName,string clientId)
        {
            AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

            ArrayList result = dictionary.LoadAriaObjectProperties(new AriaDbConnection("Aria", ""), objectName, "1.0", false, clientId);
            result.AddRange(LoadAriaObjectRelatedProperties(new AriaDbConnection("Aria", ""), objectName, "1.0", false, clientId));
            
            return result;
        }

        private static Dictionary<string, string> _newBusinessObject = new Dictionary<string, string>();
        
        public DataTable GetNewBusinessObject(string userId, string password, string companyId, string objectName,string  clientId)
        {
            string sqlCommand = "";

            if (_newBusinessObject.ContainsKey(objectName))
            {
                sqlCommand = _newBusinessObject[objectName];
            }
            else
            {

                AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

                AriaObjectRevision objectRevision = dictionary.LoadActiveRevision(new AriaDbConnection("Aria", ""), objectName, clientId);

                ArrayList properties = dictionary.LoadAriaObjectProperties(new AriaDbConnection("Aria", ""), objectName, "1.0", false, clientId);

                ArrayList relatedProperties = LoadAriaObjectRelatedProperties(new AriaDbConnection("Aria", ""), objectName, "1.0", false, clientId);

                sqlCommand = "SELECT ";

                for (int index = 0; index < properties.Count; index++)
                {
                    AriaObjectProperty property = (AriaObjectProperty)properties[index];
                    sqlCommand += ((AriaDataObjectSettings)objectRevision.ObjectRevisionSettings).TableName.Trim() + "." + ((AriaFieldSettings)property.PropertySettings).FieldName + " AS " + property.PropertyName;
                    if (index != properties.Count - 1)
                    {
                        sqlCommand += ", ";
                    }
                }

                for (int index = 0; index < relatedProperties.Count; index++)
                {
                    AriaObjectProperty property = (AriaObjectProperty)relatedProperties[index];

                    sqlCommand += ", ";
                    sqlCommand += ((AriaRelatedFieldSettings)property.PropertySettings).FieldName + " AS " + property.PropertyName;
                }

                sqlCommand += " FROM " + ((AriaDataObjectSettings)objectRevision.ObjectRevisionSettings).TableName + " ";

                ArrayList relatedObjects = dictionary.LoadAriaObjectChildObjectsOfType(new AriaDbConnection("Aria", ""), objectName, AriaObjectTypes.RelatedData, clientId);

                for (int index = 0; index < relatedObjects.Count; index++)
                {
                    AriaObject ariaRelatedObject = (AriaObject)relatedObjects[index];

                    AriaObjectRevision relatedRevision = dictionary.LoadActiveRevision(new AriaDbConnection("Aria", ""), ariaRelatedObject.ObjectName, clientId);
                    AriaRelatedDataObjectSettings relatedObjectSettings = (AriaRelatedDataObjectSettings)relatedRevision.ObjectRevisionSettings;

                    AriaDataObjectSettings relatedObjectObjectSettings = (AriaDataObjectSettings)dictionary.LoadActiveRevision(new AriaDbConnection("Aria", ""), relatedObjectSettings.DataObjectName, clientId).ObjectRevisionSettings;

                    sqlCommand += " LEFT JOIN " + relatedObjectObjectSettings.TableName + " " + ariaRelatedObject.ObjectName.Replace(objectName + ".", "") + " ON (";

                    sqlCommand += relatedObjectSettings.Filter;
                    sqlCommand += ")";
                }

                sqlCommand += " WHERE .F.";

                _newBusinessObject.Add(objectName, sqlCommand);
            }

            AriaDbCommand command = new AriaDbCommand(sqlCommand, new AriaDbConnection("Aria", companyId), Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);
            DataTable table = command.GetDataTable();

            return table;
        }

        private static Dictionary<string, string> _BusinessObject = new Dictionary<string, string>();

        public DataTable GetBusinessObject(string userId, string password, string companyId, string objectName, AriaDataObjectPointer pointer ,string  clientId)
        {
            string sqlCommand = "";

            if (_BusinessObject.ContainsKey(objectName))
            {
                sqlCommand = _BusinessObject[objectName];
            }
            else
            {
                AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

                AriaObjectRevision objectRevision = dictionary.LoadActiveRevision(new AriaDbConnection("Aria", ""), objectName, clientId);

                ArrayList properties = dictionary.LoadAriaObjectProperties(new AriaDbConnection("Aria", ""), objectName, "1.0", false, clientId);

                ArrayList relatedProperties = LoadAriaObjectRelatedProperties(new AriaDbConnection("Aria", ""), objectName, "1.0", false, clientId);

                sqlCommand = "SELECT ";

                for (int index = 0; index < properties.Count; index++)
                {
                    AriaObjectProperty property = (AriaObjectProperty)properties[index];
                    sqlCommand += ((AriaDataObjectSettings)objectRevision.ObjectRevisionSettings).TableName.Trim() + "." + ((AriaFieldSettings)property.PropertySettings).FieldName + " AS " + property.PropertyName;
                    if (index != properties.Count - 1)
                    {
                        sqlCommand += ", ";
                    }
                }

                for (int index = 0; index < relatedProperties.Count; index++)
                {
                    AriaObjectProperty property = (AriaObjectProperty)relatedProperties[index];

                    sqlCommand += ", ";
                    sqlCommand += ((AriaRelatedFieldSettings)property.PropertySettings).FieldName + " AS " + property.PropertyName;
                }

                sqlCommand += " FROM " + ((AriaDataObjectSettings)objectRevision.ObjectRevisionSettings).TableName + " ";

                // T20110803.0001 MAH 8/2/2011
                if (objectName.StartsWith("Aria4XP"))
                {
                    sqlCommand+= " AS [" + objectName.Trim() + "] ";
                }
                else
                {
                    // T20110803.0001 MAH 8/2/2011 End
                    ArrayList relatedObjects = dictionary.LoadAriaObjectChildObjectsOfType(new AriaDbConnection("Aria", ""), objectName, AriaObjectTypes.RelatedData, clientId);

                    for (int index = 0; index < relatedObjects.Count; index++)
                    {
                        AriaObject ariaRelatedObject = (AriaObject)relatedObjects[index];

                        AriaObjectRevision relatedRevision = dictionary.LoadActiveRevision(new AriaDbConnection("Aria", ""), ariaRelatedObject.ObjectName, clientId);
                        AriaRelatedDataObjectSettings relatedObjectSettings = (AriaRelatedDataObjectSettings)relatedRevision.ObjectRevisionSettings;

                        AriaDataObjectSettings relatedObjectObjectSettings = (AriaDataObjectSettings)dictionary.LoadActiveRevision(new AriaDbConnection("Aria", ""), relatedObjectSettings.DataObjectName, clientId).ObjectRevisionSettings;

                        sqlCommand += " LEFT JOIN " + relatedObjectObjectSettings.TableName + " " + ariaRelatedObject.ObjectName.Replace(objectName + ".", "") + " ON (";

                        sqlCommand += relatedObjectSettings.Filter;
                        sqlCommand += ")";
                    }

                    sqlCommand += " WHERE " + ((AriaDataObjectSettings)objectRevision.ObjectRevisionSettings).Filter;
                    // T20110803.0001 MAH 8/2/2011
                }
                // T20110803.0001 MAH 8/2/2011 End
                _BusinessObject.Add(objectName, sqlCommand);
            }

            AriaDbCommand command = new AriaDbCommand(sqlCommand, new AriaDbConnection("Aria", companyId), Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);

            for(int index =0; index < pointer.KeyFields.Count; index ++)
            {
                command.Parameters.Add(((AriaDataObjectPointerKeyField)pointer.KeyFields[index]).FieldName,
                                        ((AriaDataObjectPointerKeyField)pointer.KeyFields[index]).Value);
            }

            DataTable table = command.GetDataTable();

            if (objectName == "CRM.Style")
            {
                table.Columns.Add("OTS1", typeof(int));
                table.Columns.Add("OTS2", typeof(int));
                table.Columns.Add("OTS3", typeof(int));
                table.Columns.Add("OTS4", typeof(int));
                table.Columns.Add("OTS5", typeof(int));
                table.Columns.Add("OTS6", typeof(int));
                table.Columns.Add("OTS7", typeof(int));
                table.Columns.Add("OTS8", typeof(int));
                if (table.Rows.Count > 0)
                {
                    string sql1 = "Select stydye.stk1 as OTS1, " +
                                        "stydye.stk2 as OTS2, " +
                                        "stydye.stk3 as OTS3, " +
                                        "stydye.stk4 as OTS4, " +
                                        "stydye.stk5 as OTS5, " +
                                        "stydye.stk6 as OTS6, " +
                                        "stydye.stk7 as OTS7, " +
                                        "stydye.stk8 as OTS8" +
                                        " from stydye where " +
                                        "style+cwarecode+dyelot = '" + ((AriaDataObjectPointerKeyField)pointer.KeyFields[0]).Value + "' " +
                                        "and stydye.CWARECODE = 'NC' ";
                    AriaDbCommand otsCommand1 = new AriaDbCommand(sql1,
                                                                new AriaDbConnection("Aria", companyId), Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);
                    DataTable otsDataTable1 = otsCommand1.GetDataTable();

                    string sql2 = "Select sum(ordline.qty1) as OTS1, " +
                                        "sum(ordline.qty2) as OTS2, " +
                                        "sum(ordline.qty3) as OTS3, " +
                                        "sum(ordline.qty4) as OTS4, " +
                                        "sum(ordline.qty5) as OTS5, " +
                                        "sum(ordline.qty6) as OTS6, " +
                                        "sum(ordline.qty7) as OTS7, " +
                                        "sum(ordline.qty8) as OTS8 " +
                                        "from ordline, ordHdr where ordline.style+DTOS(ordline.complete)+ordline.cordtype+ordline.order+ordline.store+STR(ordline.lineno,6) = '" +
                                        ((AriaDataObjectPointerKeyField)pointer.KeyFields[0]).Value + "' " +
                                        "and ordHdr.status <> 'C' and ordHdr.Status <> 'X' and (ordline.cordtype + ordline.order = ordHdr.cordtype + ordHdr.order) and ordline.CWARECODE = 'NC'";


                    AriaDbCommand otsCommand2 = new AriaDbCommand(sql2,
                                                                new AriaDbConnection("Aria", companyId), Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);

                    DataTable otsDataTable2 = otsCommand2.GetDataTable();

                    if (otsDataTable1.Rows.Count > 0 && otsDataTable2.Rows.Count == 0)
                    {
                        table.Rows[0]["OTS1"] = otsDataTable1.Rows[0]["OTS1"];
                        table.Rows[0]["OTS2"] = otsDataTable1.Rows[0]["OTS2"];
                        table.Rows[0]["OTS3"] = otsDataTable1.Rows[0]["OTS3"];
                        table.Rows[0]["OTS4"] = otsDataTable1.Rows[0]["OTS4"];
                        table.Rows[0]["OTS5"] = otsDataTable1.Rows[0]["OTS5"];
                        table.Rows[0]["OTS6"] = otsDataTable1.Rows[0]["OTS6"];
                        table.Rows[0]["OTS7"] = otsDataTable1.Rows[0]["OTS7"];
                        table.Rows[0]["OTS8"] = otsDataTable1.Rows[0]["OTS8"];
                    }

                    if (otsDataTable1.Rows.Count > 0 && otsDataTable2.Rows.Count > 0)
                    {
                        table.Rows[0]["OTS1"] = Convert.ToInt32(otsDataTable1.Rows[0]["OTS1"]) - Convert.ToInt32(otsDataTable2.Rows[0]["OTS1"]);
                        table.Rows[0]["OTS2"] = Convert.ToInt32(otsDataTable1.Rows[0]["OTS2"]) - Convert.ToInt32(otsDataTable2.Rows[0]["OTS2"]);
                        table.Rows[0]["OTS3"] = Convert.ToInt32(otsDataTable1.Rows[0]["OTS3"]) - Convert.ToInt32(otsDataTable2.Rows[0]["OTS3"]);
                        table.Rows[0]["OTS4"] = Convert.ToInt32(otsDataTable1.Rows[0]["OTS4"]) - Convert.ToInt32(otsDataTable2.Rows[0]["OTS4"]);
                        table.Rows[0]["OTS5"] = Convert.ToInt32(otsDataTable1.Rows[0]["OTS5"]) - Convert.ToInt32(otsDataTable2.Rows[0]["OTS5"]);
                        table.Rows[0]["OTS6"] = Convert.ToInt32(otsDataTable1.Rows[0]["OTS6"]) - Convert.ToInt32(otsDataTable2.Rows[0]["OTS6"]);
                        table.Rows[0]["OTS7"] = Convert.ToInt32(otsDataTable1.Rows[0]["OTS7"]) - Convert.ToInt32(otsDataTable2.Rows[0]["OTS7"]);
                        table.Rows[0]["OTS8"] = Convert.ToInt32(otsDataTable1.Rows[0]["OTS8"]) - Convert.ToInt32(otsDataTable2.Rows[0]["OTS8"]);
                    }
                }

                table.Columns.Add("PriceLevel", typeof(string));
                table.Columns.Add("Price", typeof(double));


                if (table.Rows.Count > 0 && pointer.KeyFields.Count > 1)
                {
                    string customerAccount = "";
                    int customerQty = 0;

                    for(int index =0; index < pointer.KeyFields.Count; index ++)
                    {
                        if(((AriaDataObjectPointerKeyField)pointer.KeyFields[index]).FieldName == "Account")
                        {
                            customerAccount = ((AriaDataObjectPointerKeyField)pointer.KeyFields[index]).Value.ToString();
                        }
                        if(((AriaDataObjectPointerKeyField)pointer.KeyFields[index]).FieldName == "Qty")
                        {
                            customerQty = Convert.ToInt32(((AriaDataObjectPointerKeyField)pointer.KeyFields[index]).Value);
                        }
                    }


                    AriaDataObjectPointer customerPointer = new AriaDataObjectPointer();
                    customerPointer.AddKeyField("Account", customerAccount);
                    DataTable customerTable = GetBusinessObject(userId, password, companyId, "CRM.Customer", customerPointer, clientId);

                    if (customerTable.Rows.Count > 0)
                        table.Rows[0]["PriceLevel"] = "Price" + customerTable.Rows[0]["PriceLevel"].ToString().Trim().ToUpper();
                    else
                    {
                        table.Rows[0]["PriceLevel"]= "PriceA";
                    }

                    if (table.Rows[0]["PriceLevel"].ToString() == "PriceA")
                    {
                        table.Rows[0]["Price"] = table.Rows[0]["PriceA"];
                    }

                    if (table.Rows[0]["PriceLevel"].ToString() == "PriceB")
                    {
                        table.Rows[0]["Price"] = table.Rows[0]["PriceB"];
                    }

                    if (table.Rows[0]["PriceLevel"].ToString() == "PriceC")
                    {
                        table.Rows[0]["Price"] = table.Rows[0]["PriceC"];
                    }

                    if (table.Rows[0]["PriceLevel"].ToString() == "PriceQ")
                    {
                        int qty = customerQty;

                        if (qty < Convert.ToInt32(table.Rows[0]["AtquantityforpriceB"]))
                        {
                            table.Rows[0]["Price"] = table.Rows[0]["PriceA"];
                        }

                        if (qty >= Convert.ToInt32(table.Rows[0]["AtquantityforpriceB"]) && qty < Convert.ToInt32(table.Rows[0]["AtquantityforpriceC"]))
                        {
                            table.Rows[0]["Price"] = table.Rows[0]["PriceB"];
                        }

                        if (qty >= Convert.ToInt32(table.Rows[0]["AtquantityforpriceC"]))
                        {
                            table.Rows[0]["Price"] = table.Rows[0]["Pricec"];
                        }
                    }
                }
            }

            return table;
        }

        public DataTable GetBusinessObjects(string userId, string password, string companyId, string objectName, string filter, string sort,string  clientId)
        {
            AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

            AriaObjectRevision objectRevision = dictionary.LoadActiveRevision(new AriaDbConnection("Aria", ""), objectName, clientId);

            ArrayList properties = dictionary.LoadAriaObjectProperties(new AriaDbConnection("Aria", ""), objectName, "1.0", false, clientId);

            ArrayList relatedProperties = LoadAriaObjectRelatedProperties(new AriaDbConnection("Aria", ""), objectName, "1.0", false, clientId);

            string sqlCommand = "SELECT ";

            for (int index = 0; index < properties.Count; index++)
            {
                AriaObjectProperty property = (AriaObjectProperty)properties[index];
                sqlCommand += ((AriaDataObjectSettings)objectRevision.ObjectRevisionSettings).TableName.Trim() + "." + ((AriaFieldSettings)property.PropertySettings).FieldName + " AS " + property.PropertyName;
                if (index != properties.Count - 1)
                {
                    sqlCommand += ", ";
                }
            }

            for (int index = 0; index < relatedProperties.Count; index++)
            {
                AriaObjectProperty property = (AriaObjectProperty)relatedProperties[index];

                sqlCommand += ", ";
                sqlCommand += ((AriaRelatedFieldSettings)property.PropertySettings).FieldName + " AS " + property.PropertyName;
            }

            sqlCommand += " FROM " + ((AriaDataObjectSettings)objectRevision.ObjectRevisionSettings).TableName + " ";

            ArrayList relatedObjects = dictionary.LoadAriaObjectChildObjectsOfType(new AriaDbConnection("Aria", ""), objectName, AriaObjectTypes.RelatedData, clientId);

            for (int index = 0; index < relatedObjects.Count; index++)
            {
                AriaObject ariaRelatedObject = (AriaObject)relatedObjects[index];

                AriaObjectRevision relatedRevision = dictionary.LoadActiveRevision(new AriaDbConnection("Aria", ""), ariaRelatedObject.ObjectName, clientId);
                AriaRelatedDataObjectSettings relatedObjectSettings = (AriaRelatedDataObjectSettings)relatedRevision.ObjectRevisionSettings;

                AriaDataObjectSettings relatedObjectObjectSettings = (AriaDataObjectSettings)dictionary.LoadActiveRevision(new AriaDbConnection("Aria", ""), relatedObjectSettings.DataObjectName, clientId).ObjectRevisionSettings;

                sqlCommand += " LEFT JOIN " + relatedObjectObjectSettings.TableName + " " + ariaRelatedObject.ObjectName.Replace(objectName + ".", "") + " ON (";

                sqlCommand += relatedObjectSettings.Filter;
                sqlCommand += ")";
            }

            if (filter.ToString().Trim().Length > 0)
            {
                sqlCommand += " WHERE " + filter;
            }

            if (sort.ToString().Trim().Length > 0)
            {
                sqlCommand += " ORDER BY " + sort;
            }

            AriaDbCommand command = new AriaDbCommand(sqlCommand, new AriaDbConnection("Aria", companyId), Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);

            DataTable table = command.GetDataTable();

            return table;
        }

        public DataTable GetBusinessObjects(string userId, string password, string companyId, string objectName, AriaConditionList filter,string  clientId)
        {
            if (objectName == "CRM.SalesRep")
            {
                AriaDbConnection connection = new AriaDbConnection("Aria", companyId);
                string sql = "SELECT REPCODE as SalesRepCode, Name FROM SalesRep WHERE ";

                switch (((AriaStandardDataType)((AriaCondition)filter.Items[0]).LeftHandSide).PropertyDataPathDictionary["Value"].ToString())
                {
                    case "SalesRepCode":
                        sql += "REPCODE LIKE '@Param1@%' order by REPCODE";
                        break;
                }

                AriaDbCommand command = new AriaDbCommand(sql, connection, Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);

                if (((AriaStandardDataType)((AriaCondition)filter.Items[0]).RightHandSide).Value.ToString().Trim() == "*")
                {
                    command.Parameters.Add(new AriaDbParameter("Param1", ""));
                }
                else
                {
                    command.Parameters.Add(new AriaDbParameter("Param1", ((AriaStandardDataType)((AriaCondition)filter.Items[0]).RightHandSide).Value));
                }

                AriaDataProvider dataProvider = new AriaDataProvider();

                return dataProvider.GetDataTable(command);
            }

            if (objectName == "CRM.ArtWork")
            {
                AriaDbConnection connection = new AriaDbConnection("Aria", companyId);
                string sql = "SELECT CDESIGNID as ArtworkDesignCode, CDSGNNAME AS DesignName  FROM ARTWRKDS WHERE ";

                switch (((AriaStandardDataType)((AriaCondition)filter.Items[0]).LeftHandSide).PropertyDataPathDictionary["Value"].ToString())
                {
                    case "ArtworkDesignCode":
                        sql += "CDESIGNID LIKE '@Param1@%' order by CDESIGNID";
                        break;
                }

                AriaDbCommand command = new AriaDbCommand(sql, connection, Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);

                if (((AriaStandardDataType)((AriaCondition)filter.Items[0]).RightHandSide).Value.ToString().Trim() == "*")
                {
                    command.Parameters.Add(new AriaDbParameter("Param1", ""));
                }
                else
                {
                    command.Parameters.Add(new AriaDbParameter("Param1", ((AriaStandardDataType)((AriaCondition)filter.Items[0]).RightHandSide).Value));
                }

                AriaDataProvider dataProvider = new AriaDataProvider();

                return dataProvider.GetDataTable(command);
            }

            
            if (objectName == "CRM.Contact")
            {
                AriaDbConnection connection = new AriaDbConnection("Aria", companyId);
                string sql = "SELECT Contact, ccontttl As Title, Phone As Phone FROM Contact WHERE ";

                switch (((AriaStandardDataType)((AriaCondition)filter.Items[0]).LeftHandSide).PropertyDataPathDictionary["Value"].ToString())
                {
                    case "Customer":
                        sql += "CCONTTYPE+CCONT_ID+STORE+UPPER(CONTACT) LIKE 'C@Param1@%' order by Contact";
                        break;
                }

                AriaDbCommand command = new AriaDbCommand(sql, connection, Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);

                if (((AriaStandardDataType)((AriaCondition)filter.Items[0]).RightHandSide).Value.ToString().Trim() == "*")
                {
                    command.Parameters.Add(new AriaDbParameter("Param1", ""));
                }
                else
                {
                    command.Parameters.Add(new AriaDbParameter("Param1", ((AriaStandardDataType)((AriaCondition)filter.Items[0]).RightHandSide).Value));
                }

                AriaDataProvider dataProvider = new AriaDataProvider();

                return dataProvider.GetDataTable(command);
            }

            SetStyleCodeStructure(companyId, clientId);

            if (objectName.Contains("CRM.Style"))
            {
                if (GetSetup(userId, password, companyId, "IC", "M_USEEXSSC", clientId).Rows[0]["DefaultData"].ToString().TrimEnd() == ".T.")
                {
                }
                else
                {
                    styleSeg = "SUBSTR(Style, 1, 12)";
                    colorSeg = "SUBSTR(Style, 14, 6)";
                    sizeSeg = "";
                }
            }

            if (objectName == "CRM.Style")
            {
                AriaDbConnection connection = new AriaDbConnection("Aria", companyId);
                string sql = "SELECT DISTINCT cstyMajor As Style, Desc1 as Description FROM STYLE WHERE status !='X' ";

                switch (((AriaStandardDataType)((AriaCondition)filter.Items[0]).LeftHandSide).PropertyDataPathDictionary["Value"].ToString())
                {
                    case "Style":
                        if (GetSetup(userId, password, companyId, "IC", "M_USEEXSSC", clientId).Rows[0]["DefaultData"].ToString().TrimEnd() == ".T.")
                        {
                            sql = "SELECT DISTINCT cstyMajor As Style, Desc as Description FROM STYLE WHERE status !='X' ";
                        }
                        sql += " and Style LIKE '@Param1@%' order by Style";
                        break;

                    case "Description":
                        sql += " and UPPER(Desc1) LIKE '%@Param1@%' ";
                        break;
                }

                AriaDbCommand command = new AriaDbCommand(sql, connection, Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);

                if (((AriaStandardDataType)((AriaCondition)filter.Items[0]).RightHandSide).Value.ToString().Trim() == "*")
                {
                    command.Parameters.Add(new AriaDbParameter("Param1", ""));
                }
                else
                {
                    command.Parameters.Add(new AriaDbParameter("Param1", ((AriaStandardDataType)((AriaCondition)filter.Items[0]).RightHandSide).Value));
                }

                AriaDataProvider dataProvider = new AriaDataProvider();

                return dataProvider.GetDataTable(command);

            }

            if (objectName == "CRM.Style.Color")
            {
                AriaDbConnection connection = new AriaDbConnection("Aria", companyId);
                string sql = "SELECT DISTINCT " + colorSeg + "as Color, " + colorSeg + " as Description FROM STYLE WHERE status !='X' ";

                switch (((AriaStandardDataType)((AriaCondition)filter.Items[0]).LeftHandSide).PropertyDataPathDictionary["Value"].ToString())
                {
                    case "Style":
                        sql += " and Style LIKE '@Param1@%' ";
                        break;
                }

                AriaDbCommand command = new AriaDbCommand(sql, connection, Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);

                command.Parameters.Add(new AriaDbParameter("Param1", ((AriaStandardDataType)((AriaCondition)filter.Items[0]).RightHandSide).Value));

                AriaDataProvider dataProvider = new AriaDataProvider();

                return dataProvider.GetDataTable(command);
            }

            if (objectName == "CRM.Style.Color.Scale")
            {
                AriaDbConnection connection = new AriaDbConnection("Aria", companyId);
                string sql = "SELECT DISTINCT " + sizeSeg + " as Scale, " + sizeSeg + " as Description FROM STYLE WHERE status !='X' ";

                switch (((AriaStandardDataType)((AriaCondition)filter.Items[0]).LeftHandSide).PropertyDataPathDictionary["Value"].ToString())
                {
                    case "Style":
                        sql += "and Style LIKE '@Param1@%'";
                        break;
                }

                AriaDbCommand command = new AriaDbCommand(sql, connection, Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);

                command.Parameters.Add(new AriaDbParameter("Param1", ((AriaStandardDataType)((AriaCondition)filter.Items[0]).RightHandSide).Value));

                AriaDataProvider dataProvider = new AriaDataProvider();

                return dataProvider.GetDataTable(command);
            }
            
            if (objectName == "CRM.Customer")
            {
                AriaDbConnection connection = new AriaDbConnection("Aria", companyId);
                string sql = "SELECT Account, Store, btName as Name, cAddress3 as City, cAddress5 as ZipCode, Phone1 as Phone FROM CUSTOMER WHERE ";

                switch (((AriaStandardDataType)((AriaCondition)filter.Items[0]).LeftHandSide).PropertyDataPathDictionary["Value"].ToString())
                {
                    case "Account":
                        sql += "UPPER(Account) LIKE '@Param1@%' AND Type = 'M' ORDER BY Account";
                        break;

                    case "Store":
                        sql += "UPPER(Store) LIKE '@Param1@%' AND Type = 'M' order by Store";
                        break;

                    case "Name":
                        sql += "UPPER(btName) LIKE '@Param1@%' AND Type = 'M' order by btName";
                        break;

                    case "City":
                        sql += "UPPER(cAddress3) LIKE '@Param1@%' AND Type = 'M' order by cAddress3";
                        break;

                    case "ZipCode":
                        sql += "UPPER(cAddress5) LIKE '@Param1@%' AND Type = 'M' order by cAddress5";
                        break;

                    case "Phone":
                        sql += "UPPER(Phone1) LIKE '@Param1@%' AND Type = 'M' order by Phone1";
                        break;
                }

                AriaDbCommand command = new AriaDbCommand(sql, connection, Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);
                
                if (((AriaStandardDataType)((AriaCondition)filter.Items[0]).RightHandSide).Value.ToString().Trim() == "*")
                {
                    command.Parameters.Add(new AriaDbParameter("Param1", ""));
                }
                else
                {
                    command.Parameters.Add(new AriaDbParameter("Param1", ((AriaStandardDataType)((AriaCondition)filter.Items[0]).RightHandSide).Value));
                }

                AriaDataProvider dataProvider = new AriaDataProvider();

                DataTable result = dataProvider.GetDataTable(command);

                for(int index = 0; index < result.Rows.Count; index ++)
                {
                    command.CommandText = "SELECT COUNT(*) AS Count FROM CUSTOMER WHERE TYPE+ACCOUNT+STORE LIKE 'S" + result.Rows[index]["Account"].ToString().TrimEnd() + "%'";
                    if (Convert.ToInt32(dataProvider.GetDataTable(command).Rows[0]["Count"]) > 0)
                    {
                        result.Rows[index]["Store"] = "***";
                    }
                }

                return result;
            }


            if (objectName == "CRM.Customer.Store")
            {
                AriaDbConnection connection = new AriaDbConnection("Aria", companyId);
                string sql = "SELECT Account, Store, btName as Name, cAddress3 as City, cAddress5 as ZipCode, Phone1 as Phone FROM CUSTOMER WHERE ";

                switch (((AriaStandardDataType)((AriaCondition)filter.Items[0]).LeftHandSide).PropertyDataPathDictionary["Value"].ToString())
                {
                    case "Account":
                        sql = "SELECT Store, btName as Name FROM CUSTOMER WHERE ";
                        sql += "UPPER(Account) = '@Param2@' AND UPPER(Store) LIKE '@Param1@%' .AND. Type = 'S' order by Store";
                        break;

                    case "Store":
                        sql += "UPPER(Account) = '@Param2@' AND UPPER(Store) LIKE '@Param1@%' .AND. Type = 'S' order by Store";
                        break;

                    case "Name":
                        sql += "UPPER(Account) = '@Param2@' AND UPPER(btName) LIKE '@Param1@%' .AND. Type = 'S' order by btName";
                        break;

                    case "City":
                        sql += "UPPER(Account) = '@Param2@' AND UPPER(cAddress3) LIKE '@Param1@%' .AND. Type = 'S' order by cAddress3";
                        break;

                    case "ZipCode":
                        sql += "UPPER(Account) = '@Param2@' AND UPPER(cAddress5) LIKE '@Param1@%' .AND. Type = 'S' order by cAddress5";
                        break;

                    case "Phone":
                        sql += "UPPER(Account) = '@Param2@' AND UPPER(Phone1) LIKE '@Param1@%' .AND. Type = 'S' order by Phone1";
                        break;
                }

                AriaDbCommand command = new AriaDbCommand(sql, connection, Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);

                if (((AriaStandardDataType)((AriaCondition)filter.Items[0]).RightHandSide).Value.ToString().Trim() == "*")
                {
                    command.Parameters.Add(new AriaDbParameter("Param1", ""));
                    command.Parameters.Add(new AriaDbParameter("Param2", ((AriaStandardDataType)((AriaCondition)filter.Items[1]).RightHandSide).Value));
                }
                else
                {
                    command.Parameters.Add(new AriaDbParameter("Param1", ((AriaStandardDataType)((AriaCondition)filter.Items[0]).RightHandSide).Value));
                    command.Parameters.Add(new AriaDbParameter("Param2", ((AriaStandardDataType)((AriaCondition)filter.Items[1]).RightHandSide).Value));
                }

                AriaDataProvider dataProvider = new AriaDataProvider();

                DataTable result = dataProvider.GetDataTable(command);

                return result;
            }

            return null;
        }


        public string GetDefaultCode(string userId, string password, string companyId, string code,string clientId)
        {
            string sqlCommand = "SELECT CCODE_NO AS Code FROM Codes WHERE cDefCode + CRLTFIELD + CFLD_NAME = 'DN" + code + "'";

            AriaDbCommand command = new AriaDbCommand(sqlCommand, new AriaDbConnection("Aria", companyId), Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);
            DataTable table = command.GetDataTable();
            if (table.Rows.Count > 0)
            {
                return table.Rows[0]["Code"].ToString();
            }
            else
            {
                return "";
            }
        }

        public DataTable GetCode(string userId, string password, string companyId, string code,string  clientId)
        {
            string sqlCommand = "SELECT CDISCREP as Description, CCODE_NO AS Code FROM Codes WHERE cDefCode + CRLTFIELD + CFLD_NAME = 'NN" + code + "' order by CDISCREP";

            AriaDbCommand command = new AriaDbCommand(sqlCommand, new AriaDbConnection("Aria", companyId), Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);

            DataTable result = command.GetDataTable();

            DataRow row = result.NewRow();

            row["Description"] = "N / A";
            row["Code"] = "";

            result.Rows.InsertAt(row, 0);

            return result;
        }

        public string GetCodeDescription(string userId, string password, string companyId, string code, string codeID,string clientId)
        {
            string sqlCommand = "SELECT CDISCREP as Description, CCODE_NO AS Code FROM Codes WHERE CDEFCODE+CCODE_NO+CRLTFIELD+CFLD_NAME = 'N" + codeID.PadRight(6) + 'N' + code.PadRight(10) + "'";

            AriaDbCommand command = new AriaDbCommand(sqlCommand, new AriaDbConnection("Aria", companyId), Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);
            DataTable result = command.GetDataTable();

            if (result.Rows.Count == 0)
            {
                return "";
            }
            else
            {
                return result.Rows[0]["Description"].ToString();
            }
        }


        public DataTable GetProfileCodes(string userId, string password, string companyId, string profileScreen, string profileType,string  clientId)
        {
            string sqlCommand = "SELECT CCODE_NO as Code, CDISCREP as Description FROM codes WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = 'NNCPRO_CODE ' AND " +
                                " CCODE_NO IN (SELECT CCODE_NO FROM codes WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = 'NYCPRO_CODE ' AND " +
                                "crltd_nam = 'CPRO_SCR  ' AND crltd_vlu = '" + profileScreen.Trim() + "') AND " +
                                " CCODE_NO IN (SELECT CCODE_NO FROM codes WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = 'NYCPRO_CODE ' AND " +
                                "crltd_nam = 'CPRO_TYPE ' AND crltd_vlu = '" + profileType.Trim() + "') ORDER BY CCODE_NO";

            AriaDbCommand command = new AriaDbCommand(sqlCommand, new AriaDbConnection("Aria", companyId), Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);

            DataTable result = command.GetDataTable();

            return result;
        }

        public DataTable GetProfileList(string userId, string password, string companyId, string profile,string  clientId)
        {
            string sqlCommand = "SELECT cpro_value as Value FROM proflist WHERE CPRO_CODE+CPRO_VALUE = '" + profile + "' ORDER BY CPRO_CODE, CPRO_VALUE";

            AriaDbCommand command = new AriaDbCommand(sqlCommand, new AriaDbConnection("Aria", companyId), Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);

            DataTable result = command.GetDataTable();

            DataRow row = result.NewRow();

            row["Value"] = "N / A";

            result.Rows.InsertAt(row, 0);

            return result;
        }

        public DataTable GetProfileValues(string userId, string password, string companyId, string profileType, string objectKey,string  clientId)
        {
            string sqlCommand = "SELECT cpro_type as Type , cKey as ObjectKey, cpro_code as Code, cPro_Value as Value FROM profvalu WHERE CPRO_TYPE+CKEY+CPRO_CODE = '" + profileType.Trim() + objectKey.Trim() + "' ORDER BY cKey, cpro_code";

            AriaDbCommand command = new AriaDbCommand(sqlCommand, new AriaDbConnection("Aria", companyId), Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);
            return command.GetDataTable();
        }

        public DataTable GetProfileValues(string userId, string password, string companyId, string profileType, string objectKey, string code,string clientId)
        {
            string sqlCommand = "SELECT cpro_type as Type , cKey as ObjectKey, cpro_code as Code, cPro_Value as Value FROM profvalu WHERE CPRO_TYPE+CKEY+CPRO_CODE = '" + profileType.Trim() + objectKey.PadRight(130) + code.Trim() + "'  ORDER BY cKey, cpro_code";

            AriaDbCommand command = new AriaDbCommand(sqlCommand, new AriaDbConnection("Aria", companyId), Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);
            return command.GetDataTable();
        }

        public DataTable GetProfileValues(string userId, string password, string companyId, string filter,string clientId)
        {
            string sqlCommand = "SELECT cpro_type as Type , cKey as ObjectKey, cpro_code as Code, cPro_Value as Value FROM profvalu WHERE " + filter + " ORDER BY cKey, cpro_code";

            AriaDbCommand command = new AriaDbCommand(sqlCommand, new AriaDbConnection("Aria", companyId), Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);
            return command.GetDataTable();
        }

        public DataTable GetProfileNewValues(string userId, string password, string companyId,string clientId)
        {
            string sqlCommand = "SELECT cpro_type as Type , cKey as ObjectKey, cpro_code as Code, cPro_Value as Value FROM profvalu WHERE .F.";

            AriaDbCommand command = new AriaDbCommand(sqlCommand, new AriaDbConnection("Aria", companyId), Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);
            DataTable table = command.GetDataTable();

            return table;
        }

        public string GetNewSequence(string userId, string password, string companyId,string sequenceID,string clientId)
        {
            string sqlCommand = "SELECT * FROM sequence WHERE cseq_type = '" + sequenceID.PadRight(8) + "'";

            AriaDbCommand command = new AriaDbCommand(sqlCommand, new AriaDbConnection("Aria", companyId), Aria.Environment.AriaDatabaseTypes.Aria27Data, clientId);
            DataTable table = command.GetDataTable();

            command.CommandText = "update sequence set nseq_no = " + (Int32.Parse(table.Rows[0]["nseq_no"].ToString()) + 1).ToString() + " WHERE cseq_type = '" + sequenceID.PadRight(8) + "'";;

            command.ExecuteNonQuery();
            return table.Rows[0]["nseq_no"].ToString().PadLeft(Convert.ToInt32(table.Rows[0]["nfld_wdth"]), '0');
        }

        public DataTable GetSetup(string userId, string password, string companyId, string applicationID, string setting,string clientId)
        {
            AriaBusinessObjectAdapter adapter = new AriaBusinessObjectAdapter();

            Aria.DataTypes.AriaDataObjectPointer pointer = new Aria.DataTypes.AriaDataObjectPointer();
            pointer.AddKeyField("ApplicationID", applicationID);
            pointer.AddKeyField("Setting", setting.PadRight(10));

            return adapter.GetBusinessObject("Aria", "", companyId, "CRM.Setup", pointer, clientId);
        }
    }
}