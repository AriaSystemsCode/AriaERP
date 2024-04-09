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
using Aria.Utilities.Aria40Converter.Helpers;



namespace Aria.Utilities.Aria40Converter.AriaDictionaryTree
{
    class AriaRelatedDataObjectField : AriaDictionaryObject
    {
        public List<IndexField> IndexFields;
        public TableField TableField;

        public AriaObjectProperty AriaDataObjectProperty = new AriaObjectProperty();

        public AriaRelatedDataObjectField(AriaDictionaryObject parent, List<IndexField> indexFields, TableField tableField)
            : base(parent)
        {
            IndexFields = indexFields;
            TableField = tableField;

            AriaDataObjectProperty.PropertyName = TableField.Head.RemoveSpecialChar();
            AriaDataObjectProperty.PropertyDescription = TableField.Head;
            AriaDataObjectProperty.PropertyType = AriaDataTypes.AriaRelatedField;
            AriaDataObjectProperty.ModificationType = AriaModificationTypes.Add;
            AriaDataObjectProperty.PropertySettings = ((AriaDataTypeSettings)new AriaRelatedFieldSettings());

            SetAriaDataPropertySettings((AriaRelatedFieldSettings)AriaDataObjectProperty.PropertySettings);


            string objName = AriaSchema.GetTableNewObjectName(TableField.TableName);
            
            if (AriaSchema.IsOrgObjectPropertyExist(objName, AriaDataObjectProperty.PropertyName))
            {
                AriaDataObjectProperty.PropertyName = AriaSchema.GetNewObjectPropertyName(objName, AriaDataObjectProperty.PropertyName).Trim();
            }
            else
            {
                if (IndexFields.Where(r => r.FieldName.TrimEnd().ToUpper() == TableField.FieldName.TrimEnd().ToUpper()).Count() == 0)
                {
                    AriaDataObjectProperty.PropertyName = "Not Used";
                }
            }
        }

        public void SetAriaDataPropertySettings(AriaRelatedFieldSettings settings)
        {
            settings.IsPrimaryKey = IndexFields.Where(r => r.FieldName.TrimEnd().ToUpper() == TableField.FieldName.Trim().ToUpper()).Count() == 1;
            settings.FieldName = TableField.FieldName;
            settings.Head = TableField.Head;
            settings.Message = TableField.Message;

            switch (TableField.DataType)
            {
                case FieldDataTypes.Date:
                    settings.DataType = AriaStandardDataTypes.Date;
                    break;

                case FieldDataTypes.General:
                    settings.DataType = AriaStandardDataTypes.Binary;
                    break;

                case FieldDataTypes.Logical:
                    settings.DataType = AriaStandardDataTypes.Logical;
                    break;

                case FieldDataTypes.Memo:
                    settings.DataType = AriaStandardDataTypes.Memo;
                    break;

                case FieldDataTypes.Number:
                    settings.DataType = AriaStandardDataTypes.Numeric;
                    break;

                case FieldDataTypes.String:
                    settings.DataType = AriaStandardDataTypes.String;
                    break;
            }

            settings.Width = TableField.Width;
            settings.DecimalPlaces = TableField.DecimalPlaces;
            settings.ValidExpression = TableField.ValidExpression;
            settings.Mask = TableField.Mask;
            settings.ValidEntry = TableField.IsValidEntery;
            settings.ValidEntries = TableField.ValidEntries;
            settings.Code = TableField.Code;
            settings.HasReleatedField = TableField.HasReleatedField;
            settings.ReleatedFields = TableField.ReleatedFields;
            settings.IsReleatedField = TableField.IsReleatedField;
            settings.HasReleatedField = TableField.HasReleatedField;

            if (AriaSchema.GetSpecialFieldsInformation(TableField.TableName, TableField.FieldName) != null)
            {
                DataRow row = AriaSchema.GetSpecialFieldsInformation(TableField.TableName, TableField.FieldName);
                settings.InternalEmail = row["InternalEmail"] != DBNull.Value ? (bool)row["InternalEmail"] : false;
                settings.IsEmail = row["IsEmail"] != DBNull.Value ? (bool)row["IsEmail"] : false;

                if (row["mventries"] != DBNull.Value)
                {
                    string[] description = ((string)row["mventries"]).Substring(0, ((string)row["mventries"]).IndexOf("~")).Split('|');
                    string[] values = ((string)row["mventries"]).Substring(((string)row["mventries"]).IndexOf("~") + 1).Split('|');

                    for (int validIndex = 0; validIndex < values.Length; validIndex++)
                    {
                        values[validIndex] += "|" + description[validIndex];
                    }

                    settings.ValidEntries = values;
                }
            }
        }

        public override void Save()
        {
            if (AriaDataObjectProperty.PropertyName == "Not Used") return;

            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

           
            AriaXmlSerializer xml = new AriaXmlSerializer();
            string xmlString;
            xmlString = xml.ConvertToXml(AriaDataObjectProperty.PropertySettings);

            string objName = AriaSchema.GetTableNewObjectName(TableField.TableName);
            if (IndexFields.Where(r => r.FieldName.TrimEnd().ToUpper() == TableField.FieldName.TrimEnd().ToUpper()).Count() > 0 || 
                AriaSchema.IsNewObjectPropertyExist(objName, AriaDataObjectProperty.PropertyName))
            {
                AriaDataObjectProperty.PropertyName = AriaDataObjectProperty.PropertyName;

                objectDictionary.SaveAriaObjectProperty(new AriaDbConnection("Aria", ""),
                                                       ((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject))).AriaObject.ObjectID,
                                                       ((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject))).AriaObject.ActiveRevision,
                                                        AriaDataObjectProperty.PropertyName,
                                                        AriaDataObjectProperty.ModificationType,
                                                        AriaDataObjectProperty.PropertyType,
                                                        xmlString,
                                                        "",
                                                        AriaDataObjectProperty.PropertyDescription);
            }
        }
    }
}
