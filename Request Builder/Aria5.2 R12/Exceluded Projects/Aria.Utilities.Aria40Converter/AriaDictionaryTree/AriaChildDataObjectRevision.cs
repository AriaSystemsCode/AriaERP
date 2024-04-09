using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Utilities.Aria40Converter;
using Aria.DataTypes.ObjectDictionary;
using Aria.Utilities.Aria40Converter.SystemFilesAdaptor;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.Data;
using Aria.Xml;
using Aria.Environment;

namespace Aria.Utilities.Aria40Converter.AriaDictionaryTree
{
    class AriaChildDataObjectRevision : AriaDictionaryObjectCollection
    {
        public AriaObjectRevision AriaObjectRevision = new AriaObjectRevision();

        public AriaChildDataObjectRevision(AriaDictionaryObject parent)
            : base(parent)
        {
            AriaObjectRevision.ObjectRevision = AriaDictionaryObject.CurrentRevision;

            AriaObjectRevision.ObjectRevisionSettings = new AriaDataObjectSettings();

            SetAriaDataObjectRevisionSettings((AriaDataObjectSettings)AriaObjectRevision.ObjectRevisionSettings,
                                             ((AriaChildDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaChildDataObject))).AriaDataObjectTable);
        }

        public void SetAriaDataObjectRevisionSettings(AriaDataObjectSettings settings, Table table)
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            switch (table.DatabaseType)
            {
                case DatabaseTypes.Aria27SystemFiles:
                    settings.DatabaseType = AriaDatabaseTypes.Aria27SystemFiles;
                    break;

                case DatabaseTypes.Aria27Data:
                    settings.DatabaseType = AriaDatabaseTypes.Aria27Data;
                    break;

                case DatabaseTypes.Aria4Data:
                    settings.DatabaseType = AriaDatabaseTypes.Aria40Data;
                    break;
            }

            string filter = "";

            if (table.DatabaseType.Equals(DatabaseTypes.Aria27Data) || table.DatabaseType.Equals(DatabaseTypes.Aria27SystemFiles))
            {
                List<TableField> TableFields = AriaSchema.GetTableFields(table.TableName);
                string[] ColumnsNameOrg = table.PrimaryKey.Expression.Split('+');
                IndexField[] ColumnsName = VFPSchemaHelper.GetIndexFields(table.PrimaryKey.Expression, SortTypes.Ascending);

                for (int index = 0; index < ColumnsName.Length; index++)
                {
                    if (TableFields.Where(p => p.FieldName.Trim() == ColumnsName[index].FieldName.Trim()).Count() > 0)
                    {
                        TableField tableField = TableFields.First(p => p.FieldName.Trim().ToUpper() == ColumnsName[index].FieldName.ToUpper().Trim().ToUpper());

                        if (tableField.DataType == FieldDataTypes.String)
                            ColumnsNameOrg[index] = ColumnsNameOrg[index].Replace(ColumnsName[index].FieldName, "'@" + ColumnsName[index].FieldName + "@'");
                        else if (tableField.DataType == FieldDataTypes.Number)
                            ColumnsNameOrg[index] = ColumnsNameOrg[index].Replace(ColumnsName[index].FieldName, "@" + ColumnsName[index].FieldName + "@");
                        else if (tableField.DataType == FieldDataTypes.Date)
                            ColumnsNameOrg[index] = ColumnsNameOrg[index].Replace(ColumnsName[index].FieldName, "{@" + ColumnsName[index].FieldName + "@}");
                    }
                }
                filter = string.Join("+", ColumnsNameOrg);

                settings.Filter = table.PrimaryKey.Expression + " = " + filter;
            }
            else
            {
                List<string> ColumnsName = table.PrimaryKey.Expression.Split('+').Select(p => p.Trim()).ToList();
                for (int index = 0; index < ColumnsName.Count; index++)
                {
                    ColumnsName[index] = "[" + ColumnsName[index] + "] = @" + ColumnsName[index];
                }

                filter = string.Join(" AND ", ColumnsName.ToArray());

                settings.Filter = filter;
            }


            settings.ModificationType = AriaModificationTypes.Add;
            settings.ParentDataObjectRevision = AriaDictionaryObject.CurrentRevision;
            settings.TableName = table.TableName;

            settings.FixedFilter = ((AriaChildDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaChildDataObject))).FixedFilter;
        }

        public override void CreateChildren()
        {
            this.Children.Add(new AriaChildDataObjectFields(this));

            base.CreateChildren();
        }

        public override void Save()
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            AriaXmlSerializer xmlSerializer = new AriaXmlSerializer();
            string xmlSettings = xmlSerializer.ConvertToXml(AriaObjectRevision.ObjectRevisionSettings);

            objectDictionary.SaveAriaObjectRevision(new AriaDbConnection("", ""),
                                                    ((AriaChildDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaChildDataObject))).AriaObject.ObjectID,
                                                    AriaObjectRevision.ObjectRevision,
                                                    xmlSettings,
                                                    "");
            base.Save();
        }
    }
}
