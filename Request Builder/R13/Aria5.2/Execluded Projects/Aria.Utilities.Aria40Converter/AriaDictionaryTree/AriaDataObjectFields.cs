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


namespace Aria.Utilities.Aria40Converter.AriaDictionaryTree
{
    class AriaDataObjectFields : AriaDictionaryObjectCollection
    {
        public AriaDataObjectFields(AriaDictionaryObject parent)
            : base(parent)
        {
        }

        public override void CreateChildren()
        {
            
            Table parentTable = ((AriaDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaDataObject))).AriaDataObjectTable;
            string ariaObjectName = "[" + ((AriaDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaDataObject))).AriaObject.ObjectName.Trim() + "]";


            foreach (TableField column in AriaSchema.GetTableFields(parentTable.TableName).OrderBy(p => p.Head))
            {
                this.Children.Add(new AriaDataObjectField(this, parentTable.PrimaryKey.IndexFields, column));

                if (column.DataType == FieldDataTypes.Date)
                {
                    TableField newColumn = new TableField();

                    newColumn = TableField.Copy(column);
                    newColumn.DataType = FieldDataTypes.Number;
                    newColumn.NumericScale = 6;

                    newColumn.Head = "Days to " + newColumn.Head;
                    string expression = "";
                    if (parentTable.DatabaseType == DatabaseTypes.Aria27Data || parentTable.DatabaseType == DatabaseTypes.Aria27SystemFiles)
                    {
                        expression = newColumn.FieldName.Trim() + " - Date()";
                    }
                    else
                    {
                        expression = "DateDIff(day, " + newColumn.FieldName.Trim() + ", GetDate())";
                    }

                    expression += "|DateDIff(day, " + ariaObjectName.Trim() + ".[" + newColumn.FieldName.Trim() + "], GetDate())";

                    newColumn.FieldName = expression;
                    this.Children.Add(new AriaDataObjectField(this, parentTable.PrimaryKey.IndexFields, newColumn));
                }
            }

            base.CreateChildren();
        }

        public override void Save()
        {
            base.Save();
        }
    }
}
