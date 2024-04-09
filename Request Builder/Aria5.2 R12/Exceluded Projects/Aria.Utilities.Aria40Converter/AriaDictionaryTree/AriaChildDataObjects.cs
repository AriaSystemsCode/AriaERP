using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Utilities.Aria40Converter;
using Aria.DataTypes.ObjectDictionary;
using Aria.Environment;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.Data;
using Aria.Xml;
using System.Text.RegularExpressions;
using System.Data;
using Aria.DataTypes.Settings;
using Aria.DataTypes;
using Aria.Utilities.Aria40Converter.SystemFilesAdaptor;
using System.Windows.Forms;
using System.IO;


namespace Aria.Utilities.Aria40Converter.AriaDictionaryTree
{
    class AriaChildDataObjects : AriaDictionaryObjectCollection
    {
        public AriaChildDataObjects(AriaDictionaryObject parent)
            : base(parent)
        {
        }

        public override void CreateChildren()
        {
            string parentNameTableName = "";
            if (((AriaChildDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaChildDataObject))) != null)
            {
                parentNameTableName = ((AriaChildDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaChildDataObject))).AriaDataObjectTable.TableName;
            }
            else
            {
                parentNameTableName = ((AriaDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaDataObject))).AriaDataObjectTable.TableName;
            }
            string flagTableName = "";

            foreach (TableRelation relation in AriaSchema.GetTableRelations(parentNameTableName).Where(p => p.RelatedTableName == parentNameTableName && p.RelationType == RelationTypes.ParentChild).OrderBy(p=>p.RelatedTableName))
            {
                Table table = AriaSchema.GetTable(relation.TableName.Trim());

                if (AriaSchema.GetParentCount(table.TableName) == 1)
                {
                    if (flagTableName == table.Description.Trim().ToUpper())
                    {
                        flagTableName = table.Description.Trim().ToUpper();
                    }
                    else
                    {
                        flagTableName = table.Description.Trim().ToUpper();

                        this.Children.Add(new AriaChildDataObject(this, table));
                    }
                }
                else
                {
                    Aria.Utilities.Aria40Converter.SystemFilesAdaptor.Helpers.AppendRecordToException(Path.Combine(table.DatabaseType == DatabaseTypes.Aria4Data ? AriaSchema.MergePath4Xp : AriaSchema.MergePath27, "SYDFILES.XML"), table.Row, "CFILE_NAM", "Child has More than One Parent");
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
