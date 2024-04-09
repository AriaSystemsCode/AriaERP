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
using Aria.Utilities.Aria40Converter.Helpers;


namespace Aria.Utilities.Aria40Converter.AriaDictionaryTree
{
    class AriaRelatedDataObjects : AriaDictionaryObjectCollection
    {
        public AriaRelatedDataObjects(AriaDictionaryObject parent)
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

            foreach (TableRelation relation in AriaSchema.GetTableRelations(parentNameTableName).Where(p => p.TableName != parentNameTableName.Trim() && p.RelationType == RelationTypes.Related).OrderBy(p => p.TableName))
            {
                if (!AriaSchema.IsChildTable(relation.TableName))
                {
                    Table table = AriaSchema.GetTable(relation.TableName.Trim());
                    this.Children.Add(new AriaRelatedDataObject(this, table, relation));
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
