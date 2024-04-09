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


namespace Aria.Utilities.Aria40Converter.AriaDictionaryTree
{
    class AriaDataObjects : AriaDictionaryObjectCollection
    {
        public AriaDataObjects(AriaDictionaryObject parent)
            : base(parent)
        {
        }

        public override void CreateChildren()
        {

            foreach (Table table in AriaSchema.GetTables().OrderBy(p => p.Description))
            {
                if (table.TableName.Trim() == "APINVHDR")
                {
                    int iii = 0;
                }

                if (!AriaSchema.IsChildTable(table.TableName))
                {
                    this.Children.Add(new AriaDataObject(this, table));
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
