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
using System.Windows.Forms;
using System.IO;


namespace Aria.Utilities.Aria40Converter.AriaDictionaryTree
{
    class AriaDataObject : AriaDictionaryObjectCollection
    {
        public AriaObject AriaObject = new AriaObject();
        public Table AriaDataObjectTable;
        public string FixedFilter = "";

        public AriaDataObject(AriaDictionaryObject parent, Table table)
            : base(parent)
        {
            AriaDataObjectTable = table;


            AriaObject.ActiveRevision = AriaDictionaryObject.CurrentRevision;
            AriaObject.ObjectName = ((AriaPackageObject)AriaDictionaryObject.GetParent(this, typeof(AriaPackageObject))).Name + "." + table.Description.RemoveSpecialChar();
            AriaObject.ObjectDescription = table.Description;
            AriaObject.ObjectType = AriaObjectTypes.Data;

            AriaSchema.AddObject(AriaObject.ObjectName, table.TableName);

            if (AriaSchema.IsOrgObjectExist(AriaObject.ObjectName))
            {
                FixedFilter = AriaSchema.GetFixedFilter(AriaObject.ObjectName);
                AriaObject.ObjectName = AriaSchema.GetNewObjectName(AriaObject.ObjectName);
            }
        }

        public override void Save()
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            AriaObject parentAriaObject = objectDictionary.LoadAriaObjectByName(new AriaDbConnection("", ""), ((AriaPackageObject)AriaDictionaryObject.GetParent(this, typeof(AriaPackageObject))).Name, "");

            if (AriaSchema.IsNewObjectExist(AriaObject.ObjectName))
            {
                objectDictionary.SaveAriaObject(new AriaDbConnection("", ""),
                                                AriaObject.ObjectName,
                                                AriaObject.ObjectType,
                                                parentAriaObject.ObjectID,
                                                AriaObject.ActiveRevision,
                                                "",
                                                AriaObject.ObjectDescription);

                AriaObject.ObjectID = objectDictionary.LoadAriaObjectByName(new AriaDbConnection("", ""), AriaObject.ObjectName, "").ObjectID;

                base.Save();
            }
        }

        public override void CreateChildren()
        {
            if (AriaSchema.IsNewObjectExist(AriaObject.ObjectName))
            {
                this.Children.Add(new AriaDataObjectRevisions(this));
                this.Children.Add(new AriaChildDataObjects(this));
                this.Children.Add(new AriaRelatedDataObjects(this));

                base.CreateChildren();
            }
        }
    }
}
