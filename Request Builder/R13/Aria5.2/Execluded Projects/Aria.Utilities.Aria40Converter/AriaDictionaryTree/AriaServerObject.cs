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
    class AriaServerObject : AriaDictionaryObjectCollection
    {
        public AriaObject AriaObject = new AriaObject();
        public OptionGrid AriaServerObjectOptionGrid;

        public AriaServerObject(AriaDictionaryObject parent, OptionGrid optionGrid)
            : base(parent)
        {
            AriaServerObjectOptionGrid = optionGrid;
            AriaServerObjectOptionGrid.ReportID = optionGrid.ReportID.Trim();
            AriaObject.ActiveRevision = AriaDictionaryObject.CurrentRevision;
            AriaObject.ObjectName = ((AriaPackageObject)AriaDictionaryObject.GetParent(this, typeof(AriaPackageObject))).Name + "." + optionGrid.ReportName.RemoveSpecialChar();
            AriaObject.ObjectDescription = optionGrid.ReportName;
            AriaObject.ObjectType = AriaObjectTypes.Server;
        }

        public override void Save()
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            AriaObject parentAriaObject = objectDictionary.LoadAriaObjectByName(new AriaDbConnection("", ""), ((AriaPackageObject)AriaDictionaryObject.GetParent(this, typeof(AriaPackageObject))).Name, "");

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

        public override void CreateChildren()
        {
            this.Children.Add(new AriaServerObjectRevisions(this));
            this.Children.Add(new AriaServerOptionGridObject(this, AriaServerObjectOptionGrid));
            base.CreateChildren();
        }
    }
}
