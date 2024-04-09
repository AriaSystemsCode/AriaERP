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
    class AriaReportOptionGridObject : AriaDictionaryObjectCollection
    {
        public AriaObject _AriaObject = new AriaObject();
        public OptionGrid AriaReportObjectOptionGrid;

        public AriaReportOptionGridObject(AriaDictionaryObject parent, OptionGrid optionGrid)
            : base(parent)
        {
            AriaReportObjectOptionGrid = optionGrid;

            _AriaObject.ActiveRevision = AriaDictionaryObject.CurrentRevision;
            _AriaObject.ObjectName = ((AriaReportObject)AriaDictionaryObject.GetParent(this, typeof(AriaReportObject))).AriaObject.ObjectName + ".OptionGrid";
            _AriaObject.ObjectDescription = ((AriaReportObject)AriaDictionaryObject.GetParent(this, typeof(AriaReportObject))).AriaObject.ObjectDescription.Trim() + " OptionGrid";
            _AriaObject.ObjectType = AriaObjectTypes.OptionGrid;
        }

        public override void Save()
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            objectDictionary.SaveAriaObject(new AriaDbConnection("", ""),
                                            _AriaObject.ObjectName,
                                            _AriaObject.ObjectType,
                                             ((AriaReportObject)AriaDictionaryObject.GetParent(this, typeof(AriaReportObject))).AriaObject.ObjectID,
                                            _AriaObject.ActiveRevision,
                                            "",
                                            _AriaObject.ObjectDescription);
            _AriaObject.ObjectID = objectDictionary.LoadAriaObjectByName(new AriaDbConnection("", ""), _AriaObject.ObjectName, "").ObjectID;
   

            base.Save();
        }

        public override void CreateChildren()
        {
            this.Children.Add(new AriaReportOptionGridObjectRevisions(this));

            base.CreateChildren();
        } 
    }
}
