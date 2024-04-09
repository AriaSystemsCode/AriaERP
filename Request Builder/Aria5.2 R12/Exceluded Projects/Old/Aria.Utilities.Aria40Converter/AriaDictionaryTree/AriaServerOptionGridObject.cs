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
    class AriaServerOptionGridObject : AriaDictionaryObjectCollection
    {
        public AriaObject _AriaObject = new AriaObject();
        public OptionGrid AriaServerObjectOptionGrid;

        public AriaServerOptionGridObject(AriaDictionaryObject parent, OptionGrid optionGrid)
            : base(parent)
        {
            AriaServerObjectOptionGrid = optionGrid;

            _AriaObject.ActiveRevision = AriaDictionaryObject.CurrentRevision;
            _AriaObject.ObjectName = ((AriaServerObject)AriaDictionaryObject.GetParent(this, typeof(AriaServerObject))).AriaObject.ObjectName + ".OptionGrid";
            _AriaObject.ObjectDescription = ((AriaServerObject)AriaDictionaryObject.GetParent(this, typeof(AriaServerObject))).AriaObject.ObjectDescription.Trim() + " OptionGrid";
            _AriaObject.ObjectType = AriaObjectTypes.OptionGrid;
        }

        public override void Save()
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            objectDictionary.SaveAriaObject(new AriaDbConnection("", ""),
                                            _AriaObject.ObjectName,
                                            _AriaObject.ObjectType,
                                             ((AriaServerObject)AriaDictionaryObject.GetParent(this, typeof(AriaServerObject))).AriaObject.ObjectID,
                                            _AriaObject.ActiveRevision,
                                            "",
                                            _AriaObject.ObjectDescription);
            _AriaObject.ObjectID = objectDictionary.LoadAriaObjectByName(new AriaDbConnection("", ""), _AriaObject.ObjectName, "").ObjectID;


            base.Save();
        }

        public override void CreateChildren()
        {
            this.Children.Add(new AriaServerOptionGridObjectRevisions(this));

            base.CreateChildren();
        }
    }
}
