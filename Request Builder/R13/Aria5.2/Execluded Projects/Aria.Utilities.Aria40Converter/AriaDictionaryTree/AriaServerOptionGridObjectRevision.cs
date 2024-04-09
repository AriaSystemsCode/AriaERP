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
    class AriaServerOptionGridObjectRevision : AriaDictionaryObjectCollection
    {
        public AriaObjectRevision AriaObjectRevision = new AriaObjectRevision();

        public AriaServerOptionGridObjectRevision(AriaDictionaryObject parent)
            : base(parent)
        {
            AriaObjectRevision.ObjectRevision = AriaDictionaryObject.CurrentRevision;

            AriaObjectRevision.ObjectRevisionSettings = new AriaOptionGridSettings();

            SetAriaServerOptionGridObjectRevisionSettings((AriaOptionGridSettings)AriaObjectRevision.ObjectRevisionSettings,
                                                          ((AriaServerObject)AriaDictionaryObject.GetParent(this, typeof(AriaServerObject))).AriaServerObjectOptionGrid);
        }

        public void SetAriaServerOptionGridObjectRevisionSettings(AriaOptionGridSettings settings, OptionGrid optionGrid)
        {
            settings.OptionGridId = optionGrid.ReportID;
            settings.ModificationType = AriaModificationTypes.Add;
            settings.ParentDataObjectRevision = AriaDictionaryObject.CurrentRevision;
        }

        public override void CreateChildren()
        {
            foreach (OptionGridVariable var in AriaSchema.GetOptionGridVariables(((AriaServerObject)AriaDictionaryObject.GetParent(this, typeof(AriaServerObject))).AriaServerObjectOptionGrid.ReportID))
            {
                this.Children.Add(new AriaServerOptionGridOption(this, var));
            }

            base.CreateChildren();
        }

        public override void Save()
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            AriaXmlSerializer xmlSerializer = new AriaXmlSerializer();
            string xmlSettings = xmlSerializer.ConvertToXml(AriaObjectRevision.ObjectRevisionSettings);

            objectDictionary.SaveAriaObjectRevision(new AriaDbConnection("", ""),
                                                    ((AriaServerOptionGridObject)AriaDictionaryObject.GetParent(this, typeof(AriaServerOptionGridObject)))._AriaObject.ObjectID,
                                                    ((AriaServerOptionGridObject)AriaDictionaryObject.GetParent(this, typeof(AriaServerOptionGridObject)))._AriaObject.ActiveRevision,
                                                    xmlSettings,
                                                    "");
            base.Save();
        }
    }
}
