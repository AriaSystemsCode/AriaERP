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
    class AriaServerObjectRevision : AriaDictionaryObjectCollection
    {
        public AriaObjectRevision AriaObjectRevision = new AriaObjectRevision();

        public AriaServerObjectRevision(AriaDictionaryObject parent)
            : base(parent)
        {
            AriaObjectRevision.ObjectRevision = AriaDictionaryObject.CurrentRevision;

            AriaObjectRevision.ObjectRevisionSettings = new AriaServerObjectSettings();

            SetAriaServerObjectRevisionSettings((AriaServerObjectSettings)AriaObjectRevision.ObjectRevisionSettings,
                                                  ((AriaServerObject)AriaDictionaryObject.GetParent(this, typeof(AriaServerObject))).AriaServerObjectOptionGrid);
        }

        public void SetAriaServerObjectRevisionSettings(AriaServerObjectSettings settings, OptionGrid optionGrid)
        {
            settings.ClassName = optionGrid.ReportID.Trim() + "." + optionGrid.ReportID.Trim();
            settings.ModificationType = AriaModificationTypes.Add;
            settings.ParentDataObjectRevision = AriaDictionaryObject.CurrentRevision;
        }

        public override void CreateChildren()
        {
            this.Children.Add(new AriaServerObjectMethods(this));

            base.CreateChildren();
        }

        public override void Save()
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            AriaXmlSerializer xmlSerializer = new AriaXmlSerializer();
            string xmlSettings = xmlSerializer.ConvertToXml(AriaObjectRevision.ObjectRevisionSettings);

            objectDictionary.SaveAriaObjectRevision(new AriaDbConnection("", ""),
                                                     ((AriaServerObject)AriaDictionaryObject.GetParent(this, typeof(AriaServerObject))).AriaObject.ObjectID,
                                                     AriaObjectRevision.ObjectRevision,
                                                    xmlSettings,
                                                    "");

            base.Save();
        }
    }
}
