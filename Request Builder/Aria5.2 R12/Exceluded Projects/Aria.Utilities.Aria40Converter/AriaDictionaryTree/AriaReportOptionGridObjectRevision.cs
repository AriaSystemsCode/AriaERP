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
    class AriaReportOptionGridObjectRevision : AriaDictionaryObjectCollection
    {
        public AriaObjectRevision AriaObjectRevision = new AriaObjectRevision();

        public AriaReportOptionGridObjectRevision(AriaDictionaryObject parent)
            : base(parent)
        {
            AriaObjectRevision.ObjectRevision = AriaDictionaryObject.CurrentRevision;

            AriaObjectRevision.ObjectRevisionSettings = new AriaOptionGridSettings();

            SetAriaReportOptionGridObjectRevisionSettings((AriaOptionGridSettings)AriaObjectRevision.ObjectRevisionSettings,
                                                          ((AriaReportObject)AriaDictionaryObject.GetParent(this, typeof(AriaReportObject))).AriaReportObjectOptionGrid);
        }

        public void SetAriaReportOptionGridObjectRevisionSettings(AriaOptionGridSettings settings, OptionGrid optionGrid)
        {
            settings.OptionGridId = optionGrid.ReportID;
            settings.ModificationType = AriaModificationTypes.Add;
            settings.ParentDataObjectRevision = AriaDictionaryObject.CurrentRevision;
        }

        public override void CreateChildren()
        {
            foreach(OptionGridVariable var in  AriaSchema.GetOptionGridVariables(((AriaReportObject)AriaDictionaryObject.GetParent(this, typeof(AriaReportObject))).AriaReportObjectOptionGrid.ReportID))
            {
                this.Children.Add(new AriaReportOptionGridOption(this, var));
            }

            base.CreateChildren();
        }

        public override void Save()
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            AriaXmlSerializer xmlSerializer = new AriaXmlSerializer();
            string xmlSettings = xmlSerializer.ConvertToXml(AriaObjectRevision.ObjectRevisionSettings);

            objectDictionary.SaveAriaObjectRevision(new AriaDbConnection("", ""),
                                                    ((AriaReportOptionGridObject)AriaDictionaryObject.GetParent(this, typeof(AriaReportOptionGridObject)))._AriaObject.ObjectID,
                                                    ((AriaReportOptionGridObject)AriaDictionaryObject.GetParent(this, typeof(AriaReportOptionGridObject)))._AriaObject.ActiveRevision,
                                                    xmlSettings,
                                                    "");
            base.Save();
        }
    }
}
