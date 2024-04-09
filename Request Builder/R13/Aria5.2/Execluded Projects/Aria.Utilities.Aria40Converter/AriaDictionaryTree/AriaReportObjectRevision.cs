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
    class AriaReportObjectRevision : AriaDictionaryObjectCollection
    {
        public AriaObjectRevision AriaObjectRevision = new AriaObjectRevision();

        public AriaReportObjectRevision(AriaDictionaryObject parent)
            : base(parent)
        {
            AriaObjectRevision.ObjectRevision = AriaDictionaryObject.CurrentRevision;

            AriaObjectRevision.ObjectRevisionSettings = new AriaReportObjectSettings();

            SetAriaReportObjectRevisionSettings((AriaReportObjectSettings)AriaObjectRevision.ObjectRevisionSettings,
                                                  ((AriaReportObject)AriaDictionaryObject.GetParent(this, typeof(AriaReportObject))).AriaReportObjectOptionGrid);
        }

        public void SetAriaReportObjectRevisionSettings(AriaReportObjectSettings settings, OptionGrid optionGrid)
        {
            settings.ClassName = optionGrid.ReportID.Trim() + "." + optionGrid.ReportID.Trim();
            settings.SupportedFormats = new AriaOutputFormatTypes[] { AriaOutputFormatTypes.Excel, AriaOutputFormatTypes.Html, AriaOutputFormatTypes.Pdf, AriaOutputFormatTypes.Txt, AriaOutputFormatTypes.Xml };
            settings.ModificationType = AriaModificationTypes.Add;
            settings.ParentDataObjectRevision = AriaDictionaryObject.CurrentRevision;
        }

        public override void CreateChildren()
        {
            this.Children.Add(new AriaReportObjectMethods(this));

            base.CreateChildren();
        }

        public override void Save()
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            AriaXmlSerializer xmlSerializer = new AriaXmlSerializer();
            string xmlSettings = xmlSerializer.ConvertToXml(AriaObjectRevision.ObjectRevisionSettings);

            objectDictionary.SaveAriaObjectRevision(new AriaDbConnection("", ""),
                                                     ((AriaReportObject)AriaDictionaryObject.GetParent(this, typeof(AriaReportObject))).AriaObject.ObjectID,
                                                     AriaObjectRevision.ObjectRevision,
                                                    xmlSettings,
                                                    "");

            base.Save();
        }
    }
}
