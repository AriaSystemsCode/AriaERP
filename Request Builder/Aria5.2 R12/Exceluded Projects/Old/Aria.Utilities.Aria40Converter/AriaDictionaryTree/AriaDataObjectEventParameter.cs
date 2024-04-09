using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Utilities.Aria40Converter.SystemFilesAdaptor;
using Aria.DataTypes.ObjectDictionary;
using Aria.Environment;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.Data;
using Aria.Xml;
using System.Text.RegularExpressions;
using System.Data;
using Aria.DataTypes.Settings;
using Aria.DataTypes;


namespace Aria.Utilities.Aria40Converter.AriaDictionaryTree
{
    class AriaDataObjectEventParameter : AriaDictionaryObject
    {
        public AriaObjectEventParameter AriaEventParameter = new AriaObjectEventParameter();

        public AriaDataObjectEventParameter(AriaDictionaryObject parent)
            : base(parent)
        {
            AriaEventParameter.EventName = ((AriaDataObjectEvent)AriaDictionaryObject.GetParent(this, typeof(AriaDataObjectEvent))).AriaObjectEvent.EventName;
            AriaEventParameter.ParameterName = "Pointer";
            AriaEventParameter.ParameterNo = 1;
            AriaEventParameter.ParameterType = AriaDataTypes.AriaDataObjectPointer;
            AriaEventParameter.ObjectRevision = AriaDictionaryObject.CurrentRevision;
            AriaEventParameter.ParameterSettings = new AriaDataObjectPointerSettings();

            SetAriaEventParameterSettings((AriaDataObjectPointerSettings)AriaEventParameter.ParameterSettings);
        }

        public void SetAriaEventParameterSettings(AriaDataObjectPointerSettings settings)
        {
            //AriaDataObject Not Parent
            settings.DataObjectName = ((AriaDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaDataObject))).AriaObject.ObjectName;
            settings.DataObjectRevision = AriaDictionaryObject.CurrentRevision;
        }
        
        public override void Save()
        {
            AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

           
            AriaXmlSerializer xml = new AriaXmlSerializer();
            string xmlString;
            xmlString = xml.ConvertToXml(AriaEventParameter.ParameterSettings);

            dictionary.SaveAriaObjectEventParameter(new Aria.Data.AriaDbConnection("", ""),
                                                    ((AriaDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaDataObject))).AriaObject.ObjectID,
                                                    AriaEventParameter.ObjectRevision,
                                                    AriaEventParameter.EventName,
                                                    1, 
                                                    AriaEventParameter.ParameterName,
                                                    AriaEventParameter.ParameterType,
                                                    xmlString, "");
        }
    }
}
