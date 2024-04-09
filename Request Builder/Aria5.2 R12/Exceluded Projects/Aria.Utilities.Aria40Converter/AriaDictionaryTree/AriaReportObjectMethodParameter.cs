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
    class AriaReportObjectMethodParameter : AriaDictionaryObject
    {
        public AriaObjectMethodParameter AriaMethodParameter = new AriaObjectMethodParameter();

        public AriaReportObjectMethodParameter(AriaDictionaryObject parent)
            : base(parent)
        {
            AriaMethodParameter.MethodName = ((AriaReportObjectMethod)AriaDictionaryObject.GetParent(this, typeof(AriaReportObjectMethod))).AriaObjectMethod.MethodName;
            AriaMethodParameter.ParameterName = "OptionGrid";
            AriaMethodParameter.ParameterNo = 1;
            AriaMethodParameter.ParameterType = AriaDataTypes.AriaOptionGridXmlDataSet;
            AriaMethodParameter.ObjectRevision = AriaDictionaryObject.CurrentRevision;
            AriaMethodParameter.ParameterSettings = new AriaOptionGridXmlDataSetSettings();

            SetAriaMethodParameterSettings((AriaOptionGridXmlDataSetSettings)AriaMethodParameter.ParameterSettings);
        }

        public void SetAriaMethodParameterSettings(AriaOptionGridXmlDataSetSettings settings)
        {
            settings.OptionGridObjectName = ((AriaReportObject)AriaDictionaryObject.GetParent(this, typeof(AriaReportObject))).AriaObject.ObjectName + ".OptionGrid";
            settings.OptionGridRevision = AriaDictionaryObject.CurrentRevision;
        }
        
        public override void Save()
        {
            AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

            AriaXmlSerializer xml = new AriaXmlSerializer();
            string xmlString;
            xmlString = xml.ConvertToXml(AriaMethodParameter.ParameterSettings);

            dictionary.SaveAriaObjectMethodParameter(new Aria.Data.AriaDbConnection("", ""),
                                                     ((AriaReportObject)AriaDictionaryObject.GetParent(this, typeof(AriaReportObject))).AriaObject.ObjectID,
                                                     AriaMethodParameter.ObjectRevision,
                                                    AriaMethodParameter.MethodName,
                                                    1, 
                                                    AriaMethodParameter.ParameterName,
                                                    AriaMethodParameter.ParameterType,
                                                    xmlString, "");
        }
    }
}
