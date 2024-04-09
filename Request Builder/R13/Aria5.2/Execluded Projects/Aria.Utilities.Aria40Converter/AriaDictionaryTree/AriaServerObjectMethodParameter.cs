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
    class AriaServerObjectMethodParameter : AriaDictionaryObject
    {
        public AriaObjectMethodParameter AriaMethodParameter = new AriaObjectMethodParameter();

        public AriaServerObjectMethodParameter(AriaDictionaryObject parent)
            : base(parent)
        {
            AriaMethodParameter.MethodName = ((AriaServerObjectMethod)AriaDictionaryObject.GetParent(this, typeof(AriaServerObjectMethod))).AriaObjectMethod.MethodName;
            AriaMethodParameter.ParameterName = "OptionGrid";
            AriaMethodParameter.ParameterNo = 1;
            AriaMethodParameter.ParameterType = AriaDataTypes.AriaOptionGridXmlDataSet;
            AriaMethodParameter.ObjectRevision = AriaDictionaryObject.CurrentRevision;
            AriaMethodParameter.ParameterSettings = new AriaOptionGridXmlDataSetSettings();

            SetAriaMethodParameterSettings((AriaOptionGridXmlDataSetSettings)AriaMethodParameter.ParameterSettings);
        }

        public void SetAriaMethodParameterSettings(AriaOptionGridXmlDataSetSettings settings)
        {
            settings.OptionGridObjectName = ((AriaServerObject)AriaDictionaryObject.GetParent(this, typeof(AriaServerObject))).AriaObject.ObjectName + ".OptionGrid";
            settings.OptionGridRevision = AriaDictionaryObject.CurrentRevision;
        }

        public override void Save()
        {
            AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

            AriaXmlSerializer xml = new AriaXmlSerializer();
            string xmlString;
            xmlString = xml.ConvertToXml(AriaMethodParameter.ParameterSettings);

            dictionary.SaveAriaObjectMethodParameter(new Aria.Data.AriaDbConnection("", ""),
                                                     ((AriaServerObject)AriaDictionaryObject.GetParent(this, typeof(AriaServerObject))).AriaObject.ObjectID,
                                                     AriaMethodParameter.ObjectRevision,
                                                    AriaMethodParameter.MethodName,
                                                    1,
                                                    AriaMethodParameter.ParameterName,
                                                    AriaMethodParameter.ParameterType,
                                                    xmlString, "");
        }
    }
}
