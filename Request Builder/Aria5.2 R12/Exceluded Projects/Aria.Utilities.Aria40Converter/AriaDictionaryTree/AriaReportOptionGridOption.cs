using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Utilities.Aria40Converter;
using Aria.Environment;
using Aria.DataTypes.ObjectDictionary;
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
    class AriaReportOptionGridOption : AriaDictionaryObject
    {
        public OptionGridVariable OptionGridVariable;

        public AriaObjectProperty AriaDataObjectProperty = new AriaObjectProperty();

        public AriaReportOptionGridOption(AriaDictionaryObject parent, OptionGridVariable optionGridVariable)
            : base(parent)
        {
            OptionGridVariable = optionGridVariable;

            AriaDataObjectProperty.PropertyName = optionGridVariable.Head;
            AriaDataObjectProperty.PropertyDescription = optionGridVariable.Head;
            AriaDataObjectProperty.PropertyType = AriaDataTypes.AriaOption;
            AriaDataObjectProperty.ModificationType = AriaModificationTypes.Add;


            AriaDataObjectProperty.PropertySettings = ((AriaDataTypeSettings)new AriaOptionSettings());

            SetAriaOptionPropertySettings((AriaOptionSettings)AriaDataObjectProperty.PropertySettings);
        }

        public void SetAriaOptionPropertySettings(AriaOptionSettings settings)
        {
            settings.VariableName = OptionGridVariable.VariableName;
            settings.FieldName = OptionGridVariable.FieldName;
            settings.Description = OptionGridVariable.Description;
            settings.Head = OptionGridVariable.Head;
            settings.Message = OptionGridVariable.Message;

            switch (OptionGridVariable.DataType)
            {
                case FieldDataTypes.Date:
                    settings.DataType = AriaStandardDataTypes.Date;
                    break;

                case FieldDataTypes.General:
                    settings.DataType = AriaStandardDataTypes.Binary;
                    break;

                case FieldDataTypes.Logical:
                    settings.DataType = AriaStandardDataTypes.Logical;
                    break;

                case FieldDataTypes.Memo:
                    settings.DataType = AriaStandardDataTypes.Memo;
                    break;

                case FieldDataTypes.Number:
                    settings.DataType = AriaStandardDataTypes.Numeric;
                    break;

                case FieldDataTypes.String:
                    settings.DataType = AriaStandardDataTypes.String;
                    break;
            }

            settings.Width = OptionGridVariable.Width;
            settings.DecimalPlaces = OptionGridVariable.DecimalPlaces;
            settings.DefaultValue = OptionGridVariable.DefaultValue;
            settings.DefaultType = OptionGridVariable.DefaultType == OptionGridVariableValueTypes.Expression ? AriaOptionDefaultValueTypes.Expression : AriaOptionDefaultValueTypes.Fixed;
            settings.ValidExpression = OptionGridVariable.ValidExpression;
            settings.Mask = OptionGridVariable.Mask;
            settings.ValidEntry = OptionGridVariable.ValidEntry;
            settings.ValidEntries = OptionGridVariable.ValidEntries;
            settings.Code = OptionGridVariable.Code;
            settings.HideExpression = OptionGridVariable.HideExpression;
            settings.Editor = OptionGridVariable.Editor;
            settings.BrowseFields = OptionGridVariable.BrowseFields;
            settings.BrowseField = OptionGridVariable.BrowseField;
            settings.BrowseFilter = OptionGridVariable.BrowseFilter;
            settings.BrowseOpenFunction = OptionGridVariable.BrowseOpenFunction;
            settings.BrowseSelectFunction = OptionGridVariable.BrowseSelectFunction;
            settings.BrowseUnselectFunction = OptionGridVariable.BrowseUnselectFunction;
        }

        public override void Save()
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            AriaXmlSerializer xml = new AriaXmlSerializer();
            string xmlString;
            xmlString = xml.ConvertToXml(AriaDataObjectProperty.PropertySettings);

            objectDictionary.SaveAriaObjectProperty(new AriaDbConnection("Aria", ""),
                                                    ((AriaReportOptionGridObject)AriaDictionaryObject.GetParent(this, typeof(AriaReportOptionGridObject)))._AriaObject.ObjectID,
                                                    ((AriaReportOptionGridObject)AriaDictionaryObject.GetParent(this, typeof(AriaReportOptionGridObject)))._AriaObject.ActiveRevision, 
                                                    AriaDataObjectProperty.PropertyName,
                                                    AriaDataObjectProperty.ModificationType,
                                                    AriaDataObjectProperty.PropertyType, 
                                                    xmlString, 
                                                    "",
                                                    AriaDataObjectProperty.PropertyDescription);
        }
    }
}
