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


namespace Aria.Utilities.Aria40Converter.AriaDictionaryTree
{
    class AriaServerObjectMethod : AriaDictionaryObjectCollection
    {
        public AriaObjectMethod AriaObjectMethod = new AriaObjectMethod();

        public AriaServerObjectMethod(AriaDictionaryObject parent, string methodName)
            : base(parent)
        {
            AriaObjectMethod.MethodName = methodName;
            AriaObjectMethod.ObjectRevision = AriaDictionaryObject.CurrentRevision;
            AriaObjectMethod.ModificationType = AriaModificationTypes.Add;
            AriaObjectMethod.BusinessObjectParameterName = "OptionGrid";
        }

        public override void CreateChildren()
        {
            this.Children.Add(new AriaServerObjectMethodParameter(this));

            base.CreateChildren();
        }
        public override void Save()
        {
            AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

            dictionary.SaveAriaObjectMethod(new Aria.Data.AriaDbConnection("", ""),
                                           ((AriaServerObject)AriaDictionaryObject.GetParent(this, typeof(AriaServerObject))).AriaObject.ObjectID,
                                           AriaObjectMethod.ObjectRevision,
                                           AriaObjectMethod.MethodName,
                                           AriaObjectMethod.ModificationType,
                                           "");

            base.Save();
        }
    }
}
