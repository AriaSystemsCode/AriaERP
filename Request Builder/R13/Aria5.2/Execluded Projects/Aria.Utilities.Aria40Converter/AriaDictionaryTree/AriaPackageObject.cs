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


namespace Aria.Utilities.Aria40Converter.AriaDictionaryTree
{
    class AriaPackageObject : AriaDictionaryObjectCollection
    {
        public string Name;
        public AriaPackageSettings _ariaPackageSetting=new AriaPackageSettings();

        public AriaPackageObject(AriaDictionaryObject parent, string name)
            : base(parent)
        {
            Name = name;
            _ariaPackageSetting.ModificationType=AriaModificationTypes.Add;
            _ariaPackageSetting.ParentDataObjectRevision = AriaDictionaryObject.CurrentRevision;
        }
        public override void CreateChildren()
        {
            this.Children.Add(new AriaDataObjects(this));
            this.Children.Add(new AriaReportObjects(this));
           this.Children.Add(new AriaServerObjects(this));

            base.CreateChildren();
        }
        
        public override void Save()
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
            objectDictionary.SaveAriaObject(new AriaDbConnection("", ""), Name, AriaObjectTypes.Package, 0, _ariaPackageSetting.ParentDataObjectRevision, "", Name);

            base.Save();
        }
    }
}
