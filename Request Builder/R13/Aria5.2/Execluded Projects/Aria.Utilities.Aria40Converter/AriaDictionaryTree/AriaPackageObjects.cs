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
    class AriaPackageObjects : AriaDictionaryObjectCollection
    {
        public AriaPackageObjects(Aria50SchemaInformation ariaSchema)
        {
            AriaSchema = ariaSchema;
        }

        public override void CreateChildren()
        {
            this.Children.Add(new AriaPackageObject(this, "Aria4XP"));
            base.CreateChildren();
        }

        public override void Save()
        {
            base.Save();
        }
    }
}
