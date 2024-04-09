using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
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



namespace Aria.Utilities.Aria40Converter.AriaDictionaryTree
{
    class AriaServerObjectMethods : AriaDictionaryObjectCollection
    {
        public AriaServerObjectMethods(AriaDictionaryObject parent)
            : base(parent)
        {
        }

        public override void CreateChildren()
        {

            this.Children.Add(new AriaServerObjectMethod(this, "Execute"));

            base.CreateChildren();
        }

        public override void Save()
        {
            base.Save();
        }
    }
}
