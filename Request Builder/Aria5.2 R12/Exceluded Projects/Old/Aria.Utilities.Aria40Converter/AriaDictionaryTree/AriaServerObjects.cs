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
using Aria.Utilities.Aria40Converter.Helpers;


namespace Aria.Utilities.Aria40Converter.AriaDictionaryTree
{
    class AriaServerObjects : AriaDictionaryObjectCollection
    {
        public AriaServerObjects(AriaDictionaryObject parent)
            : base(parent)
        {
        }

        public override void CreateChildren()
        {

            foreach (OptionGrid optionGrid in AriaSchema.GetOptionGrids().Where(r => r.Type == OptionGridTypes.Program).OrderBy(p => p.ReportName.RemoveSpecialChar()))
            {
                this.Children.Add(new AriaServerObject(this, optionGrid));
            }

            base.CreateChildren();
        }

        public override void Save()
        {
            base.Save();
        }
    }
}
