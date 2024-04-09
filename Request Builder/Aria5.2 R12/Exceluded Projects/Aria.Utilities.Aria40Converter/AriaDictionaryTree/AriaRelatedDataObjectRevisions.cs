using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Utilities.Aria40Converter;
using Aria.DataTypes.ObjectDictionary;
using Aria.Utilities.Aria40Converter.SystemFilesAdaptor;


namespace Aria.Utilities.Aria40Converter.AriaDictionaryTree
{
    class AriaRelatedDataObjectRevisions : AriaDictionaryObjectCollection
    {
        public AriaRelatedDataObjectRevisions(AriaDictionaryObject parent)
            : base(parent)
        {
        }
        
        public override void CreateChildren()
        {
            this.Children.Add(new AriaRelatedDataObjectRevision(this));

            base.CreateChildren();
        }
        public override void Save()
        {
            base.Save();
        }
    }
}
