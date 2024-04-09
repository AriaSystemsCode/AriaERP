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

namespace Aria.Utilities.Aria40Converter.AriaDictionaryTree
{
    abstract class AriaDictionaryObjectCollection : AriaDictionaryObject
    {
        public List<AriaDictionaryObject> Children = new List<AriaDictionaryObject>();

        public AriaDictionaryObjectCollection()
        {
        }

        public AriaDictionaryObjectCollection(AriaDictionaryObject parent)
            : base(parent)
        {
            Parent = parent;
        }

        public override void Save()
        {
            foreach (AriaDictionaryObject dectionaryObject in Children)
            {
                dectionaryObject.Save();

                
            }
        }

        public virtual void CreateChildren()
        {
            foreach (AriaDictionaryObject dectionaryObject in Children)
            {
                if (dectionaryObject is AriaDictionaryObjectCollection)
                {
                    ((AriaDictionaryObjectCollection)dectionaryObject).CreateChildren();

                    
                }
            }
        }
    }
}
