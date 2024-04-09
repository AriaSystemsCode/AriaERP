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
    abstract class AriaDictionaryObjectLeave : AriaDictionaryObject 
    {
        public AriaDictionaryObjectLeave(AriaDictionaryObject parent)
        {
            Parent = parent;
        }
    }
}
