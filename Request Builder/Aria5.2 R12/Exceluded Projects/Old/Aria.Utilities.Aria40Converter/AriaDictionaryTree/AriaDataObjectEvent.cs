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
    class AriaDataObjectEvent : AriaDictionaryObjectCollection
    {
        public AriaObjectEvent AriaObjectEvent = new AriaObjectEvent();

        public AriaDataObjectEvent(AriaDictionaryObject parent, string EventName)
            : base(parent)
        {
            AriaObjectEvent.EventName = EventName;
            AriaObjectEvent.EventDescription = EventName;
            AriaObjectEvent.ObjectRevision = AriaDictionaryObject.CurrentRevision;
            AriaObjectEvent.ModificationType = AriaModificationTypes.Add;
            AriaObjectEvent.BusinessObjectParameterName = "Pointer";
        }

        public override void CreateChildren()
        {
            this.Children.Add(new AriaDataObjectEventParameter(this));

            base.CreateChildren();
        }
        public override void Save()
        {
            AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

            dictionary.SaveAriaObjectEvent(new Aria.Data.AriaDbConnection("", ""),
                                           ((AriaDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaDataObject))).AriaObject.ObjectID,
                                           AriaObjectEvent.ObjectRevision,
                                           AriaObjectEvent.EventName,
                                           AriaObjectEvent.ModificationType,
                                           "",
                                           AriaObjectEvent.EventDescription);
            
            base.Save();
        }
    }
}
