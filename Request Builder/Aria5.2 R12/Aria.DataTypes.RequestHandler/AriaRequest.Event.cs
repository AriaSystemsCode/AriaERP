using System;
using Aria.DataTypes;
using Aria.DataTypes.ObjectDictionary;

namespace Aria.DataTypes.RequestHandler
{
    public partial class AriaRequest
    {
        private string _eventObjectName = "";
        public string EventObjectName
        {
            get { return _eventObjectName; }
            set { _eventObjectName = value; }
        }

        private string _eventName = "";
        public string EventName
        {
            get { return _eventName; }
            set { _eventName = value; }
        }

        private AriaConditionList _eventConditionList = null;
        public AriaConditionList EventConditionList
        {
            get { return _eventConditionList; }
            set { _eventConditionList = value; }
        }

        private AriaArgumentList _eventArgumentList = null;
        public AriaArgumentList EventArgumentList
        {
            get { return _eventArgumentList; }
            set { _eventArgumentList = value; }
        }

        public void SetOnEventRequestSettings(string eventObjectName, string eventName, AriaArgumentList eventArguments, AriaConditionList eventConditionList)
        {
            _recurrenceType = AriaRequestRecurrenceTypes.Event;
            _eventObjectName = eventObjectName;
            _eventName = eventName;
            _eventArgumentList = eventArguments;
            _eventConditionList = eventConditionList;
        }
    }
}