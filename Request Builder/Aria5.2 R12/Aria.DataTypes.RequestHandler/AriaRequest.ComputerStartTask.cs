using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using Aria.DataTypes;
using Aria.Data.ObjectDictionary;
using Aria.Data.Messaging;

namespace Aria.Data.RequestHandler
{
    public partial class AriaRequest
    {
        public void SetOnComputerStartRequestSettings(string methodObjectName, string methodName, AriaArguments methodArguments,
            AriaLoginTypes loginType, string userID, AriaPriorityTypes priority)
        {
            this._recurrenceType = AriaRecurrenceTypes.ComputerStart;
            this._methodObjectName = methodObjectName;
            this._methodName = methodName;
            this._loginType = loginType;
            this._userID = userID;
            this._methodArguments = methodArguments;
            this._priority = priority;
        }
    }
}