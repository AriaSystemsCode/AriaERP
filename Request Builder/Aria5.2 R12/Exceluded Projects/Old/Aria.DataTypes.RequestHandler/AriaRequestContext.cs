using System;
using Aria.Xml;
using System.ComponentModel;

namespace Aria.DataTypes.RequestHandler
{
    [Serializable, AriaSerializableAttribute, TypeConverter(typeof(ExpandableObjectConverter))]
    public class AriaRequestContext
    {
        private string _requestId = "";
        public string RequestId
        {
            get { return _requestId; }
            set { _requestId = value; }
        }

        private string _customerName = "";
        public string CustomerName
        {
            get { return _customerName; }
            set { _customerName = value; }
        }

        private string _userName = "";
        public string UserName
        {
            get { return _userName; }
            set { _userName = value; }
        }

        private string _companyName = "";
        public string CompanyName
        {
            get { return _companyName; }
            set { _companyName = value; }
        }

        private string _eventName = "";
        public string EventName
        {
            get { return _eventName; }
            set { _eventName = value; }
        }


        private string _methodName = "";
        public string MethodName
        {
            get { return _methodName; }
            set { _methodName = value; }
        }

        private bool _isInternal = false;
        public bool IsInternal
        {
            get { return _isInternal; }
            set { _isInternal = value; }
        }
    }
}
