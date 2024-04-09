using System;
using Aria.DataTypes.ObjectDictionary;
using Aria.DataTypes.Messaging;
using Aria.Xml;

namespace Aria.DataTypes.RequestHandler
{
    [Serializable, AriaSerializableAttribute]
    public partial class AriaRequest
    {   // T20100512.0026 Hassan.I 20-05-2010 [Begin]
        private string _clientID ;
        public string clientID
        {   get { return _clientID; }
            set { _clientID = value; }
        }
        // T20100512.0026 Hassan.I 20-05-2010 [Begin]

        private int _requestNumber;
        public int RequestNumber
        {
          get { return _requestNumber; }
          set { _requestNumber = value; }
        }

        private string _requestID = Guid.NewGuid().ToString();
        public string RequestID
        {
            get { return _requestID; }
            set { _requestID = value; }
        }

        private int _parentRequestNumber;
        public int ParentRequestNumber
        {
            get { return _parentRequestNumber; }
            set { _parentRequestNumber = value; }
        }

        private string _parentRequestID = Guid.Empty.ToString();
        public string ParentRequestID
        {
            get { return _parentRequestID; }
            set { _parentRequestID = value; }
        }

        private string _description = "";
        public string Description
        {
            get { return _description; }
            set { _description = value; }
        }

        private DateTime _startAfterDate = new DateTime(1900, 01, 01);
        public DateTime StartAfterDate
        {
            get { return _startAfterDate; }
            set { _startAfterDate = value.Date; }
        }

        private DateTime _endAfterDate = new DateTime(1900, 01, 01);
        public DateTime EndAfterDate
        {
            get { return _endAfterDate; }
            set { _endAfterDate = value.Date; }
        }
        
        private int _endAfterOccurence = 0;
        public int EndAfterOccurence
        {
            get { return _endAfterOccurence; }
            set { _endAfterOccurence = value; }
        }

        private AriaRequestEndAfterTypes _endRequestType = AriaRequestEndAfterTypes.NotEnded;
        public AriaRequestEndAfterTypes EndRequestType
        {
            get { return _endRequestType; }
            set { _endRequestType = value; }
        }
        
        private DateTime _requestStartTime = new DateTime(1900,01,01);
        public DateTime RequestStartTime
        {
            get { return _requestStartTime; }
            set { _requestStartTime = value; }
        }

        private DateTime _nextChildRequestDateTime = new DateTime(1900,01,01);
        public DateTime NextChildRequestDateTime
        {
            get { return _nextChildRequestDateTime; }
            set { _nextChildRequestDateTime = value; }
        }

        private int _occurence = 0;
        public int Occurence
        {
            get { return _occurence; }
            set { _occurence = value; }
        }

        private string _methodObjectName = "";
        public string MethodObjectName
        {
            get { return _methodObjectName; }
            set { _methodObjectName = value; }
        }

        private string _methodName = "";
        public string MethodName
        {
            get { return _methodName; }
            set { _methodName = value; }
        }

        private AriaArgumentList _methodArgumentList = null;
        public AriaArgumentList MethodArgumentList
        {
            get { return _methodArgumentList; }
            set { _methodArgumentList = value; }
        }

        private AriaLoginTypes _loginType = AriaLoginTypes.NotSet;
        public AriaLoginTypes LoginType
        {
            get { return _loginType; }
            set { _loginType = value; }
        }

        private string _userName = "";
        public string UserName
        {
            get { return _userName; }
            set { _userName = value; }
        }

        private string _password = "";
        public string Password
        {
            get { return _password; }
            set { _password = value; }
        }

        private AriaRequestContext _context = null;
        public AriaRequestContext Context
        {
            get { return _context; }
            set { _context = value; }
        }
        
        private AriaRequestPriorityTypes _priority = AriaRequestPriorityTypes.Normal;
        public AriaRequestPriorityTypes Priority
        {
            get { return _priority; }
            set { _priority = value; }
        }

        private AriaRequestRecurrenceTypes _recurrenceType = AriaRequestRecurrenceTypes.Immediate;
        public AriaRequestRecurrenceTypes RecurrenceType
        {
            get { return _recurrenceType; }
            set { _recurrenceType = value; }
        }

        private AriaArgumentList _result = null;
        public AriaArgumentList Result
        {
            get { return _result; }
            set { _result = value; }
        }

        private AriaRequestStatusTypes _status = AriaRequestStatusTypes.OnHold;
        public AriaRequestStatusTypes Status
        {
            get { return _status; }
            set { _status = value; }
        }

        private string _completeTemplateID = "";
        public string CompleteTemplateID
        {
            get { return _completeTemplateID; }
            set { _completeTemplateID = value; }
        }

        private AriaEmail _completeNotification = null;
        public AriaEmail CompleteNotification 
        {
            get { return _completeNotification; }
            set { _completeNotification = value; }
        }

        private string _errorTemplateID = "";
        public string ErrorTemplateID
        {
            get { return _errorTemplateID; }
            set { _errorTemplateID = value; }
        }

        private AriaEmail _errorNotification = null;
        public AriaEmail ErrorNotification 
        {
            get { return _errorNotification; }
            set { _errorNotification = value; }
        }

        private AriaRequestProgress _progress = null;
        public AriaRequestProgress Progress
        {
            get { return _progress; }
            set { _progress = value; }
        }

        private Exception _error = null;
        public Exception Error
        {
            get { return _error; }
            set { _error = value; }
        }

        public void SetRequestLoginSettings(AriaLoginTypes loginType, string userName, string password)
        {
            _loginType = loginType;
            _userName = userName;
            _password = password;
        }

        public void SetRequestContextSettings(string customerName, string companyName)
        {
            _context = new AriaRequestContext();
            _context.RequestId = _requestID;
            _context.CustomerName = customerName;
            _context.CompanyName = companyName;
        }

        public void SetRequestPrioritySettings(AriaRequestPriorityTypes priority)
        {
            _priority = priority;
        }

        public void SetRequestMethodSettings(string methodObjectName, string methodName, AriaArgumentList methodArguments)
        {
            _methodObjectName = methodObjectName;
            _methodName = methodName;
            _methodArgumentList = methodArguments;
        }
                
        public void SetRequestDateRangeAfterDate(DateTime startAfterDateTime, DateTime endAfterDateTime)
        {
            _endRequestType = AriaRequestEndAfterTypes.AfterDate;
            _startAfterDate = startAfterDateTime.Date;
            _endAfterDate = endAfterDateTime.Date;
        }

        public void SetRequestDateRangeAfterOccurence(DateTime startAfterDate, int endAfterOccurence)
        {
            _endRequestType = AriaRequestEndAfterTypes.AfterOccurence;
            _startAfterDate = startAfterDate.Date;
            _endAfterOccurence = endAfterOccurence;
        }

        public void SetRequestDateRangeNotEnded(DateTime startAfterDate)
        {
            _endRequestType = AriaRequestEndAfterTypes.NotEnded;
            _startAfterDate = startAfterDate.Date;
        }
    }
}
