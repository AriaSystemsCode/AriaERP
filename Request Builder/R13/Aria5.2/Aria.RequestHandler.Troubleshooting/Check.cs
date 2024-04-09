using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.RequestHandler.Troubleshooting
{
    public abstract class Check
    {
        private string _action;
        public string CheckAction
        {
            get { return _action; }
            set { _action = value; }
        }

        private CheckResult _checkResult;
        public CheckResult CheckResult
        {
            get { return _checkResult; }
            set { _checkResult = value; }
        }

        private string _checkError;
        public string CheckError
        {
            get { return _checkError; }
            set { _checkError = value; }
        }

        private Check _parentCheck;
        public Check ParentCheck
        {
            get { return _parentCheck; }
            set { _parentCheck = value; }
        }

        private List<Check> _childrenCheck = new List<Check>();
        public List<Check> ChildrenCheck
        {
            get { return _childrenCheck; }
            set { _childrenCheck = value; }
        }        

        public abstract void GetCheckResult();       

    }
}
