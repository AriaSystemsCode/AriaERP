using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Setup.ClientComponents
{
    public class DllInfo
    {
        private string dllShortName;
        public string DllShortName
        {
            get { return dllShortName; }
            set { dllShortName = value; }
        }

        private string dllLongName;
        public string DllLongName
        {
            get { return dllLongName; }
            set { dllLongName = value; }
        }

        private bool isGacRegistered;
        public bool IsGacRegistered
        {
            get { return isGacRegistered; }
            set { isGacRegistered = value; }
        }

        private bool isRegisteredAssembly;
        public bool IsRegisteredAssembly
        {
            get { return isRegisteredAssembly; }
            set { isRegisteredAssembly = value; }
        }
    }
}
