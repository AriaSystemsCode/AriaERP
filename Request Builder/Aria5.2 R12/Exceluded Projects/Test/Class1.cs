using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Test
{
    [Serializable]
    class Class1
    {
        private string _a1;

        public string A1
        {
            get { return _a1; }
            set { _a1 = value; }
        }

        [System.Runtime.Serialization.OptionalField()]
        private string _a2;

        public string A2
        {
            get { return _a2; }
            set { _a2 = value; }
        }
    }
}
