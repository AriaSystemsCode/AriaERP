using System;
using System.Collections.Generic;
using DevExpress.Xpo;
using DevExpress.ExpressApp ; 

namespace GenericMessageBox
{
    [NonPersistent]
    public class PingSearchEngine
    {

        private readonly string _Message="sdsdsd";
 
        [Custom("Caption", " ")]
        [Size(SizeAttribute.Unlimited)]
        public string Message
        { 
            get { return _Message; }  
        }

        public PingSearchEngine(string Message)
        {
            _Message = Message;
        }
    }

    [NonPersistent]
    public class RefreshSiteMap
    {

        private readonly string _Message = "sdsdsd";

        [Custom("Caption", " ")]
        [Size(SizeAttribute.Unlimited)]
        public string Message
        {
            get { return _Message; }
        }

        public RefreshSiteMap(string Message)
        {
            _Message = Message;
        }
    }
}
