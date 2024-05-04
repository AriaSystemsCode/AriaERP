using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace AriaDevExpress.Module.BusinessObjects.WebSite
{
    class EmailTemplate : XPLiteObject
    {
        
        string _ID;
        [Key(true)]
        [Size(10)]
        public string ID
        {
            get { return _ID; }
            set { SetPropertyValue<string>("ID", ref _ID, value); }
        }
        string _TO;
        [Size(50)]
        public string TO
        {
            get { return _TO; }
            set { SetPropertyValue<string>("TO", ref _TO, value); }
        }


        string _CC;
        [Size(50)]
        public string CC
        {
            get { return _CC; }
            set { SetPropertyValue<string>("CC", ref _CC, value); }
        }

        string _BCC;
        [Size(50)]
        public string BCC
        {
            get { return _BCC; }
            set { SetPropertyValue<string>("BCC", ref _BCC, value); }
        }


        string _ReplayTo;
        [Size(50)]
        public string ReplayTo
        {
            get { return _ReplayTo; }
            set { SetPropertyValue<string>("ReplayTo", ref _ReplayTo, value); }

        }



        string _Subject;
        [Size(50)]
        public string Subject
        {
            get { return _Subject; }
            set { SetPropertyValue<string>("Subject", ref _Subject, value); }

        }

        string _Body;
        [Size(SizeAttribute.Unlimited)]
        public string Body
        {
            get { return _Body; }
            set { SetPropertyValue<string>("Body", ref _Body, value); }

        }




    }
}


    