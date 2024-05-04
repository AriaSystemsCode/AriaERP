using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Xpo;

namespace AriaDevExpress.Module.BusinessObjects.Globalization
{
    public class SydEnString : XPLiteObject
    {
        [Key(true)]
        public int EnStr_Key
        {
            get { return GetPropertyValue<int>("EnStr_Key"); }
            set { SetPropertyValue<int>("EnStr_Key", value); }
        }

        [Association(@"SydEnString.Obj_Key_SydLangObject")]
        public SydLangObject Obj_Key
        {
            get { return GetPropertyValue<SydLangObject>("Obj_Key"); }
            set { SetPropertyValue<SydLangObject>("Obj_Key", value); }
        }

        [Size(200)]
        public string Row_Key
        {
            get { return GetPropertyValue<string>("Row_Key"); }
            set { SetPropertyValue<string>("Row_Key", value); }
        }

        [Size(30)]
        public string cProperty
        {
            get { return GetPropertyValue<string>("cProperty"); }
            set { SetPropertyValue<string>("cProperty", value); }
        }

        [Size(200)]
        public string OriginalText
        {
            get { return GetPropertyValue<string>("OriginalText"); }
            set { SetPropertyValue<string>("OriginalText", value); }
        }

        [Size(200)]
        public string FullText
        {
            get { return GetPropertyValue<string>("FullText"); }
            set { SetPropertyValue<string>("FullText", value); }
        }

        public short OriginalWidth
        {
            get { return GetPropertyValue<short>("OriginalWidth"); }
            set { SetPropertyValue<short>("OriginalWidth", value); }
        }

        [Size(200)]
        public string OriginalControlName
        {
            get { return GetPropertyValue<string>("OriginalControlName"); }
            set { SetPropertyValue<string>("OriginalControlName", value); }
        }

        //[Association("SydEnString-SydString")]
        //public XPCollection<SydString> SydStrings
        //{
        //    get { return GetCollection<SydString>("SydStrings"); }
        //}

        public SydEnString(Session session) : base(session) { }
        public SydEnString() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
