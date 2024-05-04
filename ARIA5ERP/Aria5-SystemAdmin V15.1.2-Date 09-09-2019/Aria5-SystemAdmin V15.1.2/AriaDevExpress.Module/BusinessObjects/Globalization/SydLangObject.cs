using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Xpo;

namespace AriaDevExpress.Module.BusinessObjects.Globalization
{
    public class SydLangObject : XPLiteObject
    {
        [Key(true)]
        public int Obj_Key
        {
            get { return GetPropertyValue<int>("Obj_Key"); }
            set { SetPropertyValue<int>("Obj_Key", value); }
        }

        [Size(3)]
        public string cObjectType
        {
            get { return GetPropertyValue<string>("cObjectType"); }
            set { SetPropertyValue<string>("cObjectType", value); }
        }

        [Size(2)]
        public string cVersion
        {
            get { return GetPropertyValue<string>("cVersion"); }
            set { SetPropertyValue<string>("cVersion", value); }
        }

        [Size(30)]
        public string cObject
        {
            get { return GetPropertyValue<string>("cObject"); }
            set { SetPropertyValue<string>("cObject", value); }
        }

        [Size(6)]
        public string GenUser
        {
            get { return GetPropertyValue<string>("GenUser"); }
            set { SetPropertyValue<string>("GenUser", value); }
        }

        public DateTime GenDateTime
        {
            get { return GetPropertyValue<DateTime>("GenDateTime"); }
            set { SetPropertyValue<DateTime>("GenDateTime", value); }
        }

        [Size(6)]
        public string ModifyUser
        {
            get { return GetPropertyValue<string>("ModifyUser"); }
            set { SetPropertyValue<string>("ModifyUser", value); }
        }

        public DateTime ModifyDateTime
        {
            get { return GetPropertyValue<DateTime>("ModifyDateTime"); }
            set { SetPropertyValue<DateTime>("ModifyDateTime", value); }
        }

        [Association(@"SydEnString.Obj_Key_SydLangObject")]
        public XPCollection<SydEnString> SydEnStrings
        {
            get { return GetCollection<SydEnString>(@"SydEnStrings"); }
        }

        public SydLangObject(Session session) : base(session) { }
        public SydLangObject() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
