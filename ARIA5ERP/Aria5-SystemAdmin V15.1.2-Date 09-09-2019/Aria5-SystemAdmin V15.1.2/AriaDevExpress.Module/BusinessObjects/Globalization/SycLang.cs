using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Xpo;

namespace AriaDevExpress.Module.BusinessObjects.Globalization
{
    public class SycLang : XPLiteObject
    {
        [Key]
        [Size(2)]
        public string cLang_ID
        {
            get { return GetPropertyValue<string>("cLang_ID"); }
            set { SetPropertyValue<string>("cLang_ID", value); }
        }

        [Size(20)]
        public string cShort_Des
        {
            get { return GetPropertyValue<string>("cShort_Des"); }
            set { SetPropertyValue<string>("cShort_Des", value); }
        }

        public bool lIs_RTL
        {
            get { return GetPropertyValue<bool>("lIs_RTL"); }
            set { SetPropertyValue<bool>("lIs_RTL", value); }
        }

        [Size(16)]
        public string cDate_Format
        {
            get { return GetPropertyValue<string>("cDate_Format"); }
            set { SetPropertyValue<string>("cDate_Format", value); }
        }

        [Size(16)]
        public string cTime_Format
        {
            get { return GetPropertyValue<string>("cTime_Format"); }
            set { SetPropertyValue<string>("cTime_Format", value); }
        }

        [Size(50)]
        public string cFormFont
        {
            get { return GetPropertyValue<string>("cFormFont"); }
            set { SetPropertyValue<string>("cFormFont", value); }
        }

        public short nFormFontS
        {
            get { return GetPropertyValue<short>("nFormFontS"); }
            set { SetPropertyValue<short>("nFormFontS", value); }
        }

        [Size(50)]
        public string cRepFont
        {
            get { return GetPropertyValue<string>("cRepFont"); }
            set { SetPropertyValue<string>("cRepFont", value); }
        }

        public short nRepFontS
        {
            get { return GetPropertyValue<short>("nRepFontS"); }
            set { SetPropertyValue<short>("nRepFontS", value); }
        }

        [Size(2)]
        public string cFormFStyle
        {
            get { return GetPropertyValue<string>("cFormFStyle"); }
            set { SetPropertyValue<string>("cFormFStyle", value); }
        }

        [Size(2)]
        public string cRepFStyle
        {
            get { return GetPropertyValue<string>("cRepFStyle"); }
            set { SetPropertyValue<string>("cRepFStyle", value); }
        }

        [Size(1)]
        public string cDateSeparator
        {
            get { return GetPropertyValue<string>("cDateSeparator"); }
            set { SetPropertyValue<string>("cDateSeparator", value); }
        }


        public SycLang(Session session) : base(session) { }
        public SycLang() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
