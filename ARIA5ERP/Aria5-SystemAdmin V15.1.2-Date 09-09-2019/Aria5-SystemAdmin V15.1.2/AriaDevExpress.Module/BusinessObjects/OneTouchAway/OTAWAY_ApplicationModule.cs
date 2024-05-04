using System;
using System.IO;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;



namespace AriaDevExpress.Module.BusinessObjects.OneTouchAway
{
    public class OTAWAYApplicationModule : XPLiteObject
    {
        int fAppModID;
        [Key(true)]
        public int AppModID
        {
            get { return fAppModID; }
            set { SetPropertyValue<int>("AppModID", ref fAppModID, value); }
        }
        string fShortDesc;
        public string ShortDesc
        {
            get { return fShortDesc; }
            set { SetPropertyValue<string>("ShortDesc", ref fShortDesc, value); }
        }
        string fLongDesc;
        [Size(SizeAttribute.Unlimited)]
        public string LongDesc
        {
            get { return string.IsNullOrWhiteSpace(fLongDesc) ? "" : fLongDesc; }
            set { SetPropertyValue<string>("LongDesc", ref fLongDesc, value); }
        }

        public string UserControl { get; set; }



        bool fVisible;
        public bool Visible
        {
            get { return fVisible; }
            set { SetPropertyValue<bool>("Visible", ref fVisible, value); }
        }

        OTAWAYApplicationModule fParentID;
        public OTAWAYApplicationModule ParentID
        {
            get { return fParentID; }
            set { SetPropertyValue<OTAWAYApplicationModule>("fParentID", ref fParentID, value); }
        }

        [Persistent()]
        private string HeaderImgUrl
        {
            get
            {
                if (fUploadHeaderImgUrl != null)
                    return UploadHeaderImgUrl.RealFileName;
                else
                    return null;
            }
        }

        OTAWAYFileDataEx fUploadHeaderImgUrl;
        [Aggregated, ExpandObjectMembers(ExpandObjectMembers.Never)]
        public OTAWAYFileDataEx UploadHeaderImgUrl
        {
            get { return fUploadHeaderImgUrl; }
            set
            {
                SetPropertyValue<OTAWAYFileDataEx>("UploadDoc", ref fUploadHeaderImgUrl, value);
            }
        }

        [Persistent()]
        private string Image1Url
        {
            get
            {
                if (fUploadImage1Url != null)
                    return fUploadImage1Url.RealFileName;
                else
                    return null;
            }
        }

        OTAWAYFileDataEx fUploadImage1Url;
        [Aggregated, ExpandObjectMembers(ExpandObjectMembers.Never)]
        public OTAWAYFileDataEx UploadImage1Url
        {
            get { return fUploadImage1Url; }
            set
            {
               
                SetPropertyValue<OTAWAYFileDataEx>("UploadImage1Url", ref fUploadImage1Url, value);
                //if (UploadImage1Url != null && UploadImage1Url.File == null)
                //{
                //    UploadImage1Url.File = new FileData(this.Session);
                //    UploadImage1Url.File.FileName = UploadImage1Url.FileName;
                  
                //}
            }
        }
       
    
        [Persistent()]
        private string Image2Url
        {
            get
            {
                if (fUploadImage2Url != null)
                    return fUploadImage2Url.RealFileName;
                else
                    return null;
            }
        }

        OTAWAYFileDataEx fUploadImage2Url;
        [Aggregated, ExpandObjectMembers(ExpandObjectMembers.Never)]
        public OTAWAYFileDataEx UploadImage2Url
        {
            get { return fUploadImage2Url; }
            set
            {
                SetPropertyValue<OTAWAYFileDataEx>("UploadImage2Url", ref fUploadImage2Url, value);
               
            }
        }

        int fAppRank;
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public int AppRank
        {
            get { return fAppRank; }
            set { SetPropertyValue<int>("AppRank", ref fAppRank, value); }
        }
        string fToolTip;
        [Size(200)]
        public string ToolTip
        {
            get { return fToolTip; }
            set { SetPropertyValue<string>("ToolTip", ref fToolTip, value); }
        }

        [Persistent()]
        private string Image3Url
        {
            get
            {
                if (fUploadImage3Url != null)
                    return fUploadImage3Url.RealFileName;
                else
                    return null;
            }

        }

        OTAWAYFileDataEx fUploadImage3Url;
        [Aggregated, ExpandObjectMembers(ExpandObjectMembers.Never)]
        public OTAWAYFileDataEx UploadImage3Url
        {
            get { return fUploadImage3Url; }
            set
            {
                SetPropertyValue<OTAWAYFileDataEx>("UploadImage1Url", ref fUploadImage3Url, value);
            }
        }

        string fVideoURL;
        [Size(500)]
        public string VideoURL
        {
            get { return fVideoURL; }
            set { SetPropertyValue<string>("VideoURL", ref fVideoURL, value); }
        }

        public OTAWAYApplicationModule(Session session) : base(session) { }
        public OTAWAYApplicationModule() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }

}
