using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;


namespace AriaDevExpress.Module.BusinessObjects.WebSite
{
    public class ApplicationModule : XPLiteObject
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

        ApplicationModule fParentID;
        public ApplicationModule ParentID
        {
            get { return fParentID; }
            set { SetPropertyValue<ApplicationModule>("fParentID", ref fParentID, value); }
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

        FileDataEx fUploadHeaderImgUrl;
        [Aggregated, ExpandObjectMembers(ExpandObjectMembers.Never)]
        public FileDataEx UploadHeaderImgUrl
        {
            get { return fUploadHeaderImgUrl; }
            set
            {
                SetPropertyValue<FileDataEx>("UploadDoc", ref fUploadHeaderImgUrl, value);
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

        FileDataEx fUploadImage1Url;
        [Aggregated, ExpandObjectMembers(ExpandObjectMembers.Never)]
        public FileDataEx UploadImage1Url
        {
            get { return fUploadImage1Url; }
            set
            {
                SetPropertyValue<FileDataEx>("UploadImage1Url", ref fUploadImage1Url, value);
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

        FileDataEx fUploadImage2Url;
        [Aggregated, ExpandObjectMembers(ExpandObjectMembers.Never)]
        public FileDataEx UploadImage2Url
        {
            get { return fUploadImage2Url; }
            set
            {
                SetPropertyValue<FileDataEx>("UploadImage2Url", ref fUploadImage2Url, value);
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

        FileDataEx fUploadImage3Url;
        [Aggregated, ExpandObjectMembers(ExpandObjectMembers.Never)]
        public FileDataEx UploadImage3Url
        {
            get { return fUploadImage3Url; }
            set
            {
                SetPropertyValue<FileDataEx>("UploadImage1Url", ref fUploadImage3Url, value);
            }
        }

        string fVideoURL;
        [Size(500)]
        public string VideoURL
        {
            get { return fVideoURL; }
            set { SetPropertyValue<string>("VideoURL", ref fVideoURL, value); }
        }

        public ApplicationModule(Session session) : base(session) { }
        public ApplicationModule() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }

}
