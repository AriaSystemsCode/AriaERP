using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;


namespace AriaDevExpress.Module.BusinessObjects.OneTouchAway
{
    public class OTAWAYArticle : XPLiteObject
    {
        int fArticle_ID;
        [Key(true)]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public int Article_ID
        {
            get { return fArticle_ID; }
            set { SetPropertyValue<int>("Article_ID", ref fArticle_ID, value); }
        }
        OTAWAYCategory fCategory_ID;
        public OTAWAYCategory Category_ID
        {
            get { return fCategory_ID; }
            set { SetPropertyValue<OTAWAYCategory>("Category_ID", ref fCategory_ID, value); }
        }
        OTAWAYComboboxEnum fStatus;
        public OTAWAYComboboxEnum Status
        {
            get { return fStatus; }
            set { SetPropertyValue<OTAWAYComboboxEnum>("Status", ref fStatus, value); }
        }
        bool fVisible;
        public bool Visible
        {
            get { return fVisible; }
            set { SetPropertyValue<bool>("Visible", ref fVisible, value); }
        }
        short fVisibleRank;
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public short VisibleRank
        {
            get { return fVisibleRank; }
            set { SetPropertyValue<short>("VisibleRank", ref fVisibleRank, value); }
        }
        string fTitle;
        [Size(200)]
        public string Title
        {
            get { return fTitle; }
            set { SetPropertyValue<string>("Title", ref fTitle, value); }
        }
        string fDescription1;
        [Size(SizeAttribute.Unlimited)]
        public string Description1
        {
            get { return fDescription1; }
            set { SetPropertyValue<string>("Description1", ref fDescription1, value); }
        }
        string fURL;
        [Size(500)]
        public string URL
        {
            get { return fURL; }
            set { SetPropertyValue<string>("URL", ref fURL, value); }
        }
        bool fOpenURL;
        public bool OpenURL
        {
            get { return fOpenURL; }
            set { SetPropertyValue<bool>("OpenURL", ref fOpenURL, value); }
        }

        [Size(500)]
        [Persistent()]
        private string RightImage
        {
            get
            {
                if (fUploadRightImage != null)
                    return fUploadRightImage.RealFileName;
                else
                    return null;
            }
        }

        OTAWAYFileDataEx fUploadRightImage;
        [Aggregated, ExpandObjectMembers(ExpandObjectMembers.Never)]
        public OTAWAYFileDataEx UploadRightImage
        {
            get { return fUploadRightImage; }
            set
            {
                SetPropertyValue<OTAWAYFileDataEx>("UploadRightImage", ref fUploadRightImage, value);
            }
        }

        [Size(500)]
        [Persistent()]
        private string MiddleImage
        {
            get
            {
                if (fUploadMiddleImage != null)
                    return fUploadMiddleImage.RealFileName;
                else
                    return null;
            }
        }

        OTAWAYFileDataEx fUploadMiddleImage;
        [Aggregated, ExpandObjectMembers(ExpandObjectMembers.Never)]
        public OTAWAYFileDataEx UploadMiddleImage
        {
            get { return fUploadMiddleImage; }
            set
            {
                SetPropertyValue<OTAWAYFileDataEx>("UploadRightImage", ref fUploadMiddleImage, value);
            }
        }

        string fDescription2;
        [Size(SizeAttribute.Unlimited)]
        public string Description2
        {
            get { return fDescription2; }
            set { SetPropertyValue<string>("Description2", ref fDescription2, value); }
        }
        DateTime fReleased;
        public DateTime Released
        {
            get { return fReleased; }
            set { SetPropertyValue<DateTime>("Released", ref fReleased, value); }
        }

        string fVideoURL;
        [Size(500)]
        public string VideoURL
        {
            get { return fVideoURL; }
            set { SetPropertyValue<string>("VideoURL", ref fVideoURL, value); }
        }

        string fMenuDesc;
        [Size(200)]
        public string MenuDesc
        {
            get { return fMenuDesc; }
            set { SetPropertyValue<string>("MenuDesc", ref fMenuDesc, value); }
        }
        public OTAWAYArticle(Session session) : base(session) { }
        public OTAWAYArticle() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}