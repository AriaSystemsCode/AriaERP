using System;
using System.ComponentModel;
using System.Linq;

using DevExpress.Xpo;
using DevExpress.Data.Filtering;

using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;
using DevExpress.Persistent.AuditTrail;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    public class Sys_Client : XPLiteObject, IAuditable
    {
        public Sys_Client(Session session)
            : base(session)
        {
            // This constructor is used when an object is loaded from a persistent storage.
            // Do not place any code here or place it only when the IsLoading property is false:
            // if (!IsLoading){
            //    It is now OK to place your initialization code here.
            // }
            // or as an alternative, move your initialization code into the AfterConstruction method.
        }

        [Key(AutoGenerate = false)]
        [Size(5)]
        [RuleRequiredField()]
        [Persistent("CCLIENTID")]
        [RuleUniqueValue("UniqueClient", DefaultContexts.Save)]
        public string ClientID { get; set; }

        [Size(50)]
        [RuleRequiredField()]
        [Persistent("CCLIENTNAME")]
        public string ClientName { get; set; }

        [Association("Client-Field")]
        public XPCollection<Field> Fields
        {
            get { return GetCollection<Field>("Fields"); }
        }

        [Association("Client-File")]
        public XPCollection<File> Files
        {
            get { return GetCollection<File>("Files"); }
        }

        [Association("Client-Report")]
        public XPCollection<Report> Reports
        {
            get { return GetCollection<Report>("Reports"); }
        }


        //[Association("Client-ReportVariable")]
        //public XPCollection<ReportVariable> ReportVariables
        //{
        //    get { return GetCollection<ReportVariable>("ReportVariables"); }
        //}

        [Association("Client-Index")]
        public XPCollection<Index> Indices
        {
            get { return GetCollection<Index>("Indices"); }
        }

        [Association("Client-Menu")]
        public XPCollection<Menu> Menus
        {
            get { return GetCollection<Menu>("Menus"); }
        }

        [Association("Client-FileField")]
        public XPCollection<FileField> FileFields
        {
            get { return GetCollection<FileField>("FileFields"); }
        }

        [Association("Client-Object")]
        public XPCollection<Object> Objects
        {
            get { return GetCollection<Object>("Objects"); }
        }

        private XPCollection<AuditDataItemPersistent> auditTrail;
        public XPCollection<AuditDataItemPersistent> AuditTrail
        {
            get
            {
                if (auditTrail == null)
                {
                    auditTrail = AuditedObjectWeakReference.GetAuditTrail(Session, this);
                }
                return auditTrail;
            }
        }

        public override string ToString()
        {
            string output = base.ToString();
            if (!string.IsNullOrEmpty(ClientID))
                output = ClientID;
            if (!string.IsNullOrEmpty(ClientName))
                output += " - " + ClientName;
            return output;
        }
    }
}