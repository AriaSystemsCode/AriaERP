using DevExpress.ExpressApp;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Xpo.Metadata;
using System;
using System.Collections.Generic;
using System.ComponentModel; 
using System.Linq;
using System.Text; 
using System.Threading.Tasks;


namespace Aria5.DevExpress.OneTouchAway.Module.BusinessObjects
{
    [NonPersistent]

    public abstract class ClientBaseObject : XPCustomObject
    {
        //18-05-2016 Threading Problem due to static property [Start]
     //   public Boolean CallFromSync { get; set; }

        //18-05-2016 Threading Problem due to static property [End]
        private XPMemberInfo defaultPropertyMemberInfo;
        private bool isDefaultPropertyAttributeInit;
        public static bool IsXpoProfiling = false;
        [Key(true), MemberDesignTimeVisibility(false), Persistent("Oid"), Browsable(false)]
        public Guid oid;
        
        public ClientBaseObject()
        {
            this.oid = Guid.Empty;
        }

        public ClientBaseObject(Session session)
            : base(session)
        {
            this.oid = Guid.Empty;
        }
        
        public override void AfterConstruction()
        {
            base.AfterConstruction();
            this.oid = XpoDefault.NewGuid();
        }
        
        protected override void OnSaving()
        {
            base.OnSaving();
            if ((!(base.Session is NestedUnitOfWork) && base.Session.IsNewObject(this)) && this.oid.Equals(Guid.Empty))
            {
                this.oid = XpoDefault.NewGuid();
            }

            if (!this.IsDeleted)
            {
                ClientDataAudit audit = new ClientDataAudit(this.Session);
                audit.AuditedObjectTypeName = this.GetType().FullName;
                audit.AuditedObject = this.Oid;
                audit.AddDateTime = DateTime.Now;
                if (base.Session.IsNewObject(this) && base.Session.IsObjectToSave(this)) audit.AuditOperationType = "ObjectCreated";
                if (!base.Session.IsNewObject(this) && base.Session.IsObjectToSave(this)) audit.AuditOperationType = "ObjectChanged";

                audit.Save();
                ClientDataAuditOid = audit.Oguid;
            }
        }

        protected override void OnDeleting()
        {
            base.OnDeleting();

            ClientDataAudit audit = new ClientDataAudit(this.Session);
            audit.AuditedObjectTypeName = this.GetType().FullName;
            audit.AuditedObject = this.Oid;
            audit.AddDateTime = DateTime.Now;
            audit.AuditOperationType = "ObjectDeleted";
            audit.Save();
            ClientDataAuditOid = audit.Oguid;
        }
        
        public override string ToString()
        {
            if (!IsXpoProfiling)
            {
                if (!this.isDefaultPropertyAttributeInit)
                {
                    string name = string.Empty;
                    XafDefaultPropertyAttribute attribute = XafTypesInfo.Instance.FindTypeInfo(base.GetType()).FindAttribute<XafDefaultPropertyAttribute>();
                    if (attribute != null)
                    {
                        name = attribute.Name;
                    }
                    else
                    {
                        DefaultPropertyAttribute attribute2 = XafTypesInfo.Instance.FindTypeInfo(base.GetType()).FindAttribute<DefaultPropertyAttribute>();
                        if (attribute2 != null)
                        {
                            name = attribute2.Name;
                        }
                    }
                    if (!string.IsNullOrEmpty(name))
                    {
                        this.defaultPropertyMemberInfo = base.ClassInfo.FindMember(name);
                    }
                    this.isDefaultPropertyAttributeInit = true;
                }
                if (this.defaultPropertyMemberInfo != null)
                {
                    object obj2 = this.defaultPropertyMemberInfo.GetValue(this);
                    if (obj2 != null)
                    {
                        return obj2.ToString();
                    }
                }
            }
            return base.ToString();
        }
        
        [Browsable(false), PersistentAlias("oid")]
        public Guid Oid
        {
            get
            {
                return this.oid;
            }
        }

        [NonPersistent]
        public Guid ClientDataAuditOid { get; set; }
    }
}
