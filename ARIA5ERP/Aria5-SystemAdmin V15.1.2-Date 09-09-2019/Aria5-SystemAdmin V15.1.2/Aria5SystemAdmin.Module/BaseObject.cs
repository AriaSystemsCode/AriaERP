using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.DC;
using DevExpress.Xpo;
using DevExpress.Xpo.Metadata;

using System.ComponentModel;
using DevExpress.Persistent.Base;


namespace Aria5SystemAdmin.Module
    {[NonPersistent]
    
    
    public abstract class BaseObject1 : XPCustomObject
    {
        private XPMemberInfo defaultPropertyMemberInfo;
        private bool isDefaultPropertyAttributeInit;
        public static bool IsXpoProfiling = false;
        [Key(true), MemberDesignTimeVisibility(false), Persistent("Oid"), Browsable(false)]
        public Guid oid;
        private static DevExpress.Persistent.BaseImpl.OidInitializationMode oidInitializationMode = DevExpress.Persistent.BaseImpl.OidInitializationMode.OnSaving;
        
        public BaseObject1()
        {
            oid = Guid.Empty;
        }
        
        public BaseObject1(Session session) : base(session)
        {
            oid = Guid.Empty;
        }
        
        public override void AfterConstruction()
        {
            base.AfterConstruction();
            if (oidInitializationMode == DevExpress.Persistent.BaseImpl.OidInitializationMode.AfterConstruction)
            {
                oid = XpoDefault.NewGuid();
            }
        }
        
        protected override void OnSaving()
        {
            base.OnSaving();
            if ((!(base.Session is NestedUnitOfWork) && base.Session.IsNewObject(this)) && oid.Equals(Guid.Empty))
            {
                oid = XpoDefault.NewGuid();
            }
        }
        
        public override string ToString()
        {
            if (!IsXpoProfiling)
            {
                if (!isDefaultPropertyAttributeInit)
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
                        defaultPropertyMemberInfo = base.ClassInfo.FindMember(name);
                    }
                    isDefaultPropertyAttributeInit = true;
                }
                if (defaultPropertyMemberInfo != null)
                {
                    object obj2 = defaultPropertyMemberInfo.GetValue(this);
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
                return oid;
            }
        }
        
        public static DevExpress.Persistent.BaseImpl.OidInitializationMode OidInitializationMode
        {
            get
            {
                return oidInitializationMode;
            }
            set
            {
                oidInitializationMode = value;
            }
        }
    }
}
