using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class EntityOperationPermission
    {
        protected override void OnSaving()
        {
            //base.OnSaving();
            //CriteriaOperator criteriaPermission = CriteriaOperator.Parse("AriaSecuritySystemRole = '" + AriaSecuritySystemRole.Oid + "'" + " AND " + "AriaObject = '" + AriaObject.Oid + "'" + " AND " + "Application_T = '" + Application_T.Oid + "'");
            //EntityOperationPermission permission = Session.FindObject<EntityOperationPermission>(criteriaPermission);

            //if (permission != null)
            //{
            //    //permission = new EntityOperationPermission(Session);
            //}

            //permission.AllowAdd = AllowAdd;
            //permission.AllowEdit = AllowEdit;
            //permission.AllowView = AllowView;
            //permission.AllowDelete = AllowDelete;
            //permission.Application_T = Application_T;
            //permission.AriaObject = AriaObject;
            //permission.AriaSecuritySystemRole = AriaSecuritySystemRole;

            //permission.Save();
            //permission.Session.CommitTransaction();
           
        }
    }
}
