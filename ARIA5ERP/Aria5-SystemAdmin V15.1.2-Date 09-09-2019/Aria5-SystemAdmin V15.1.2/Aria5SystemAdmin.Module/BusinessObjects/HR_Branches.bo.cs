using Aria5.DevExpress.MainSystem.Module.Managers;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Security;
using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class HR_Branches
    {

        public override void AfterConstruction()
        {
               base.AfterConstruction();
               //if (Session.IsNewObject(this))
               //{
               //    HR_EntityType temp = null;
               //    temp = Session.FindObject<HR_EntityType>(CriteriaOperator.Parse("[Id] == 'Bran'"));
               //    if (temp != null)
               //    {
               //        Type = temp;
               //    }
               //    if (Type != null && Type.IdentifierStructture != null)
               //    {
               //        Code = IdentifierStructureManager.GetNextId(Type.IdentifierStructture.IdentifierStructureId, new Dictionary<int, object>(), Session);
               //    }
               //}

               //ATA add this datasourceproperty to detrmin whic divisio to choose for this contact 
               Id = " ";
               string userName1 = ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters).UserName;
               CriteriaOperator criteria = CriteriaOperator.Parse("UserName = '" + userName1 + "'");
               AriaSecuritySystemUser user = Session.FindObject<AriaSecuritySystemUser>(criteria);
               if (user != null)
               {
                   Divisionlist = user.Account.Divisions;
               }
            //ATA add this datasourceproperty to detrmin whic divisio to choose for this contact 
        }
    }
}
