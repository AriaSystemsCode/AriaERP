using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Security;
using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class Person
    {

        public override void AfterConstruction()
        {
            //ATA add this datasourceproperty to detrmin whic divisio to choose for this contact 
            string userName1 = ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters).UserName;
            CriteriaOperator criteria = CriteriaOperator.Parse("UserName = '" + userName1 + "'");
            AriaSecuritySystemUser user = this.Session.FindObject<AriaSecuritySystemUser>(criteria);
            if (user != null)
            {
                ListofDevisions = user.Account.Divisions;
            }
            //ATA add this datasourceproperty to detrmin whic divisio to choose for this contact 
            base.AfterConstruction();
        }
    }
}
