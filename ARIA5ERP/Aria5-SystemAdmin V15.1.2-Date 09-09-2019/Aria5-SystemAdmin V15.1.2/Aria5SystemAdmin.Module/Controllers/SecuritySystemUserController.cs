using System;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp;
using DevExpress.Data.Filtering;
using System.Collections.Generic;
using DevExpress.Persistent.Base;
using DevExpress.ExpressApp.Utils;
using DevExpress.ExpressApp.Layout;
using DevExpress.ExpressApp.Actions;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Templates;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.SystemModule;
using DevExpress.ExpressApp.Model.NodeGenerators;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.ExpressApp.Security;
using DevExpress.ExpressApp.Model;
using DevExpress.Xpo;

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class SecuritySystemUserController : ViewController
    {
        private AriaSecuritySystemUser user;
        private string userName;
        public SecuritySystemUserController()
        {
            InitializeComponent();
            RegisterActions(components);
            
            // Target required Views (via the TargetXXX properties) and create their Actions.
        }
        protected override void OnActivated()
        {
            base.OnActivated();
            // Perform various tasks depending on the target View.

            IObjectSpace objectSpace = this.Application.CreateObjectSpace();
            userName = ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters).UserName;
            CriteriaOperator criteriaUser = CriteriaOperator.Parse("UserName = '" + userName + "'");
            user = objectSpace.FindObject<AriaSecuritySystemUser>(criteriaUser);            
        }
        
        protected override void OnViewControlsCreated()
        {
            base.OnViewControlsCreated();
            // Access and customize the target View control.
        }
        protected override void OnDeactivated()
        {
            // Unsubscribe from previously subscribed events and release other references and resources.
            base.OnDeactivated();
        }

        private void SecuritySystemUserController_ViewControlsCreated(object sender, EventArgs e)
        {
            if ((sender as SecuritySystemUserController).View != null)
            {
                if ((sender as SecuritySystemUserController).View.Id == "SecuritySystemUser_LookupListView")
                {                    
                    CollectionSourceBase source = ((sender as SecuritySystemUserController).View as ListView).CollectionSource;

                    XPCollection<AriaSecuritySystemUser> users = new XPCollection<AriaSecuritySystemUser>(user.Session, new BinaryOperator("Account", user.Account.Oid, BinaryOperatorType.Equal));
                    GroupOperator go = new GroupOperator();
                    go.OperatorType = GroupOperatorType.Or;

                    for (int i = 0; i < users.Count; i++)
                    {
                        go.Operands.Add(new BinaryOperator("Oid", users[i].Oid, BinaryOperatorType.Equal));
                    }

                    ((sender as SecuritySystemUserController).View as ListView).CollectionSource.Criteria["C1"] = go;
                }
            }
        }

        
    }
}
