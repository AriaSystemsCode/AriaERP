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
using DevExpress.ExpressApp.Security;
using Aria5SystemAdmin.Module.BusinessObjects;

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class AccountController : ViewController
    {
        private AriaSecuritySystemUser user;
        private string userName;
        public AccountController()
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

            if (View is ListView)
            {
                CriteriaOperator criteriaUser = CriteriaOperator.Parse("UserName = '" + userName + "'");
                user = objectSpace.FindObject<AriaSecuritySystemUser>(criteriaUser);
                if (user.Account.Id != "Aria")
                {
                    ((ListView)View).CollectionSource.Criteria["Filter1"] = new BinaryOperator("Oid", user.Account.Oid, BinaryOperatorType.Equal);
                }
            }
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
    }
}
