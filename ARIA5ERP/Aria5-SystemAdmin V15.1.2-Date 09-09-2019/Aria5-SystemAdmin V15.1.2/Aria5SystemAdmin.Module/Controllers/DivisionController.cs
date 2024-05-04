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

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class DivisionController : ViewController
    {
        private AriaSecuritySystemUser user;
        private string userName;
        public DivisionController()
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
            
            DivisionAttribute divisionAttribute =  View.ObjectTypeInfo.FindAttribute<DivisionAttribute>(true);
            if (divisionAttribute != null && divisionAttribute.ContainDivision == true && View is ListView)
            {
                if (user.Division != null)
                {
                    ((ListView)View).CollectionSource.Criteria["Filter1"] = new BinaryOperator("Division", user.Division.Oid, BinaryOperatorType.Equal);
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
