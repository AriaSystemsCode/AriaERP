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
using DevExpress.ExpressApp.Web.Editors.ASPx;
using DevExpress.ExpressApp.Security.Strategy;
using DevExpress.ExpressApp.Web.SystemModule;

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class AriaSecuritySystemUserController : ViewController
    {
        private AriaSecuritySystemUser user;
        private string userName;
        private List<SecuritySystemRole> rolesList;

        public AriaSecuritySystemUserController()
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
           
            if (View is DetailView)
            {
                CriteriaOperator criteria = CriteriaOperator.Parse("UserName = '" + userName + "'");
                user = (View.CurrentObject as AriaSecuritySystemUser).Session.FindObject<AriaSecuritySystemUser>(criteria);
                
                if (((DetailView)View).ViewEditMode == ViewEditMode.Edit)
                {
                    (View.CurrentObject as AriaSecuritySystemUser).Account = user.Account;
                }
            }
            else if (View is ListView)
            {
                CriteriaOperator criteriaUser = CriteriaOperator.Parse("UserName = '" + userName + "'");
                user = objectSpace.FindObject<AriaSecuritySystemUser>(criteriaUser);
                if (user.Account.Id.ToUpper().Trim() != "ARIA")
                {
                    
                    ((ListView)View).CollectionSource.Criteria["Filter1"] = new BinaryOperator("Account", user.Account.Oid, BinaryOperatorType.Equal);
                }
            }

            rolesList = new List<SecuritySystemRole>();
            List<SecuritySystemRole> userRole = new List<SecuritySystemRole>();
            foreach (SecuritySystemRole role in user.Roles)
            {
                userRole.Add(role);
            }
            GetRoles(userRole);

            bool isAdmin = false;
            foreach (var role in rolesList)
            {
                if (role.IsAdministrative)
                {
                    isAdmin = true;
                }
            }
            if (!isAdmin)
            {
                //Frame.GetController<ModificationsController>().Actions["Save"].Enabled.SetItemValue("Disable", false);
                //Frame.GetController<ModificationsController>().Actions["SaveAndClose"].Enabled.SetItemValue("Disable", false);
                //Frame.GetController<ModificationsController>().Actions["SaveAndNew"].Enabled.SetItemValue("Disable", false);
                //Frame.GetController<NewObjectViewController>().Actions["New"].Enabled.SetItemValue("Disable", false);
                //Frame.GetController<LinkUnlinkController>().Actions["Link"].Enabled.SetItemValue("Disable", false);
                //Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", false);
                //Frame.GetController<WebModificationsController>().Actions["SwitchToEditMode"].Enabled.SetItemValue("Disable", false);
                //Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Enabled.SetItemValue("Disable", false);
                Frame.GetController<ResetPasswordController>().Actions["ResetPassword"].Enabled.SetItemValue("Disable", false);
                Frame.GetController<ChangePasswordController>().Actions["ChangePasswordByUser"].Enabled.SetItemValue("Disable", false);

            }
        }

        private void GetRoles(List<SecuritySystemRole> roles)
        {
            foreach (SecuritySystemRole role in roles)
            {
                rolesList.Add(role);
                if (role.ChildRoles.Count > 0)
                {
                    List<SecuritySystemRole> childRoles = new List<SecuritySystemRole>();
                    foreach (SecuritySystemRole childRole in role.ChildRoles)
                    {
                        if (!rolesList.Contains(childRole))
                        {
                            childRoles.Add(childRole);
                        }
                    }
                    if (childRoles.Count > 0)
                    {
                        GetRoles(childRoles);
                    }
                }
            }
        }
        protected override void OnViewControlsCreated()
        {
            base.OnViewControlsCreated();
            // Access and customize the target View control.
            if (View is DetailView)
            {
                if (user.Account.Id.ToUpper().Trim() != "ARIA")
                {
                    foreach (var item in ((DetailView)View).Items)
                    {
                        if (item is ASPxLookupPropertyEditor && item.Id == "Account")
                        {
                            ((ASPxLookupPropertyEditor)item).AllowEdit["CanEdit"] = false;
                        }
                    }
                }
            }
        }
       
        protected override void OnDeactivated()
        {
            // Unsubscribe from previously subscribed events and release other references and resources.
            base.OnDeactivated();
        }
    }
}
