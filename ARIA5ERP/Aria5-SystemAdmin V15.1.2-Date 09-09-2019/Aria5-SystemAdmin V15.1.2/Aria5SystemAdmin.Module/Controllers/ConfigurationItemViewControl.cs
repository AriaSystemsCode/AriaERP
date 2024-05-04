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
using Aria5SystemAdmin.Module.Managers;
using DevExpress.ExpressApp.Web.SystemModule;
using DevExpress.ExpressApp.Security;

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class ConfigurationItemViewControl : ViewController
    {

        public static Guid currenConfigurationItem;

        public static ConfigurationItem currentConfigurationItemObject;


        public ConfigurationItemViewControl()
        {
            InitializeComponent();
            RegisterActions(components);
            // Target required Views (via the TargetXXX properties) and create their Actions.
        }
        protected override void OnActivated()
        {
            base.OnActivated();
            // Perform various tasks depending on the target View.

            if (AriaSecuritySystemUser.CurrentAccount.Id.ToUpper().Trim() != "ARIA")
            {
                Frame.GetController<ModificationsController>().Actions["Save"].Enabled.SetItemValue("Disable", false);
                Frame.GetController<ModificationsController>().Actions["SaveAndClose"].Enabled.SetItemValue("Disable", false);
                Frame.GetController<ModificationsController>().Actions["SaveAndNew"].Enabled.SetItemValue("Disable", false);
                Frame.GetController<NewObjectViewController>().Actions["New"].Enabled.SetItemValue("Disable", false);
                Frame.GetController<LinkUnlinkController>().Actions["Link"].Enabled.SetItemValue("Disable", false);
                Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", false);
                Frame.GetController<WebModificationsController>().Actions["SwitchToEditMode"].Enabled.SetItemValue("Disable", false);
                Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Enabled.SetItemValue("Disable", false);
                Frame.GetController<ActivationKeyViewControl>().Actions["GenerateActivationKeyViewController"].Enabled.SetItemValue("Disable", false);
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

        private void simpleAction1_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            currentConfigurationItemObject = objectSpace.GetObjectByKey<ConfigurationItem>(currenConfigurationItem);
            ActivationManager activationKey = new ActivationManager();
            activationKey.GenerateNewActivationKey(currentConfigurationItemObject.Oid);

        }

        private void ConfigurationItemViewControl_ViewControlsCreated(object sender, EventArgs e)
        {
            if (this.View.CurrentObject != null && this.View.CurrentObject.GetType() == typeof(ConfigurationItem))
            {
                currenConfigurationItem = ((ConfigurationItem)this.View.CurrentObject).Oid;
            }
        }
    }
}
