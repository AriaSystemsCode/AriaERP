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

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class EntityOperationPermissionController : ViewController
    {
        public EntityOperationPermissionController()
        {
            InitializeComponent();
            RegisterActions(components);
            // Target required Views (via the TargetXXX properties) and create their Actions.
        }
        protected override void OnActivated()
        {
            base.OnActivated();
            // Perform various tasks depending on the target View.
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

        private void NewPermission_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();
            EntityOperationPermissionGenerator permissionGenerator = objectSpace.CreateObject<EntityOperationPermissionGenerator>();
            this.SetView(Application.CreateDetailView(objectSpace, permissionGenerator, true));

            //IObjectSpace objectSpace = Application.CreateObjectSpace();
            //EntityOperationPermissionGenerator permissionGenerator = objectSpace.CreateObject<EntityOperationPermissionGenerator>();
            ////(View as ListView) = Application.CreateDetailView(objectSpace, permissionGenerator, true);
            ////this.View = Application.CreateDetailView(objectSpace, permissionGenerator, true);
            //this.View.AllowEdit["CanEditPermissionGenerator"] = true;
        }
    }
}
