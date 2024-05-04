using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Actions;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Layout;
using DevExpress.ExpressApp.Model.NodeGenerators;
using DevExpress.ExpressApp.SystemModule;
using DevExpress.ExpressApp.Templates;
using DevExpress.ExpressApp.Utils;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.Validation;
using Aria5SystemAdmin.Module.BusinessObjects;

namespace Aria5SystemAdmin.Module.Controllers
{
    //ATA add new contoller to control the validation proccess 
    // For more typical usage scenarios, be sure to check out https://documentation.devexpress.com/eXpressAppFramework/clsDevExpressExpressAppViewControllertopic.aspx.
    public partial class ValidationController : ViewController
    {
        PersistenceValidationController validator;
        public ValidationController()
        {
            InitializeComponent();
            // Target required Views (via the TargetXXX properties) and create their Actions.
        }
        protected override void OnActivated()
        {
            validator = Frame.GetController<PersistenceValidationController>();
            if (validator != null)
            {
                validator.NeedToValidateObject += validator_NeedToValidateObject;
            }
            base.OnActivated();
            // Perform various tasks depending on the target View.
        }

        void validator_NeedToValidateObject(object sender, NeedToValidateObjectEventArgs e)
        {
            if ((e.CurrentObject.GetType() == typeof(HREmployee) && View.ObjectTypeInfo.Type != typeof(HREmployee)) || (e.CurrentObject.GetType() == typeof(Account) && View.ObjectTypeInfo.Type != typeof(Account)))
            {
                e.NeedToValidate = false;
            }
        }
        protected override void OnViewControlsCreated()
        {
           base.OnViewControlsCreated();
            // Access and customize the target View control.
        }
        protected override void OnDeactivated()
        {
            if (validator != null)
            {
                validator.NeedToValidateObject -= validator_NeedToValidateObject;
            }
            // Unsubscribe from previously subscribed events and release other references and resources.
            base.OnDeactivated();
        }
    }
}
