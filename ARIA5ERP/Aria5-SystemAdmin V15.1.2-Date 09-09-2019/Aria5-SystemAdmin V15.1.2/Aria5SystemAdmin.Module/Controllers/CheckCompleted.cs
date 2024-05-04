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
    public partial class CheckCompleted : ViewController
    {
        public CheckCompleted()
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

        private void check_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            if (this.View.CurrentObject != null && View.CurrentObject is ProjectTemplate)
            {
                if (((ProjectTemplate)View.CurrentObject).Oid != null)
                {
                    
                    for (int i = 0; i < ((ProjectTemplate)View.CurrentObject).Defects.Count; i++)
                    {
                        if (((ProjectTemplate)View.CurrentObject).Defects[i].Status != QADefect.DefectStatus.Completed)
                        {
                         //  ((ProjectTemplate)View.CurrentObject). Completness = 0;
                        }
                        else
                        {
                            ((ProjectTemplate)View.CurrentObject).Completness++;
                        }
                    }
                    ((ProjectTemplate)View.CurrentObject).Completness = (((ProjectTemplate)View.CurrentObject).Completness / ((ProjectTemplate)View.CurrentObject).Defects.Count)*100;

                }
            }
        }
        public void checkDefects()
        {

        }


    }
}
