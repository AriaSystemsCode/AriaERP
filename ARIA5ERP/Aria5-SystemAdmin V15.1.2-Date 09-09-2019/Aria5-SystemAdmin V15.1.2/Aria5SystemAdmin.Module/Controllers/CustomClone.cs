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
using DevExpress.Xpo.Metadata;
using DevExpress.Xpo;
using DevExpress.ExpressApp.CloneObject;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.ExpressApp.Security;

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class CustomClone : ViewController
    {
        public CustomClone()
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


        private void CopyTestRun_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            if (this.View is ListView)
            {
                foreach (TestRun TR in this.View.SelectedObjects)
                {
                    TestRun NewTestRun = new TestRun(TR.Session, TR);
                    NewTestRun.Save();
                    NewTestRun.Session.CommitTransaction();
                    this.View.Refresh();
                }
            }
            else if (this.View is DetailView)
            {
                if (this.View.CurrentObject != null && this.View.CurrentObject is TestRun)
                {
                    TestRun NewTestRun = new TestRun(((TestRun)this.View.CurrentObject).Session, (TestRun)this.View.CurrentObject);
                    NewTestRun.Save();
                    ((TestRun)this.View.CurrentObject).Session.CommitTransaction();
                    //ATA open the copied test run in edit mode start 6/14/2017[start]
                    this.View.Refresh();
                    IObjectSpace objectSpace = Application.CreateObjectSpace(typeof(TestRun));
                    TestRun copiedtestrun = objectSpace.FindObject<TestRun>(CriteriaOperator.Parse("Oid = '" + NewTestRun.Oid + "'"));
                    DetailView DV = Application.CreateDetailView(objectSpace, copiedtestrun); 
                    DV.ViewEditMode = DevExpress.ExpressApp.Editors.ViewEditMode.Edit;
                    e.ShowViewParameters.CreatedView = DV;
                    //ATA open the copied test run in edit mode start 6/14/2017[End]

                }
            }
        }

        private void CloneUseCasePoints_Execute(object sender, SimpleActionExecuteEventArgs e)
        {

        }
    }
}


