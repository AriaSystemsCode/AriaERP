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
using DevExpress.Xpo;


namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class CloneTestCase : ViewController
    {
        public CloneTestCase()
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

        private void SelectTestCases_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {
            //ATA 4/7/2017 Fix the issue of this object was modified by another user  [start] 
            foreach (object obj in this.View.ObjectSpace.ModifiedObjects)
            {
                DevExpress.Persistent.BaseImpl.BaseObject obj1 = obj as DevExpress.Persistent.BaseImpl.BaseObject;
                if (obj1 != null)
                {
                    if (this.View.CurrentObject != obj1)
                        obj1.Reload();
                }

            }
            //ATA 4/7/2017 Fix the issue of this object was modified by another user  [End]  
                   List<TestCase> T = new List<TestCase>();
            foreach (TestCase selectedTestCase in e.PopupWindow.View.SelectedObjects)
                {
                // sara.N ,1 Can't persist the object 14-03-2016 [start]
                TestCase ClonedTestCase = new TestCase(selectedTestCase.Session,selectedTestCase);                        
                ClonedTestCase.Save();
                T.Add(ClonedTestCase);
                selectedTestCase.Session.CommitTransaction();               
                }
                 IObjectSpace objectSpace = Application.CreateObjectSpace();
                 TestRun Test = objectSpace.FindObject<TestRun>(CriteriaOperator.Parse("[Oid] = '" + ((TestRun)(this.View.CurrentObject)).Oid + "'"));
                 foreach (TestCase TC in T)
                 {
                 TestCase x = objectSpace.FindObject<TestCase>(CriteriaOperator.Parse("[Oid] = '" + TC.Oid + "'"));
                 Test.TestCases.Add(x);
                 Test.Save();
                 }

                 ((TestRun)(this.View.CurrentObject)).Reload();
                 this.View.Refresh();
                 objectSpace.CommitChanges();
              	           
        }

        private void SelectTestCases_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();
            CollectionSource newCollectionSource = new CollectionSource(objectSpace, typeof(TestCase));

            newCollectionSource.Criteria.Add("MyCriteria1", CriteriaOperator.Parse("IsOriginal=True"));
         //  newCollectionSource.Criteria.Add("MyCriteria2", CriteriaOperator.Parse("ProjectTemplate='" + ((TestRun)(this.View.CurrentObject)).ProjectTemplate+ "'"));

            newCollectionSource.Reload();
            e.View = Application.CreateListView(Application.FindListViewId(typeof(TestCase)),   newCollectionSource, false); 
             
        }

        
    }
}
