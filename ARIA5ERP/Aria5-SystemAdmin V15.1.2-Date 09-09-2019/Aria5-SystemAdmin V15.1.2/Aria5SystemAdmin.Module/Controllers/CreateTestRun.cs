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
using  DevExpress.Xpo;
using DevExpress.ExpressApp.Security;

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class CreateTestRun : ViewController
    {
       
        public CreateTestRun()
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

        private void CreateTestTun_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            if (this.View.CurrentObject!=null && this.View.CurrentObject is ProjectTemplate)
            {
                if (((ProjectTemplate)this.View.CurrentObject).TestCases.Count>0)
                {
                    string userName = ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters).UserName;
                    TestRun NewTestRun = new TestRun(((ProjectTemplate)this.View.CurrentObject).Session);
                    NewTestRun.Name = "TR- " + ((ProjectTemplate)this.View.CurrentObject).Name + "- " + userName + "- " + DateTime.Now.ToString();
                  

                    NewTestRun.TestCases.AddRange(((ProjectTemplate)this.View.CurrentObject).TestCases);
                    NewTestRun.Save();
                    ((ProjectTemplate)this.View.CurrentObject).Session.CommitTransaction();
                    this.View.Refresh();
                    ((ProjectTemplate)this.View.CurrentObject).TestRuns.Add(NewTestRun);
                    ((ProjectTemplate)this.View.CurrentObject).Save();
                 
                    ((ProjectTemplate)this.View.CurrentObject).Session.CommitTransaction();
                    this.View.Refresh();
                }
               
            }
        }



        private void AssignedResource_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {

            IObjectSpace objectSpace = Application.CreateObjectSpace();
            CriteriaOperator criteriAccount = CriteriaOperator.Parse("Id = 'Aria'");
            Account accountAria = objectSpace.FindObject<Account>(criteriAccount);
            CollectionSource newCollectionSource = new CollectionSource(objectSpace, typeof(AriaSecuritySystemUser));

            newCollectionSource.Criteria.Add("MyCriteria1", CriteriaOperator.Parse("Account='" + accountAria.Oid + "'"));
            e.View = Application.CreateListView(Application.FindListViewId(typeof(AriaSecuritySystemUser)), newCollectionSource, true);
         
        }
                
        

        private void AssignedResource_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {
            // sara.N ,1 Create Test Run For Integration testing Include Non Original test cases [Start]
            IObjectSpace objectSpace = Application.CreateObjectSpace();
            ProjectTemplate Current = ((ProjectTemplate)this.View.CurrentObject);
            foreach (AriaSecuritySystemUser testRunAssignedUser in e.PopupWindow.View.SelectedObjects)
            {
                if (!string.IsNullOrEmpty(testRunAssignedUser.UserName))
                {
                    TestRun NewTestRun = new TestRun(testRunAssignedUser.Session);
                    ProjectTemplate c = NewTestRun.Session.FindObject<ProjectTemplate>(CriteriaOperator.Parse("Oid='" + Current.Oid + "'"));
                    
                    NewTestRun.AssignedUser = testRunAssignedUser;
                    NewTestRun.Name = "TR- " + c.Name + "- " + testRunAssignedUser.UserName.ToString() + "- " + DateTime.Now.ToString();
                    NewTestRun.Application = c.Application;            
                    // sara.N ,1 Create Test Run For Integration testing Include Non Original test cases [End]
                    foreach (TestCase  item in c.TestCases.Where (r=> r.IsOriginal==true))
                    {
                        NewTestRun.TestCases.Add(item);
                    }
                    NewTestRun.Save();
                    NewTestRun.Session.CommitTransaction();
                    c.TestRuns.Add(NewTestRun);
                    c.Save();
                    c.Session.CommitTransaction();
                    
                    this.View.Refresh();
                }
                
            }  
        }

        private void CreateTestRunbasedontestcases_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            IObjectSpace ospace = Application.CreateObjectSpace();
            TestRun newtestrun = ospace.CreateObject<TestRun>();
            foreach (var item in this.View.SelectedObjects)
            {
                TestCase selectedtestcase = ospace.FindObject<TestCase>(CriteriaOperator.Parse("[Oid]= '" + ((TestCase)item).Oid + "'"));
                TestCase copytestcase = new TestCase(newtestrun.Session, selectedtestcase);


                newtestrun.TestCases.Add(copytestcase);

            }
            newtestrun.Save();
            DetailView DV = Application.CreateDetailView(ospace, newtestrun);
            DV.ViewEditMode = DevExpress.ExpressApp.Editors.ViewEditMode.Edit;
            Application.ShowViewStrategy.ShowViewInPopupWindow(DV);
        }

        private void CreateTestcasebasedonsteps_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            IObjectSpace ospace = Application.CreateObjectSpace();
            TestCase newtestcase = ospace.CreateObject<TestCase>();
            foreach (var item in this.View.SelectedObjects)
            {
                TestCaseSteps selectedtestcasestep = ospace.FindObject<TestCaseSteps>(CriteriaOperator.Parse("[Oid]= '" + ((TestCaseSteps)item).Oid + "'"));
                TestCaseSteps copytestcasestep = new TestCaseSteps(newtestcase.Session, selectedtestcasestep);


                newtestcase.TestCaseSteps.Add(copytestcasestep);

            }
            newtestcase.Save();
            DetailView DV = Application.CreateDetailView(ospace, newtestcase);
            DV.ViewEditMode = DevExpress.ExpressApp.Editors.ViewEditMode.Edit;
            Application.ShowViewStrategy.ShowViewInPopupWindow(DV);
        }

        private void TestCaseStepsPassed_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            foreach (var Step in e.SelectedObjects)
            {
                if (Step is TestCaseSteps)
                {
                    ((TestCaseSteps)Step).Passed = true;
                    ((TestCaseSteps)Step).Save();
                    
                }
            }
            if (e.SelectedObjects.Count > 0)
            {
                ((TestCaseSteps)e.SelectedObjects[0]).Session.CommitTransaction();
            }
        }
      }
    }

