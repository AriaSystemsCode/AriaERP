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
    public partial class QACreateIssue : ViewController
    {
        public QACreateIssue()
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

        private void AddIssueAction_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            // Ras 8-11-2015 create issue from the risk and change risk status to in work [begin]
            if (this.View.CurrentObject!=null && this.View.CurrentObject is QARisk &&(((QARisk)(this.View.CurrentObject)).Status == QARisk.status.Open || ((QARisk)(this.View.CurrentObject)).Status == QARisk.status.InWork) )
            {
                ((QARisk)(this.View.CurrentObject)).Status = QARisk.status.InWork;
                ((QARisk)(this.View.CurrentObject)).Save();
                QAManagementIssues MI = new QAManagementIssues(((QARisk)(this.View.CurrentObject)).Session);
                MI.Description = ((QARisk)(this.View.CurrentObject)).Description;
                MI.Resolution = ((QARisk)(this.View.CurrentObject)).Contingency;
                MI.ProjectTemplate = ((QARisk)(this.View.CurrentObject)).ProjectTemplate;
                MI.IssueType = (QAManagementIssues.IssueTypes)((QARisk)(this.View.CurrentObject)).RiskCategory;
                
                MI.RiskRefrence = ((QARisk)(this.View.CurrentObject));
                int Priority = ((QARisk)(this.View.CurrentObject)).Impact;
                if (Priority == 1) MI.Priority = QAManagementIssues.Priorities.Low;
                if (Priority == 2) MI.Priority = QAManagementIssues.Priorities.medium;
                if (Priority == 3) MI.Priority = QAManagementIssues.Priorities.High;
                int counter = 1;
                string RiskName=((QARisk)(this.View.CurrentObject)).Name;  ;
                string IssueName="";
                if (((QARisk)(this.View.CurrentObject)).Session.FindObject<QAManagementIssues>(CriteriaOperator.Parse("Issue='" + RiskName + "'"))==null)
                    IssueName = RiskName;
                else
                {
                    do
                    {
                        IssueName = RiskName + "_" + counter.ToString();
                        counter++;
                    } while (((QARisk)(this.View.CurrentObject)).Session.FindObject<QAManagementIssues>(CriteriaOperator.Parse("Issue='" + IssueName + "'")) != null);
                    
                }
                MI.Issue = IssueName;
                MI.Save();
                ((QARisk)(this.View.CurrentObject)).Session.CommitTransaction();
                ((QARisk)(this.View.CurrentObject)).Reload();
            }
            if (this.View.CurrentObject!=null && this.View.CurrentObject is QARisk &&(((QARisk)(this.View.CurrentObject)).Status == QARisk.status.Closed || ((QARisk)(this.View.CurrentObject)).Status == QARisk.status.Retired) )
            {
                DevExpress.XtraEditors.XtraMessageBox.Show("Can not create issue for Closed or Retired Risk.");
            }
            // Ras 8-11-2015 create issue from the risk and change risk status to in work [end]
        }
    }
}
