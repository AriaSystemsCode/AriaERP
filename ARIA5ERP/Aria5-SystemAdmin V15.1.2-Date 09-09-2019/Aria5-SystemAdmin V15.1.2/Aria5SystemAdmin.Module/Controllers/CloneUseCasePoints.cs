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
    public partial class CloneUseCasePoints : ViewController
    {
        public CloneUseCasePoints()
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

        private void CloneThisTemplate_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
              if (this.View is ListView)
            {
                foreach (QAUseCasePoints UCP in this.View.SelectedObjects)
                {
                    QAUseCasePoints NewTestRun = new QAUseCasePoints(UCP.Session, UCP);
                    NewTestRun.Save();
                    NewTestRun.Session.CommitTransaction();
                    this.View.Refresh();
                }
            }
            else if (this.View is DetailView)
            {
                if (this.View.CurrentObject != null && this.View.CurrentObject is QAUseCasePoints)
                {
                    QAUseCasePoints NewUseCasePoints = new QAUseCasePoints(((QAUseCasePoints)this.View.CurrentObject).Session, (QAUseCasePoints)this.View.CurrentObject);
                    NewUseCasePoints.Save();
                  //  NewUseCasePoints.Session.CommitTransaction();
                   ((QAUseCasePoints)this.View.CurrentObject).Session.CommitTransaction();
                    this.View.Refresh();
                }
            }
        }
        }
    }

