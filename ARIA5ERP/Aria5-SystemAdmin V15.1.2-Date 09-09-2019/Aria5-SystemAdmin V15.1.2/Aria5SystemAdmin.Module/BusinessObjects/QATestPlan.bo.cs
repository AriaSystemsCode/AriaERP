using DevExpress.ExpressApp.ConditionalAppearance;
using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    [Appearance("TrackingDisabling", AppearanceItemType = "ViewItem", Context = "DetailView", TargetItems = "TrackingEntry", Criteria =("TrackingEntry is not null"), Enabled = false)]
    public partial class QATestPlan
    {
        //protected override void OnLoaded()
        //{
        //    base.OnLoaded();
        //    try
        //    {
        //        ProjectEntities.AddRange(ProjectTemplate.ProjectEntities);
        //        Environments.AddRange(ProjectTemplate.Environments);

        //        this.Save();
        //        this.Session.CommitTransaction();
        //    }
        //    catch (Exception)
        //    {
                
        //        throw;
        //    }

        //}
        public override void AfterConstruction()
        {
            base.AfterConstruction();
             try
             {
                 ProjectEntities.AddRange(ProjectTemplate.ProjectEntities);
                 Environments.AddRange(ProjectTemplate.Environments);

                 this.Save();
                 this.Session.CommitTransaction();
             }
             catch (Exception)
             {

                // throw;
             }
        }

        protected override void OnSaving()
        {
            if (!PerformanceTesting && !PortabilityTesting && !FunctionTesting && !UsabilityTesting)
            {
                throw new Exception("you can't save test plane without select one type at least");
            }
            if (!SystemTest && !ComponentTest && !AcceptanceTest && !IntegrationTest)
            {
                throw new Exception("you can't save test plane without select one Level at least");
            }
            base.OnSaving();
        }
    }
}
