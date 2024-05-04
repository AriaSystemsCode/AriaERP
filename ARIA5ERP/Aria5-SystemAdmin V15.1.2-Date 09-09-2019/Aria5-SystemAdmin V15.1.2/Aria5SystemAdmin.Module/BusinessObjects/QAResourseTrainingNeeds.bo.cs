using DevExpress.Data.Filtering;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class QAResourseTrainingNeeds
    {

        protected override void OnSaving()
        {
            base.OnSaving();
            ////ATA
            //if (ResourceShare.ResourceName != null)
            //{
            //    Resources = ResourceShare.ResourceName;
            //}
            ////ATA
            //if (this.TrainingStatus == Trainingstatus.Completed)// && Resources.CompletedTraining.)
            //{
            // XPCollection<   QAResourseTrainingNeeds> x = Resources.CompletedTraining;
            // x.Filter = new BinaryOperator("Oid", Oid, BinaryOperatorType.Equal);
            //    if (x.Count==0)
            //    {
            //        Resources.CompletedTraining.Add(this);  
            //    }

            //}
          

        }
    }
}
