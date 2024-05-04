using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class QAWBS
    {
        
       [DevExpress.Persistent.Base.Action]
        public void ReCalculateTotalMonthEstimation()
        {
            this.EstCurCapIndTot = 0;
            this.EstTrgCapIndTot = 0;
            this.AvgEstValTot = 0;
            this.ResAvaHrsTot = 0;
            foreach (QAActivity QAA in this.QAActivities)
            {
                this.EstCurCapIndTot += QAA.EstCurCapInd;
                this.EstTrgCapIndTot += QAA.EstTrgCapInd;
                this.AvgEstValTot += QAA.AvgEstVal;
                this.ResAvaHrsTot += QAA.ResAvaHrs;
            }
        }

       public override void AfterConstruction()
       {
           // base.AfterConstruction();
           if (this.UseCasePoints != null && this.UseCasePoints.ProjectEfforts > 0)
           {
               //ProjEfrt = this.UseCasePoints.ProjectEfforts;
           }
            //5-01-2016 Create WBS Issue [Start]
            //ATA
            //if (this.UseCasePoints!=null)
            // {
            //        ProjEfrt = this.UseCasePoints.ProjectEfforts;
            // }
            //   ProjEfrt = this.UseCasePoints.ProjectEfforts;
            //5-01-2016 Create WBS Issue [End]

            //Doaa 05/08/2019
           // QAActivities = new DevExpress.Xpo.XPCollection<QAActivity>();
            //Doaa 05/08/2019


        }


    }
}
