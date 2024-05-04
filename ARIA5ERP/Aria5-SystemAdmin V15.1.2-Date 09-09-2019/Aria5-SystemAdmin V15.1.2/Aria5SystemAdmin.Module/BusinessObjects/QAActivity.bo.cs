using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class QAActivity
    {
        
        protected override void OnSaving()
        {
            base.OnSaving();
          
            if (this.Session.IsNewObject(this))
             {
             this.QAWBS.EstCurCapIndTot += this.EstCurCapInd;
             this.QAWBS.EstTrgCapIndTot += this.EstTrgCapInd;
             this.QAWBS.AvgEstValTot += this.AvgEstVal;
             this.QAWBS.ResAvaHrsTot += this.ResAvaHrs;
             }
            //ATA 2/16/2017 enhance the resource share screen 
            if(Resource != null &&this.QAWBS != null&& this.QAWBS.UseCasePoints != null)
            {
                if (this._qAWBS.UseCasePoints.ResourceShares.FindIndex(x => x.ResourceName == this.Resource) <= 0)
                {
                    QAResourceShare share = new QAResourceShare(this.Session);
                    share.UseCasePoints = this.QAWBS.UseCasePoints;
                    share.ResourceName = this.Resource;
                    share.DepartmentName = this.Department;
                }
            }
            
        }

        protected override void OnDeleting()
        {
            base.OnDeleting();
            this.QAWBS.EstCurCapIndTot -= this.EstCurCapInd;
            this.QAWBS.EstTrgCapIndTot -= this.EstTrgCapInd;
            this.QAWBS.AvgEstValTot -= this.AvgEstVal;
            this.QAWBS.ResAvaHrsTot -= this.ResAvaHrs;
        }
        

    }
}
