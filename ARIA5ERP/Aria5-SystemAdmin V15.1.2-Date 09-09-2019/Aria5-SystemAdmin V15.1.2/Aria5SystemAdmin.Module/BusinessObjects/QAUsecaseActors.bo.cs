using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class QAUsecaseActors
    {
       protected override void OnSaving()
       {
        //    base.OnSaving();
        //    if (this.Session.IsNewObject(this))
        //    {
        //        this.UseCasePoints.TotalUsecaseActorsPoints += this.UAW;
        //    }
            base.OnSaving();
            if (!this.Session.IsNewObject(this))
            {
                if (this.UseCasePoints != null )//&& this.UseCasePoints.ProjectEfforts > 0)
                {
                  //  ATA 6/6/2016 to solve issue in chnage usecasepoints project effort not reflect in wbs [start]
                    this.UseCasePoints.TotalUsecaseActorsPoints += this._UAW;
                    //foreach (QAWBS phase in this.UseCasePoints.WBS)
                    //{
                    //   // phase.ProjEfrt = this.UseCasePoints.ProjectEfforts;
                    //}

                    //ATA 6/6/2016 to solve issue in chnage usecasepoints project effort not reflect in wbs [end]
                }
            }
      }
    }
}
