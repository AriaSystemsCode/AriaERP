using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class QAUseCaseSteps
    {
        protected override void OnSaving()
        {
 	         base.OnSaving();
             //if (this.UseCaseFlow != null)
             //{
             //    int x = this.UseCaseFlow.QAUseCaseSteps.Count;
             //    Number = (short)(x + 1);
             //}
        }
       
    }
}
