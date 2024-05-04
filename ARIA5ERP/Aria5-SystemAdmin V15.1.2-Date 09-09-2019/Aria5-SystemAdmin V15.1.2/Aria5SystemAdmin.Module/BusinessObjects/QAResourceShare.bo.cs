using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class QAResourceShare
    {

        protected override void OnSaving()
        {
            if (this.UseCasePoints != null)
            {
                this.ProjectTemplate = this.UseCasePoints.Project;
                if (this.ProjectTemplate != null)
                {
                    this.Application = this.ProjectTemplate.Application;
                }
            }
            base.OnSaving();
        }
    }
}
