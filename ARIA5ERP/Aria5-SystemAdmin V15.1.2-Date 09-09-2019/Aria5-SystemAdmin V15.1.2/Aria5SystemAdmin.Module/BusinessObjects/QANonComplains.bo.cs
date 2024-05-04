using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class QANonComplains
    {
        protected override void OnSaving()
        {
            alarmTime = DateTime.Now;
           // NotificationMessage = string.Format("Kindly note that you have Nc related to this project '{0}' related to this activity '{1}'", RelatedProject.Name,NonComplainActivity.ActivityName);
            base.OnSaving();
        }
    }
}
