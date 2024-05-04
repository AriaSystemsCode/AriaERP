using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class TeamLeaderDailyFollowUp
    {
        protected override void OnSaving()
        {
            Date = DateTime.Now;
            base.OnSaving();
        }
    }
}
