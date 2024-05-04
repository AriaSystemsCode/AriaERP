using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class HRActivity
    {

        protected override void OnDeleting()
        {
            if (UserActivityList.Count > 0)
            {
                throw new Exception("Can't delete this Activity because it has instances");
            }
            base.OnDeleting();
        }
    }
}
