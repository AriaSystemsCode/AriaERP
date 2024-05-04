using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class HR_ActivityCategory
    {
        protected override void OnDeleting()
        {
            if (Activities.Count > 0)
            {
                throw new Exception("Can't delete this category because there are some activities linked to it");
            }
            base.OnDeleting();
        }
    }
}
