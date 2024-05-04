using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class HRSkillsCategory
    {
        protected override void OnDeleting()
        {
            if (this.ListOfSkills.Count > 0)
            {
                throw new Exception("Can't Delete this category because it is linked to a list of skills");
            }
            base.OnDeleting();
        }
    }
}
