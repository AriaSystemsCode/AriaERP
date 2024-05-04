using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class EntityClassification
    {
        protected override void OnDeleting()
        {
            //ATA , 2, can't delete account issue [start]
            if (Entities.Count > 0)
            {
                throw new Exception(" can't delete classification cause of there are entities related to it ");
            }
            else
            {
                base.OnDeleting();
            }
            //ATA , 2, can't delete account issue [end]
          
        }
    }
}
