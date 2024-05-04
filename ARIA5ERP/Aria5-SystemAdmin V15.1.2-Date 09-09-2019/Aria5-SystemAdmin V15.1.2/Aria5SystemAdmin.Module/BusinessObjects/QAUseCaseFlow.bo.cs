using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class QAUseCaseFlow
    {
        protected override void OnDeleting()
        {
            if (Session.CollectReferencingObjects(this).Count > 0)
            {
                throw new Exception("Can not Delete Use case Flow!");
            }
            else
            {
                base.OnDeleting();
            }

        }
    }
}
