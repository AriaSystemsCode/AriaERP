using Aria5SystemAdmin.Module;
using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5.DevExpress.OneTouchAway.Module.BusinessObjects
{
    [IsClient(true, true)]
    public partial class ClientDataAudit
    {
        public override void AfterConstruction()
        {
            base.AfterConstruction();

            Oguid = Guid.NewGuid();
        }
    }
}
