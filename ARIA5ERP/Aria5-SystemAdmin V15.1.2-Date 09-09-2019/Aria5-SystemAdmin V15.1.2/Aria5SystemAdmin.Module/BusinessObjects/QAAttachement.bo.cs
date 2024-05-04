using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class QAAttachement
    {
        protected override void OnSaving()
        {
            this.AddDate = DateTime.Now;
            base.OnSaving();
        }
    }
}
