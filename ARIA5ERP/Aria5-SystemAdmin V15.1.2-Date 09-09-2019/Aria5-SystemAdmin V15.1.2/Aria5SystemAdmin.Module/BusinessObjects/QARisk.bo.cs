using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class QARisk
    {
        protected override void OnSaving()
        {
            if (this.Session.IsNewObject(this))
            {
                this.AddDate = DateTime.Now; 
                
            }
            base.OnSaving();
        }
    }
}
