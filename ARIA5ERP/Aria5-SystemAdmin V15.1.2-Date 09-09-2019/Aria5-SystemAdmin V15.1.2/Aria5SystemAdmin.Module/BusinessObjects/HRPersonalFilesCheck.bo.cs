using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class HRPersonalFilesCheck
    {
        protected override void OnSaving()
        {
            //if (this.Exist && this.Attachment == null)
            //{
            //    throw new Exception()
            //}
            if (Attachment != null && Attachment.EntityAttachments.Count == 0)
            {
                EntityAttachment x = new EntityAttachment(Session);
                x.Entity = this.Employee;
                this.Attachment.EntityAttachments.Add(x);
            }
            base.OnSaving();
        }
    }
}
