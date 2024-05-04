using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class Attachment
    {
        // load the attachment
        protected override void OnLoaded()
        {
            base.OnLoaded();
            // Wael
            // The fiel name is the same OID for the current record
            // 1) Compare the attachment file with the name as Attachment.Name located in the Attachment.Location with 
            // the attachmanet file located in the folder Attachmanets and has a name same as the field Attachment.ID for the current record
            // If they are different, copy the attachment file Attachment.Location\Attachment.Name to the Attacments folder with a name same as Attachment.ID
            // 2) Use the method Attachment.LoadFromStream() to load the attachment in the Attachments folder
  

        }

        protected override void OnSaving()
        {
            base.OnSaving();
            // Wael
            // 1) Save the original path of the attachment in the field Attachment.Location 
            // 2) Save the name of the attachemnt in the fied Attachment.Name
            // 3) Copy the attachment file from the original path to the folder Attachments and give it a name same as Attachment.ID
        }
    }
}
