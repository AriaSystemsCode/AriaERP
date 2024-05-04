using System;
using System.Collections.Generic;
using System.Text;
using Aria5SystemAdmin.Module.Managers;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class HREmployee
    {

        protected override void OnSaving()
        {
           
            HRManager manager = new HRManager();
            //if (this.JobPosition != null)
            //{
            //    if (this.JobPosition.Master == true)
            //    {
                    
            //        this.JobPosition = manager.getlastrevision(this.JobPosition);
            //    }
            //}
         
            if (this.Employee_IMAGE != null)
            {
                this.Employee_IMAGE = manager.ResizeImage(this.Employee_IMAGE, 165, 200);
            }
            if (this.Session.IsNewObject(this))
            {
               // assignchecklist();
            }
            base.OnSaving();
            
        }
        [DevExpress.Persistent.Base.Action]
        public void addnewchecklist()
        {
            assignchecklist();
            this.Save();
        }
        public void assignchecklist()
        {
            if (this.FilesCheckList.Count == 0)
            {
                foreach (var item in Enum.GetValues(typeof(HRPersonalFilesCheck.FileName)))
                {

                    if (this.Gender == Gender.Female && item == "Militarilycertificate")
                    {
                        continue;
                    }
                    else
                    {
                        HRPersonalFilesCheck newfile = new HRPersonalFilesCheck(Session);
                        newfile.File = (HRPersonalFilesCheck.FileName)Enum.Parse(typeof(HRPersonalFilesCheck.FileName), item.ToString());
                        this.FilesCheckList.Add(newfile);
                    }
                }
            }
            
        }
    }
}
