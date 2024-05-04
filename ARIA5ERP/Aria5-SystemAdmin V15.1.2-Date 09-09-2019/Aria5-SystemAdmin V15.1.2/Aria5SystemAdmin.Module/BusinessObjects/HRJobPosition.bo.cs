using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class HRJobPosition
    {
        protected override void OnSaving()
        {
            if (this.Session.IsNewObject(this))
            {
                
                    this._master = true;
                    this._revision = 0;
               
            }
            this.EnteredDate = DateTime.Now.Date;
            if (this.CurrenRevision != null)
            {
                this.Name = CurrenRevision.Name;
                this.RevisionNumber = CurrenRevision.RevisionNumber + 1;
                this.ReportTo1 = CurrenRevision.ReportTo1;
                this.ReportTo2 = CurrenRevision.ReportTo2;
                this.BackUp = CurrenRevision.BackUp;
                this.Department = CurrenRevision.Department;
                this._sat = CurrenRevision.Sat; this._sun = CurrenRevision.Sun; this._mon = CurrenRevision.Mon; this._tue = CurrenRevision.Tue;
                this._wed = CurrenRevision.Wed; this._Thu = CurrenRevision.Thu; this._fri = CurrenRevision.Fri;
                this._Workinghoursfrom = CurrenRevision.WorkingHoursFrom; this.WorkingHoursTo = CurrenRevision.WorkingHoursTo;
                this.SalaryFrom = CurrenRevision.SalaryFrom; this.SalaryTo = CurrenRevision.SalaryTo;
                this._jobdescription = CurrenRevision.JobDescription;
            }
            //else
            //{
            //    HRJobPosition clonedobj = new HRJobPosition(this.Session,this);
            //    //clonedobj = (HRJobPosition)this.MemberwiseClone();
            //    //clonedobj.ParentJobPosition = this;
            //    clonedobj.Save();
            //}
            base.OnSaving();
        }

        protected override void OnDeleting()
        {
            if (this.Employees.Count > 0)
            {
                throw new Exception("Can't delete this jobposition because there are some employees already linked to it");
            }
            if (this.Revisions.Count > 0)
            {
                for (int i = 0; i < this.Revisions.Count; i++)
                {
                    this.Revisions[i].Delete();
                }
            }
            base.OnDeleting();
        }
    }
}
