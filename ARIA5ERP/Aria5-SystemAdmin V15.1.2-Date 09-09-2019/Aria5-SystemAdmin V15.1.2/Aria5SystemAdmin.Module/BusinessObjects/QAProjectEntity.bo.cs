using System;
using System.Collections.Generic;
using System.Text;
using DevExpress.Xpo;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class QAProjectEntity
    {
        protected override void OnSaving()
        {

            if (TrackingNumber != null)
            {
                //  TrackingNumber.ProjectEntity = this;
                if (TrackingNumber.Entity != null)
                {
                    AriaObject = TrackingNumber.Entity;
                }
                if (TrackingNumber.Application != null)
                {
                    Application = TrackingNumber.Application;
                }
                if (this.TrackingNumber != null && this.TrackingNumber.ProjectTemplate == null && this.TrackingNumber.ProjectEntity == null)
                {
                    this.TrackingNumber.ProjectTemplate = this.ProjectTemplate;
                    this.TrackingNumber.ProjectEntity = this;
                    base.OnSaving();
                }
                else if ((this.TrackingNumber != null && this.TrackingNumber.ProjectTemplate == this.ProjectTemplate && this.TrackingNumber.ProjectEntity == null) || (this.TrackingNumber != null && this.TrackingNumber.ProjectTemplate == this.ProjectTemplate && this.TrackingNumber.ProjectEntity.Equals(this)))
                {
                    this.TrackingNumber.ProjectEntity = this;
                    base.OnSaving();
                }
                else if ((this.TrackingNumber != null && this.TrackingNumber.ProjectTemplate == this.ProjectTemplate && this.TrackingNumber.ProjectEntity != null))
                {
                    //DevExpress.XtraEditors.XtraMessageBox.Show(" This Tracking Entry already linked with this project Entity '" + this.TrackingNumber.ProjectEntity.Name.ToString() + "' on the same project please check");
                    throw new Exception("Can't save please modifiy tracking entry cause it linked with this project Entity '" + this.TrackingNumber.ProjectEntity.Name.ToString() + "'");
                }
                else if (this.TrackingNumber != null && this.TrackingNumber.ProjectTemplate != this.ProjectTemplate && this.TrackingNumber.ProjectEntity != null)
                {
                    //    DevExpress.XtraEditors.XtraMessageBox.Show(" This Tracking Entry already linked on this project'" + this.TrackingNumber.ProjectTemplate.Name.ToString() + "' and project entity'" + this.TrackingNumber.ProjectEntity.Name.ToString() + "'");
                    throw new Exception("Can't save please modifiy tracking entry cause it linked with this project '" + this.TrackingNumber.ProjectTemplate.Name.ToString() + "' and project entity'" + this.TrackingNumber.ProjectEntity.Name.ToString() + "'");

                }
                else
                {
                    //  DevExpress.XtraEditors.XtraMessageBox.Show(" This Tracking Entry already linked on this project '" + this.TrackingNumber.ProjectTemplate.Name.ToString() + "' please check");
                    throw new Exception("Can't save please modifiy tracking entry cause it linked with this project '" + this.TrackingNumber.ProjectTemplate.Name.ToString() + "' ");
                }
            }
            else 
            {
                if (this.ProjectTemplate != null && this.ProjectTemplate.Type != ProjectTemplate.ProjectType.CustomNonSoftware)
                {
                    throw new Exception("Can't save Project Entity for this Project without tracking number");
                }
            }
            this.TotalEstimate = DetailDesignTime + ProgrammingTime;

                //ATA modify project template alarm messsage and time [start]
            if (this.ProjectTemplate != null)
            {
                if (this.ApproveStatus == ApproveStatu.Ready)
                {
                    if (this.MainFeatures.Count == 0 || this.QAUseCases.Count == 0)
                    {
                        string ex = "can't save this project entity as ready without ";
                        if (this.MainFeatures.Count == 0)
                        {
                            ex += " Main feature under this tracking '" + this.TrackingNumber.ID + "' & ";
                           
                        }
                        if (this.QAUseCases.Count == 0)
                        {
                            ex += " Use Cases";
                        }
                        throw new Exception(ex);
                    }
                }
                this.Session.Save(this.ProjectTemplate);
            }
            //ATA modify project template alarm messsage and time [start]
            base.Save();
            
        }
        //ATA 20/7/2016 [start]
        protected override void OnDeleting()
        {
            if (this.ProjectTemplate.Type == ProjectTemplate.ProjectType.Key)
            {
                if (DateTime.Today > CalcEndate.CalcEnddate(this.ProjectTemplate.StartDate, 2))
                {
                    base.OnDeleting();
                }
                else if (DateTime.Today >= CalcEndate.CalcEnddate(this.ProjectTemplate.StartDate, 2).Subtract(new TimeSpan(10, 0, 0, 0)))
                {
                    if (this.ProjectTemplate.ProjectEntities.Count == 1)
                    {

                        this.Session.Save(this.ProjectTemplate);
                        base.OnDeleting();
                    }
                }
            }
            else
            {
                base.OnDeleting();
            }
           
        }
        //ATA 
       
    }
}
