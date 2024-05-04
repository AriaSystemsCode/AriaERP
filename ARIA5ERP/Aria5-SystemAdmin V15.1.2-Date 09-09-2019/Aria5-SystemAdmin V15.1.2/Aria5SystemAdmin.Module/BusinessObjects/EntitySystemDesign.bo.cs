using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class EntitySystemDesign
    {
        protected override void OnSaving()
        {
            if (Session.IsNewObject(this))
            {
                //ATA notification customization 17/7/2016 [start]
                //if (QAProjectEntity.AlarmTime != null && QAProjectEntity.MainFeatures.Count > 0)
                //{
                //    QAProjectEntity.AlarmTime = null;
                //    Session.Save(QAProjectEntity);
                //}
                //ATA notification customization 17/7/2016 [END]

            }
            base.OnSaving();
        }
        protected override void OnDeleting()
        {
            if (DateTime.Today > QAProjectEntity.Enddate)
            {
                base.OnDeleting();
            }
            else if (DateTime.Today >= QAProjectEntity.Enddate.Subtract(new TimeSpan(10, 0, 0, 0)))
            {
                if (QAProjectEntity.MainFeatures.Count == 1)
                {
                    //ProjectEntity.NotificationMessage = string.Format("you need to add this items before this date '{0}' for this project enity '{1}'", ProjectEntity.Enddate, ProjectEntity.Name);
                    //ProjectEntity.NotificationMessage += "  Main feature ";
                    //if (ProjectEntity.EntitySystemDesign.Count == 0)
                    //{
                    //    ProjectEntity.NotificationMessage += " Entity system design";
                    //}
                    //QAProjectEntity.AlarmTime = DateTime.Today;
                    //Session.Save(QAProjectEntity);
                    //base.OnDeleting();
                }
            }
        }
    }
}
