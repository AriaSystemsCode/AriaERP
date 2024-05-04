using Aria5SystemAdmin.Module.Controllers;
using DevExpress.ExpressApp.Utils;
using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class ApplicationBuild_T
    {
        public override void AfterConstruction()
        {
            base.AfterConstruction();

            IsNew = Session.IsNewObject(this);

            if (IsNew)
            {
                Status = ApplicationBuildStatus.Open;
                IssueDate = DateTime.Today;
               
               
            }
        }
        protected override void OnSaving()
        {
           
            if (Session.IsNewObject(this))
            {
                if (Application_T != null)
                {
                    ApplicationBuildId = (int.Parse(Application_T.BuildNo) + 1).ToString();
                    //MMT
                    //Application_T.BuildNo = (int.Parse(Application_T.BuildNo) + 1).ToString();
                    //Application_T.Save();
                    //MMT
                }
               // try
                {
                    foreach (ApplicationBuild_T item in Application_T.ApplicationBuild_Ts)
                    {
                        if (item.Oid != Oid && item.Status != ApplicationBuildStatus.Complete)
                        {
                            //throw new Exception(string.Format(CaptionHelper.GetLocalizedText("Messages", "CannnotBuild"), Application_T.Name));
                            throw new Exception("Cannot create new build while Application:"+ Application_T.Name+" still has uncompleted builds");
                        }
                    }
                }
                //catch (Exception)
                {
                    
                   // throw;
                }

               

            }

        }
    }
}
