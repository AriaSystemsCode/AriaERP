using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Actions;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Layout;
using DevExpress.ExpressApp.Model.NodeGenerators;
using DevExpress.ExpressApp.SystemModule;
using DevExpress.ExpressApp.Templates;
using DevExpress.ExpressApp.Utils;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.Validation;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Xpo;
using DevExpress.ExpressApp.Security;
using System.Web;

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out https://documentation.devexpress.com/eXpressAppFramework/clsDevExpressExpressAppViewControllertopic.aspx.
    public partial class HRController : ViewController
    {
        public HRController()
        {
            InitializeComponent();
            // Target required Views (via the TargetXXX properties) and create their Actions.
        }
        protected override void OnActivated()
        {
            base.OnActivated();

            if (Application.MainWindow != null && Application.MainWindow.View is ListView && View is ListView && View.ObjectTypeInfo.Type == typeof(HR_ActivityLog))
            {
                string username = ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters).UserName;
                CriteriaOperator criteriaUser = CriteriaOperator.Parse("UserName = '" + username + "'");
                AriaSecuritySystemUser user = ObjectSpace.FindObject<AriaSecuritySystemUser>(criteriaUser);
                if (user != null && user.Employee != null)
                {
                    ((ListView)View).CollectionSource.Criteria["Filter"] = CriteriaOperator.Parse("[User]= '" + user.Employee.Oid + "' and [Status] = '" + ActivityStatus.Pending+ "'");

                }

            }
            if (Application.MainWindow != null && Application.MainWindow.View is ListView && View is ListView && View.ObjectTypeInfo.Type == typeof(HREmployee))
            {
                string username = ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters).UserName;
                CriteriaOperator criteriaUser = CriteriaOperator.Parse("UserName = '" + username + "'");
                AriaSecuritySystemUser user = ObjectSpace.FindObject<AriaSecuritySystemUser>(criteriaUser);
                if (user!=null && user.Roles.Where(x=>x.Name == "Managers").Count() > 0 && user.Employee != null)
                {
                    ((ListView)View).CollectionSource.Criteria["Filter"] = CriteriaOperator.Parse("[Department]= '" + user.Employee.Department.Oid + "'");
                }
            }
            if (Application.MainWindow != null && Application.MainWindow.View is ListView && View is ListView && View.ObjectTypeInfo.Type == typeof(HRJobVacancy))
            {
                string username = ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters).UserName;
                CriteriaOperator criteriaUser = CriteriaOperator.Parse("UserName = '" + username + "'");
                AriaSecuritySystemUser user = ObjectSpace.FindObject<AriaSecuritySystemUser>(criteriaUser);
                if (user != null && user.Roles.Where(x => x.Name == "Managers").Count() > 0 && user.Employee != null)
                {
                    ((ListView)View).CollectionSource.Criteria["Filter"] = CriteriaOperator.Parse("[Department]= '" + user.Employee.Department.Oid + "'");
                }
            }
            //if (View is DetailView && View.ObjectTypeInfo == typeof(HRJobVacancy))
            //{
            //   // ((HRJobVacancy)View.CurrentObject).PendingActivities =((HRJobVacancy)View.CurrentObject).PendingActivities.Where(x => ((HRJobVacancy)View.CurrentObject).Applicants.Contains<HREmployee>(x.Entity));
            //}
            //// Perform various tasks depending on the target View.
            //if (View is DetailView && View.CurrentObject.GetType() == typeof(HRJobPosition))
            //{

            //    if (((HRJobPosition)View.CurrentObject).Employees.Count > 0)
            //    {
            //        Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Executing += HRController_Executing;
            //    }
            //}
        }
        //private void popupWindowShowAction1_CustomizeTemplate(object sender, CustomizeTemplateEventArgs e)
        //{
        //    ((DevExpress.XtraReports.Design.XRSmartTagService.PopupForm)e.Template).Size = new System.Drawing.Size(200, 200);
        //}
        //void HRController_Executing(object sender, System.ComponentModel.CancelEventArgs e)
        //{
        //    MessageBox.Show(Application, "Can't Delete this JobPosition ", new Action(delegate { ObjectSpace.Refresh(); }));
        //}
        protected override void OnViewControlsCreated()
        {
            base.OnViewControlsCreated();
            // Access and customize the target View control.
        }
        protected override void OnDeactivated()
        {
            // Unsubscribe from previously subscribed events and release other references and resources.
            base.OnDeactivated();
        }

        private void AssignNewRevision_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            HRJobPosition currentobject = (HRJobPosition)View.CurrentObject;
            HRJobPositionRevision newobject = new HRJobPositionRevision(currentobject.Session, currentobject);

            //if (currentobject.Revisions.Count > 0)
            //{

            //    HRJobPositionRevision lastrevision = currentobject.Revisions.Where(x => x.RevisionNumber == currentobject.RevisionNumber).FirstOrDefault();
            //    if (lastrevision != null)
            //    {
                    newobject.RevisionNumber = currentobject.RevisionNumber + 1;
                    currentobject.RevisionNumber = currentobject.RevisionNumber + 1;
            //    }
            //}
                    newobject.Employees.BaseAddRange(currentobject.Employees);
            newobject.Activities.BaseAddRange(currentobject.Activities);
            currentobject.CurrenRevision = newobject;
           // newobject.EntityAttachments.BaseAddRange(currentobject.EntityAttachments);
            newobject.Save();
            currentobject.Save();
            currentobject.Session.CommitTransaction();
           
        }

        private void GetSettings_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            IObjectSpace currentobjectspace = Application.CreateObjectSpace();
            ICollection<EntityType> alltypes = ObjectSpace.GetObjects<EntityType>();
            ObjectType setuptype = ObjectSpace.FindObject<ObjectType>(CriteriaOperator.Parse("[Name] = 'Settings'"));
            EntityTypeSettings newEntitySetting;
            if (setuptype != null)
            {
                foreach (EntityType Type in alltypes)
                {
                    AriaObject Setupobject = ObjectSpace.FindObject<AriaObject>(CriteriaOperator.Parse("[ObjectType] = '" + setuptype.Oid + "' and [ObjectName] = '" + Type.Name + "'"));
                    if (Setupobject != null)
                    {
                       newEntitySetting = ObjectSpace.FindObject<EntityTypeSettings>(CriteriaOperator.Parse("[Application] = '" + Setupobject.Application.Oid + "' and [EntityType] = '" + Type.Oid + "'"));
                       if (newEntitySetting == null)
                        {
                            newEntitySetting = ObjectSpace.CreateObject<EntityTypeSettings>();
                            newEntitySetting.Application = Setupobject.Application;
                            newEntitySetting.EntityType = Type;
                            newEntitySetting.Save();
                        }
                        
                        foreach (AriaObjectProperty Property in Setupobject.AriaObjectProperties)
                        {
                            if (Property.AriaObjectPropertiesSettings.Count() > 0)
                            {
                                foreach (AriaObjectPropertySetting propertysetting in Property.AriaObjectPropertiesSettings)
                                {
                                    TypeSettings existsetting = newEntitySetting.Settings.Where(x => x.SettingName == Property.PropertyName && x.Settingtype == propertysetting.SettingType).FirstOrDefault();
                                    if (existsetting == null)
                                    {
                                        TypeSettings newsetting = ObjectSpace.CreateObject<TypeSettings>();
                                        newsetting.Settingtype = propertysetting.SettingType;
                                        newsetting.Value = propertysetting.Value;
                                        newsetting.SettingName = Property.PropertyName;
                                        newsetting.Save();
                                        newEntitySetting.Settings.Add(newsetting);
                                    }
                                }
                            }
                        }
                        ObjectSpace.CommitChanges();
                    }
                }
            }
           
        }

        private void CompleteActivity_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            AriaSecuritySystemUser currentuser = ((HR_ActivityLog)View.CurrentObject).Session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("[Oid] = '" + ((AriaSecuritySystemUser)SecuritySystem.CurrentUser).Oid + "'"));
            if (currentuser != null && currentuser.Employee != null)
            {
                HR_ActivityLog Activity = (HR_ActivityLog)View.CurrentObject;
                if (Activity.User == currentuser.Employee)
                {
                    Activity.Status = ActivityStatus.History;
                    Activity.CompleteDate = DateTime.Now;
                    Activity.CompleteBy = ((HR_ActivityLog)View.CurrentObject).Session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("[Oid] = '" + ((AriaSecuritySystemUser)SecuritySystem.CurrentUser).Oid + "'"));
                    Activity.Save();
                    Activity.Session.CommitTransaction();
                }
                else
                {
                    throw new Exception("yoou are not the Activity user to complete it please ask the activity user to complete");
                }
               
            }
           
        }

        private void ScheduleActivities_Execute(object sender, ParametrizedActionExecuteEventArgs e)
        {
            string username = e.ParameterCurrentValue.ToString();
            AriaSecuritySystemUser user = ObjectSpace.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("UserName = '" + username + "'"));
           //  HRActivity x = new HRActivity();

            List<HRActivity> list = ObjectSpace.GetObjects<HRActivity>().Where(z => z.Category.Name.Contains("Recruitment")).ToList<HRActivity>();
            if (!string.IsNullOrEmpty(username))
            {
                foreach (HREmployee applicant in e.SelectedObjects)
                {
                    foreach (HRActivity act in list)
                    {
                        if (applicant.PendingActivities.Where(x => x.HRActivity == act).Count() == 0)
                        {
                            HR_ActivityLog activity = ObjectSpace.CreateObject<HR_ActivityLog>();
                            activity.HRActivity = act;
                            activity.Department = act.Category.Department;
                            activity.Category = act.Category;
                            activity.Entity = applicant;
                            activity.User = user.Employee;
                            activity.StartDate = DateTime.Now.Date;
                            activity.EndDate = DateTime.Now.Date;
                            activity.Save();
                        }
                        
                    }
                   
                }
                ObjectSpace.CommitChanges();
            }
        }

        private void GetTheJobVacancyForm_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            DevExpress.Web.ASPxWebControl.RedirectOnCallback(string.Format("{0}ApplyForJobVacancy.aspx?jobvacancy=" + ((HRJobVacancy)View.CurrentObject).Oid + "&jobvacancyName=" +((HRJobVacancy)View.CurrentObject) .JobPosition.Name+ "", System.Web.HttpContext.Current.Request.Url.AbsoluteUri.Replace("default.aspx", "")));
        }
        public Notes note;
        private void changeapplicantstage_Execute(object sender, SingleChoiceActionExecuteEventArgs e)
        {
            IObjectSpace ospace = Application.CreateObjectSpace();
            if (e.SelectedChoiceActionItem.Id == "Passed")
            {
                foreach (HREmployee App in e.SelectedObjects)
                {
                    HREmployee Applicant = ospace.FindObject<HREmployee>(CriteriaOperator.Parse("[Oid] = '" + App.Oid + "'"));
                    if(Applicant.Stage != HREmployee.RecruitmentStages.SendingJobOffer)
                    Applicant.Stage += 1;
                    Applicant.Save();
                }
            }
            else
            {               
                    note = ospace.CreateObject<Notes>();
                    note.Date = DateTime.Today;
                    DetailView dv = Application.CreateDetailView(ospace, note);
                    dv.ViewEditMode = ViewEditMode.Edit;
                    Application.ShowViewStrategy.ShowViewInPopupWindow(dv, okDelegate: OkDelegate);
            }
            ospace.CommitChanges();
            this.View.Refresh();
           
        }

        public void OkDelegate()
        {
            IObjectSpace os = DevExpress.ExpressApp.Xpo.XPObjectSpace.FindObjectSpaceByObject(note);
            
            if (os != null)
            {
                foreach (var item in os.ModifiedObjects)
                {
                    if (item.GetType() == typeof(Notes))
                    {
                        foreach (HREmployee app in View.SelectedObjects)
                        {
                            Notes newnote = os.CreateObject<Notes>();
                            newnote.Title = app.Stage.ToString() + "Rejected";
                            newnote.Description = ((Notes)item).Description;
                            newnote.Date = ((Notes)item).Date;
                            HREmployee emp = os.FindObject<HREmployee>(CriteriaOperator.Parse("[Oid] = '" + app.Oid + "'"));
                            emp.NotesList.Add(newnote);
                            emp.Stage = HREmployee.RecruitmentStages.Rejected;
                            emp.Save();
                        }
                        break;
                    }
                }
            }
            os.CommitChanges();
            View.RefreshDataSource();
            View.Refresh();   
        }
        public void CancelDelegate()
        {
            // place the code for the 'Cancel' button here 
        }
    }
}
