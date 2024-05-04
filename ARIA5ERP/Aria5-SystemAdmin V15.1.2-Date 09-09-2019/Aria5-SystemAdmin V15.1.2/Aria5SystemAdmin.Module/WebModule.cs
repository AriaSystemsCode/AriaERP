using System;
using System.Collections.Generic;

using DevExpress.ExpressApp;
using System.Reflection;
using DevExpress.ExpressApp.Validation;
using DevExpress.Persistent.Validation;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp.Security;
using DevExpress.Xpo;
using DevExpress.ExpressApp.Notifications;
using DevExpress.Persistent.Base.General;
using Aria5SystemAdmin.Module.Managers;
using System.Data.SqlClient;


namespace Aria5SystemAdmin.Module
{
    public sealed partial class Aria5SystemAdminModule : ModuleBase
    {
        public Aria5SystemAdminModule()
        {
            InitializeComponent();
        }

        public override void Setup(ApplicationModulesManager moduleManager)
        {
            base.Setup(moduleManager);

            ValidationRulesRegistrator.RegisterRule(moduleManager, typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveProperty.AriaObjectShelvePropertyCodeRule), typeof(IRuleBaseProperties));
            ValidationRulesRegistrator.RegisterRule(moduleManager, typeof(Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry.TrackingEntryCodeRule), typeof(IRuleBaseProperties));
            ValidationRulesRegistrator.RegisterRule(moduleManager, typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveMethodParameter.AriaObjectShelveMethodParameterCodeRule), typeof(IRuleBaseProperties));
            ValidationRulesRegistrator.RegisterRule(moduleManager, typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveMethod.AriaObjectShelveMethodCodeRule), typeof(IRuleBaseProperties));
            ValidationRulesRegistrator.RegisterRule(moduleManager, typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveEventParameter.AriaObjectShelveEventParameterCodeRule), typeof(IRuleBaseProperties));
            ValidationRulesRegistrator.RegisterRule(moduleManager, typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveEvent.AriaObjectShelveEventCodeRule), typeof(IRuleBaseProperties));




            try
            {
                //included the next 2 lines in a try-catch block to check if there is a failure during their execution Emad 18/6/18
                Application.CustomProcessShortcut += Application_CustomProcessShortcut;
                //ATA  customiza the logged on event for notification 
                Application.LoggedOn += new EventHandler<LogonEventArgs>(application_LoggedOn);
                //ATA 
            }
            catch (Exception ex)
            {
                Console.WriteLine("default exception on line:38");
                //throw;
            }
            
        }
        void application_LoggedOn(object sender, LogonEventArgs e)
        {
            NotificationsModule notificationsModule = Application.Modules.FindModule<NotificationsModule>();
            DefaultNotificationsProvider notificationsProvider = notificationsModule.DefaultNotificationsProvider;
            notificationsProvider.CustomizeNotificationCollectionCriteria += notificationsProvider_CustomizeNotificationCollectionCriteria;
          //  selectdata();
        }
        //ATA checke the notification objects and create Nc's commented code
        #region Commented Code 
        //public void selectdata()
        //{
        //    //SqlConnection CS = new SqlConnection(@"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123");
        //    SqlConnection CS = new SqlConnection(@"Data Source=NSDE_KHALED;Initial Catalog=Aria5SystemAdmin22;User ID=sa;Password=aria_123");
        //    SqlCommand CMD = new SqlCommand("Select * from Projecttemplate where [notificationmessage] is not null",CS);
        //    CMD.Connection.Open();
        //    SqlDataReader dr = CMD.ExecuteReader();
            
        //    while (dr.Read())
        //    {
        //        DevExpress.Xpo.Session sess = new DevExpress.Xpo.Session();
        //        sess.ConnectionString= @"Data Source=NSDE_KHALED;Initial Catalog=Aria5SystemAdmin22;User ID=sa;Password=aria_123";
        //        sess.Connect();
        //        bool y = sess.IsConnected;
        //        string x = dr[0].ToString();
        //        ProjectTemplate Notification_Project = sess.FindObject<ProjectTemplate>(CriteriaOperator.Parse("[oid] = '" +x+ "'"));
        //        if (Notification_Project != null)
        //        {
        //            Checkprojectalarmtimes(Notification_Project);                    
        //        }
        //    }
        //    CMD.Connection.Close();
        //}
        //public void Checkprojectalarmtimes( ProjectTemplate Notification_Project) 
        //{
        //    //SqlConnection CS = new SqlConnection(@"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123");
        //    //SqlCommand CMD = new SqlCommand("Select * from Projecttemplate where [notificationmessage] is not null",CS);
        //    //CMD.Connection.Open();
        //    //SqlDataReader dr = CMD.ExecuteReader();
        //    //IList<ProjectTemplate> allproject = new List<ProjectTemplate>();
        //    //while (dr.Read())
        //    //{
        //    //            allproject = 
        //    //}
        //    //CMD.Connection.Close();
        //    //if (allproject.Count > 0)
        //    //{
        //    //    foreach (ProjectTemplate Notification_Project in allproject)
        //    //    {
        //            Notification_Project.Session.LockingOption = LockingOption.None;
        //            if (DateTime.Today >= (CalcEndate.CalcEnddate(Notification_Project.StartDate, 2).Subtract(new TimeSpan(10, 0, 0, 0)))) //&& DateTime.Today < (CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)))
        //            {
                        
        //                if (Notification_Project.EmailSent != false)
        //                {
        //                    string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
        //                    Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some objects not ready or missing yet and you have only '{0}' remaining to complete it before the end of the related project phase which will be end on Sunday, {1} <br /> Details of the object(s) not ready or missing: <br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)).Subtract(DateTime.Today)).Days,(CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)));
        //                    FillEmailBody(Emailbody, Notification_Project, false);
        //                    //Notification_Project.EmailSent = true;
        //                    //Notification_Project.Save();
        //                    //Notification_Project.Session.CommitTransaction();
        //                }
                        
        //            }
        //            else if ((DateTime.Today >= (CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)).Subtract( new TimeSpan(4,0,0,0)) || DateTime.Today == (CalcEndate.CalcEnddate(Notification_Project.StartDate,2)))&& DateTime.Today < (CalcEndate.CalcEnddate(Notification_Project.StartDate,2)).AddDays(1))
        //            {
        //                if (Notification_Project.EmailSent == true)
        //                {
        //                    string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
        //                    Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some objects not ready or missing yet and you have only '{0}' remaining to complete it before the end of the related project phase which will be end on Sunday, {1} <br /> Details of the object(s) not ready or missing: <br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)).Subtract(DateTime.Today)).Days, (CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)));
        //                    FillEmailBody(Emailbody, Notification_Project , true);
        //                    Notification_Project.EmailSent = false;
        //                    Notification_Project.Save();
        //                    Notification_Project.Session.CommitTransaction();
        //                }
                        
        //            }
        //            else if (DateTime.Today >= (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3).Subtract(new TimeSpan(10, 0, 0, 0))) && DateTime.Today < (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)))
        //            {
        //                if (Notification_Project.TestCases.Count == 0)
        //                {
        //                    if (Notification_Project.SecondEmailSent == false)
        //                    {
        //                        string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
        //                        Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some test cases<font color='red'> missing </font>yet and you have only '{0}' remaining to complete it before the end of the related project phase which will be end on Sunday, {1} <br /><br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)).Subtract(DateTime.Today)).Days, (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)));
        //                        string TOEmail = Notification_Project.ProjectOwner.CurrentEmailAddress + ",quality@ariasystems.biz";
        //                        SendEmail(Emailbody, "BaselineForTestCases ", TOEmail);
        //                        Notification_Project.SecondEmailSent = true;
        //                        Notification_Project.Save();
        //                        Notification_Project.Session.CommitTransaction();
        //                    }

        //                }
        //                else
        //                {
        //                    bool sendmail = false;
        //                    string  Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
        //                    Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some test cases not ready yet and you have only '{0}' remaining to complete it before the end of the related project phase which will be end on Sunday, {1} <br /> Details of the object(s) not ready: <br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)).Subtract(DateTime.Today)).Days, (CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)));
        //                    foreach (TestCase testcase in Notification_Project.TestCases)
        //                    {
        //                        if (testcase.Status != TestCase.status.Ready)
        //                        {
        //                            Emailbody += string.Format(testcase.Title);
        //                            sendmail = true;
        //                        }
        //                    }
        //                    if (Notification_Project.SecondEmailSent == false && sendmail == true)
        //                    {
        //                        string TOEmail = Notification_Project.ProjectOwner.CurrentEmailAddress + ",quality@ariasystems.biz";
        //                        SendEmail(Emailbody, "BaselineForTestCases ", TOEmail);
        //                        Notification_Project.SecondEmailSent = true;
        //                    }
        //                }
        //            }
        //            else if ((DateTime.Today >= (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)).Subtract( new TimeSpan(4,0,0,0)) || DateTime.Today == (CalcEndate.CalcEnddate(Notification_Project.StartDate,3))) && DateTime.Today < (CalcEndate.CalcEnddate(Notification_Project.StartDate,3)).AddDays(1))
        //            {
        //                if (Notification_Project.TestCases.Count == 0)
        //                {
        //                    if (Notification_Project.SecondEmailSent == false)
        //                    {
        //                        string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
        //                        Emailbody += string.Format("Please note that the duedates for the following project '" + Notification_Project.Name + "' has {0} days left, <br /> For the test cases , <br /><br />", (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)).Subtract(DateTime.Today));
        //                        string TOEmail = Notification_Project.ProjectOwner.CurrentEmailAddress + ",quality@ariasystems.biz";
        //                        SendEmail(Emailbody, "BaselineForTestCases ", TOEmail);
        //                        Notification_Project.SecondEmailSent = true;
        //                        Notification_Project.Save();
        //                        Notification_Project.Session.CommitTransaction();
        //                    }

        //                }
        //                else
        //                {
        //                    bool sendmail = false;
        //                    string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
        //                    Emailbody += string.Format("Please note that the duedates for the following project '" + Notification_Project.Name + "' has {0} days left, <br /> and statues for below test cases is not ready, <br /><br />", (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)).Subtract(DateTime.Today));
        //                    foreach (TestCase testcase in Notification_Project.TestCases)
        //                    {
                                
        //                        if (testcase.Status != TestCase.status.Ready)
        //                        {
        //                            Emailbody += string.Format(testcase.Title);
        //                            sendmail = true;
        //                        }
        //                    }
        //                    if (Notification_Project.SecondEmailSent == false && sendmail == true)
        //                    {
        //                        string TOEmail = Notification_Project.ProjectOwner.CurrentEmailAddress + ",quality@ariasystems.biz";
        //                        SendEmail(Emailbody, "BaselineForTestCases ", TOEmail);
        //                        Notification_Project.SecondEmailSent = true;
        //                    }
        //                }
        //            }
        //      //  }
        //   // }
        //}
        //public void checkrequirementalarmtime()
        //{
        //    //IObjectSpace ObjectSpace = Application.CreateObjectSpace();
        //    //IList<Requirement> Requirements = ObjectSpace.GetObjects<Requirement>(CriteriaOperator.Parse("AlarmTime <= LocalDateTimeToday()"));
        //    //if (Requirements.Count > 0)
        //    //{
        //    //    foreach (Requirement req in Requirements)
        //    //    {
        //    //        if ((DateTime.Today >= req.Enddate.Subtract(new TimeSpan(4, 0, 0, 0)) || DateTime.Today == req.Enddate) && DateTime.Today < req.Enddate.AddDays(1))
        //    //        {
        //    //            string Emailbody = " Dear " + req.ProjectTemplate.ProjectOwner.Name + "<br /><br />";
        //    //            Emailbody += "@Please note that the duedates for this requirement '" + req.Title + "' is Today , <br /> For the use cases and test cases and project entities , <br /><br />";
        //    //            string TOEmail = req.ProjectTemplate.ProjectOwner.CurrentEmailAddress;
        //    //            SendEmail(Emailbody, "Base line For Test Cases ", TOEmail);
        //    //        }
        //    //    }
        //    //}
         

        //}
        //public void FillEmailBody(string emailbody , ProjectTemplate Project , bool createNc)
        //{
        //    bool requirement = false;
        //    bool uc = false;
        //    bool pe = false;
        //    bool ucp = false;
        //    if (Project.Requirements.Count == 0)
        //    {
        //        emailbody += "@Requirements is <font color='red'> missing </font> <br />";
        //        requirement = true;
        //        CreateNCs((int)QANonComplains.NonComplianceItems.Requirement, "is missing", new Session(), Project);
        //    }
        //    else
        //    {
        //        foreach (Requirement existrequirement in Project.Requirements)
        //        {
        //            if (existrequirement.RequirementStatus != Requirement.RequirmentsStatus.Ready)
        //            {
        //                emailbody += string.Format("Requirement : '{0}' it's Status is not ready <br />", existrequirement.Title);
        //                requirement = true;
        //                CreateNCs((int)QANonComplains.NonComplianceItems.Requirement, string.Format("{0} status is not ready",existrequirement.Title), new Session(), Project);
        //            }
        //        }
        //    }
        //    if (Project.ProjectEntities.Count == 0)
        //    {
        //        emailbody += "@Project entities is <font color='red'> missing  </font><br />";
        //        pe = true;
        //    }
        //    else
        //    {
        //        foreach (QAProjectEntity existprojectenity in Project.ProjectEntities)
        //        {
        //            if (existprojectenity.ProjectEntity_Statues != QAProjectEntity.Statues.Ready)
        //            {
        //                emailbody += string.Format("Project Entity : '{0}' it's status is not ready<br />", existprojectenity.Name);
        //                pe = true;
        //            }
        //        }
        //    }
        //    if (Project.UseCasePoints == null)
        //    {
        //        emailbody += "@Use Case Points is <font color='red'> missing </font><br />";
        //        ucp = true;
        //    }
        //    if (Project.UseCases.Count == 0)
        //    {
        //        emailbody += "@Use Cases is missing <br />";
        //        uc = true;
        //    }
        //    else
        //    {
        //        foreach (QAUseCase existusecase in Project.UseCases)
        //        {
        //            if (existusecase.UseCase_Statues != QAUseCase.Statues.Ready)
        //            {
        //                emailbody += string.Format("Use Case : '{0}' it's status is not ready<br />", existusecase.Name);
        //                uc = true;
        //            }
        //        }
        //    }
        //    if (requirement || pe || uc || ucp)
        //    {
               
        //        string ToEmail = Project.ProjectOwner.CurrentEmailAddress;
        //        SendEmail(emailbody, "BaseLineNotification", ToEmail);
        //        Project.EmailSent = true;
        //        Project.Session.BeginTransaction();
        //        Project.Save();
        //        Project.Session.CommitTransaction();
        //    }
        //}
        //public void SendEmail(string emailbody, string emailtitle, string emailTo)
        //{
        //     Email notificationmail = new Email();
        //    notificationmail.FromEmail = "khaled.m@ariasystems.biz";
        //    notificationmail.EmailPassword = "Kamag@2016";
        //    notificationmail.ToEmail = emailTo.ToString();
        //    emailbody = emailbody.Replace("@", System.Environment.NewLine);
        //    notificationmail.EmailBody = emailbody;
        //    notificationmail.EmailTitle = emailtitle;
        //    notificationmail.EmailSubject = "Base line Notification ";
        //    notificationmail.SendEmail();
        //}
        //public void CreateNCs(int Object , string desc ,Session Objectspace, ProjectTemplate project)
        //{
        //    //QANonComplains newone = Objectspace.CreateObject<QANonComplains>();
        //    //ProjectTemplate relatedproject = Objectspace.FindObject<ProjectTemplate>(CriteriaOperator.Parse("[oid]= '" + project.Oid + "'"));
        //    //Resources owner = Objectspace.FindObject<Resources>(CriteriaOperator.Parse("[oid]= '" + project.ProjectOwner.Oid + "'"));            
        //    //newone.RelatedProject = relatedproject;
        //    //newone.Description = desc;
        //    //newone.NonComplainType = Objectspace.FindObject<QAComplainType>(CriteriaOperator.Parse("[oid] = 'cbca1979-ec87-4f95-9321-c8b28c1bb177'"));
        //    //newone.Status = QANonComplains.ComplainStatus.New;
        //    //newone.NonComplianceObjects = Object;
        //    //project.ProjectOwner.Session.LockingOption = LockingOption.None;
        //    //newone.Owner = owner;
        //    //newone.NotificationMessage = string.Format("Kindly note that you have Nc related to this project '{0}'", project.Name);//, newone.NonComplainActivity.ActivityName);
        //    //newone.Save();
        //    //newone.Session.CommitTransaction();
        //    Objectspace.ConnectionString = @"Data Source=NSDE_KHALED;Initial Catalog=Aria5SystemAdmin22;User ID=sa;Password=aria_123";
        //    Objectspace.Connect();
        //    if (Object == (int)QANonComplains.NonComplianceItems.TestCases)
        //    {
        //        Objectspace.ExecuteQuery("insert into QANonComplains ([oid],[Owner],[RelatedProject],[Description],[NonComplainType],[Status],[NonComplianceObjects],[AlarmTime],[NotificationMessage]) Values ('" + Guid.NewGuid() + "','8A46A928-AE14-4C48-95DF-E226E256D695','" + project.Oid + "','" + desc + "','cbca1979-ec87-4f95-9321-c8b28c1bb177','" + Object + "','" + 1 + "','" + DateTime.Today + "','" + string.Format("Kindly note that you have Nc related to this project \"{0}\"", project.Name) + "')");
        //    }
        //    else
        //    {
        //        Objectspace.ExecuteQuery("insert into QANonComplains ([oid],[Owner],[RelatedProject],[Description],[NonComplainType],[Status],[NonComplianceObjects],[AlarmTime],[NotificationMessage]) Values ('" + Guid.NewGuid() + "','" + project.ProjectOwner.Oid + "','" + project.Oid + "','" + desc + "','cbca1979-ec87-4f95-9321-c8b28c1bb177','" + Object + "','" + 1 + "','" + DateTime.Today + "','" + string.Format("Kindly note that you have Nc related to this project \"{0}\"", project.Name) + "')");
        //    }
        //    Objectspace.Disconnect();
        //}
#endregion 
        void notificationsProvider_CustomizeNotificationCollectionCriteria(object sender, CustomizeCollectionCriteriaEventArgs e)
        {
            if (e.Type == typeof(TrackingTask))
            {
                e.Criteria = CriteriaOperator.Parse("Resources.UserName.Oid == CurrentUserId()");
            }
            else if (e.Type == typeof(QANonComplains))
            {
                e.Criteria = CriteriaOperator.Parse("Owner.UserName.Oid == CurrentUserId()");
            }
            else if (e.Type == typeof(ProjectTemplate))
            {
                e.Criteria = CriteriaOperator.Parse("ProjectOwner.UserName.Oid == CurrentUserId()");//&& (Requirements.Count == 0 || ProjectEntities.Count == 0)");
            }
            //else if (e.Type == typeof(QAProjectEntity))
            //{
            //    e.Criteria = CriteriaOperator.Parse("ProjectTemplate.ProjectOwner.UserName.Oid == CurrentUserId()");
            //}
        }
        private void Application_CustomProcessShortcut(object sender, CustomProcessShortcutEventArgs e)
        {
            if (e.Shortcut.ViewId == "EntityOperationPermissionGenerator_ListView")
            {
                IObjectSpace objectSpace = Application.CreateObjectSpace();
                EntityOperationPermissionGenerator permissionGenerator = objectSpace.CreateObject<EntityOperationPermissionGenerator>();
                e.View = Application.CreateDetailView(objectSpace, permissionGenerator, true);
                e.View.AllowEdit["CanEditPermissionGenerator"] = true;
                e.Handled = true;
            }
            //SARA.N,1 [Tracking# + Aria5-DevExpress-Account]_Programming, 2-08-2015 [START]

            if (e.Shortcut.ViewId == "Account_DetailView" & string.IsNullOrWhiteSpace(e.Shortcut.ObjectKey))
            {
                IObjectSpace objectSpace2 = Application.CreateObjectSpace();
                string userName = ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters).UserName;
                CriteriaOperator criteria = CriteriaOperator.Parse("UserName = '" + userName + "'");
                AriaSecuritySystemUser user = objectSpace2.FindObject<AriaSecuritySystemUser>(criteria);
                Account account = objectSpace2.FindObject<Account>(CriteriaOperator.Parse("Oid = '" + user.Account.Oid + "'"));
                e.View = Application.CreateDetailView(objectSpace2, account, false);
                ((DetailView)(e.View)).ViewEditMode = DevExpress.ExpressApp.Editors.ViewEditMode.View;
                e.View.AllowEdit["CanEditPermissionGenerator"] = false;
                e.Handled = true;
            }
            //SARA.N,1 [Tracking# + Aria5-DevExpress-Account]_Programming, 2-08-2015 [END]
        }   
    }
}
