using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Data.Filtering;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Aria5SystemAdmin.Module.Managers
{
    public class BaselineNCsManager
    {

        //ATA this function check each month the required actions  
        public  void Checkprojectalarmtimes(ProjectTemplate Notification_Project, Session Session)
        {
            Notification_Project.Session.LockingOption = LockingOption.None;
            if (DateTime.Today >= (CalcEndate.CalcEnddate(Notification_Project.StartDate, 1).Subtract(new TimeSpan(10, 0, 0, 0))) && DateTime.Today < (CalcEndate.CalcEnddate(Notification_Project.StartDate, 1)))
            {
                string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
                Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some objects not ready or missing yet and you have only '{0}' remaining days to complete it before the end of the related project phase which will be end on Sunday, {1} <br /> Details of the object(s) not ready or missing: <br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 1)).Subtract(DateTime.Today)).Days, (CalcEndate.CalcEnddate(Notification_Project.StartDate, 1)));
                //FillEmailBody(Emailbody, Notification_Project, false);
                firstmonthcheck(Emailbody, Notification_Project, false,Session);
            }
            else if (DateTime.Today == (CalcEndate.CalcEnddate(Notification_Project.StartDate, 1)))
            {
                string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
                Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some objects not ready or missing yet and you have only '{0}' remaining days to complete it before the end of the related project phase which will be end on Sunday, {1} <br /> Details of the object(s) not ready or missing: <br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 1)).Subtract(DateTime.Today)).Days, (CalcEndate.CalcEnddate(Notification_Project.StartDate, 1)));
                //FillEmailBody(Emailbody, Notification_Project, false);
                firstmonthcheck(Emailbody, Notification_Project, false,Session);
            }
            else if (DateTime.Today == (CalcEndate.CalcEnddate(Notification_Project.StartDate, 1)).AddDays(1))
            {
                string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
                Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some objects not ready or missing yet and the base line was yesterday so checke the NC's that created related to this project <br /> Details of the object(s) not ready or missing: <br /><br />");
                // FillEmailBody(Emailbody, Notification_Project, true);
                firstmonthcheck(Emailbody, Notification_Project, true,Session);
            }
            else if (DateTime.Today >= (CalcEndate.CalcEnddate(Notification_Project.StartDate, 2).Subtract(new TimeSpan(10, 0, 0, 0))) && DateTime.Today < (CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)))
            {
                string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
                Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some objects not ready or missing yet and you have only '{0}' remaining days to complete it before the end of the related project phase which will be end on Sunday, {1} <br /> Details of the object(s) not ready or missing: <br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)).Subtract(DateTime.Today)).Days, (CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)));
                //FillEmailBody(Emailbody, Notification_Project, false);
                Secondmonthcheck(Emailbody, Notification_Project, false, Session);
            }
            else if (DateTime.Today == (CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)))
            {
                string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
                Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some objects not ready or missing yet and you have only '{0}' remaining days to complete it before the end of the related project phase which will be end on Sunday, {1} <br /> Details of the object(s) not ready or missing: <br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)).Subtract(DateTime.Today)).Days, (CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)));
                //FillEmailBody(Emailbody, Notification_Project, false);
                Secondmonthcheck(Emailbody, Notification_Project, false, Session);
            }
            else if (DateTime.Today == (CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)).AddDays(1))
            {
                string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
                Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some objects not ready or missing yet and the base line was yesterday so checke the NC's that created related to this project <br /> Details of the object(s) not ready or missing: <br /><br />");
                //FillEmailBody(Emailbody, Notification_Project, true);
                Secondmonthcheck(Emailbody, Notification_Project, true, Session);
            }
            else if (DateTime.Today >= (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3).Subtract(new TimeSpan(10, 0, 0, 0))) && DateTime.Today < (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)))
            {
                string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
                Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some test cases<font color='red'> missing </font>yet and you have only '{0}' remaining days to complete it before the end of the related project phase which will be end on Sunday, {1} <br /><br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)).Subtract(DateTime.Today)).Days, (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)));
                //  string TOEmail = Notification_Project.ProjectOwner.CurrentEmailAddress + ",quality@ariasystems.biz";
                Thirdmonthcheck(Emailbody, Notification_Project, false, Session);
            }
            else if (DateTime.Today == (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)))
            {
                string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
                Emailbody += string.Format("@In reference to the project '" +  Notification_Project.Name + "' , please note that you have some test cases not ready yet and you have only '{0}' remaining days to complete it before the end of the related project phase which will be end on Sunday, {1} <br /> Details of the object(s) not ready: <br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)).Subtract(DateTime.Today)).Days, (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)));
                Thirdmonthcheck(Emailbody, Notification_Project, false, Session);
            }
            else if (DateTime.Today == (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)).AddDays(1))
            {
                string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
                Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some objects not ready or missing yet and the base line was yesterday so checke the NC's that created related to this project <br /> Details of the object(s) not ready or missing: <br /><br />");
                Thirdmonthcheck(Emailbody, Notification_Project, true, Session);
            }
            else if (DateTime.Today >= (CalcEndate.CalcEnddate(Notification_Project.StartDate, 4).Subtract(new TimeSpan(10, 0, 0, 0))) && DateTime.Today < (CalcEndate.CalcEnddate(Notification_Project.StartDate, 4)))
            {
                string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
                Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some test cases<font color='red'> missing </font>yet and you have only '{0}' remaining days to complete it before the end of the related project phase which will be end on Sunday, {1} <br /><br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)).Subtract(DateTime.Today)).Days, (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)));
                Forthmonthcheck(Emailbody, Notification_Project, false, Session);

            }
            else if (DateTime.Today == (CalcEndate.CalcEnddate(Notification_Project.StartDate, 4)))
            {
                string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
                Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some test cases not ready yet and you have only '{0}' remaining days to complete it before the end of the related project phase which will be end on Sunday, {1} <br /> Details of the object(s) not ready: <br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)).Subtract(DateTime.Today)).Days, (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)));
                Forthmonthcheck(Emailbody, Notification_Project, false, Session);
            }
            else if (DateTime.Today == (CalcEndate.CalcEnddate(Notification_Project.StartDate, 4)).AddDays(1))
            {
                string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
                Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some objects not ready or missing yet and the base line was yesterday so checke the NC's that created related to this project <br /> Details of the object(s) not ready or missing: <br /><br />");
                Forthmonthcheck(Emailbody, Notification_Project, true, Session);
            }
        }
        #region oldfunctioncommented
        //public static void FillEmailBody(string emailbody, ProjectTemplate Project, bool createNc)
        //{
        //    string body = "FillEmailBody start work now for project '" + Project.Name + "'";
        //    string emailto = "ahmed.t@ariany.com";
        //    SendEmail(body, "Base line Web Job", emailto);
        //    bool requirement = false;
        //    bool uc = false;
        //    bool pe = false;
        //    bool ucp = false;
        //    bool PSC = false;
        //    if (Project.Requirements.Count == 0)
        //    {
        //        emailbody += "@Requirements is <font color='red'> missing </font> <br />";
        //        requirement = true;
        //        if (createNc == true)
        //        {
        //            CreateNCs((int)QANonComplains.NonComplianceItems.Requirement, "is missing", new Session(), Project);
        //        }
        //    }
        //    else
        //    {
        //        foreach (Requirement existrequirement in Project.Requirements)
        //        {
        //            if (existrequirement.ApproveStatus != Requirement.ApproveStatu.Ready)
        //            {
        //                emailbody += string.Format("Requirement : '{0}' it's Status is not ready <br />", existrequirement.Title);
        //                requirement = true;
        //                if (createNc == true)
        //                {
        //                    CreateNCs((int)QANonComplains.NonComplianceItems.Requirement, string.Format(" \"{0}\" status is not ready", existrequirement.Title), new Session(), Project);
        //                }
        //            }
        //        }
        //    }
        //    ATA add project scope to validation [Start]
        //    if (Project.Scope.Count == 0)
        //    {
        //        emailbody += "@project Scope is <font color='red'> missing </font> <br />";
        //        PSC = true;
        //        if (createNc == true)
        //        {
        //            CreateNCs((int)QANonComplains.NonComplianceItems.Requirement, "is missing", new Session(), Project);
        //        }
        //    }
        //    else
        //    {
        //        if (Project.Scope.Where(x => x.Status == approvalstatus.Approved).Count() == 0)
        //        {

        //            emailbody += "There are no project scop Approved yet";
        //            PSC = true;
        //            if (createNc == true)
        //            {
        //                 CreateNCs((int)QANonComplains.NonComplianceItems.Requirement, string.Format(""), new Session(), Project);
        //            }
        //        }
        //    }
        //    ATA add project scope to validation [End]
        //    if (Project.ProjectEntities.Count == 0)
        //    {
        //        emailbody += "@Project entities is <font color='red'> missing  </font><br />";
        //        pe = true;
        //        if (createNc == true)
        //        {
        //            CreateNCs((int)QANonComplains.NonComplianceItems.ProjectEntities, "is missing", new Session(), Project);

        //        }
        //    }
        //    else
        //    {
        //        foreach (QAProjectEntity existprojectenity in Project.ProjectEntities)
        //        {
        //            if (existprojectenity.ApproveStatus != QAProjectEntity.ApproveStatu.Ready)
        //            {
        //                emailbody += string.Format("Project Entity : '{0}' it's status is not ready<br />", existprojectenity.Name);
        //                pe = true;
        //                if (createNc == true)
        //                {
        //                    CreateNCs((int)QANonComplains.NonComplianceItems.ProjectEntities, string.Format(" \"{0}\" status is not ready", existprojectenity.Name), new Session(), Project);

        //                }
        //            }
        //        }
        //    }
        //    if (Project.UseCasePoints == null)
        //    {
        //        emailbody += "@Use Case Points is <font color='red'> missing </font><br />";
        //        ucp = true;
        //        if (createNc == true)
        //        {
        //            CreateNCs((int)QANonComplains.NonComplianceItems.UseCasePoints, "is missing ", new Session(), Project);
        //        }
        //    }
        //    else
        //    {
        //        if (Project.UseCasePoints.ApproveStatus != QAUseCasePoints.ApproveStatu.Ready)
        //        {
        //            emailbody += @"Use Case Points Is not Approved Please approve it";
        //            ucp = true;
        //            if (createNc == true)
        //            {
        //                CreateNCs((int)QANonComplains.NonComplianceItems.UseCasePoints, "is not approved ", new Session(), Project);
        //            }
        //        }
        //    }
        //    if (Project.UseCases.Count == 0)
        //    {
        //        emailbody += "@Use Cases is <font color='red'> missing </font> <br />";
        //        uc = true;
        //        if (createNc == true)
        //        {
        //            CreateNCs((int)QANonComplains.NonComplianceItems.UseCases, "is missing", new Session(), Project);
        //        }
        //    }
        //    else
        //    {
        //        foreach (QAUseCase existusecase in Project.UseCases)
        //        {
        //            if (existusecase.ApproveStatus != QAUseCase.ApproveStatu.Ready)
        //            {
        //                emailbody += string.Format("Use Case : '{0}' it's status is not ready<br />", existusecase.Name);
        //                uc = true;
        //                if (createNc == true)
        //                {
        //                    CreateNCs((int)QANonComplains.NonComplianceItems.UseCases, string.Format("\"{0}\" status is not ready", existusecase.Name), new Session(), Project);

        //                }
        //            }
        //        }
        //    }
        //    if (requirement || pe || uc || ucp || PSC)
        //    {

        //        string ToEmail = Project.ProjectOwner.CurrentEmailAddress;
        //        SendEmail(emailbody, "BaseLineNotification", ToEmail);
        //        Project.Session.BeginTransaction();
        //         Project.EmailSent = true;
        //         Project.Save();
        //         Project.Session.CommitTransaction();
        //    }
        //}

        #endregion

        //ATA Add four function to check each phase NC's Activitiy 1/29/2017[Start]
        public void firstmonthcheck(string emailbody, ProjectTemplate Project, bool createNc, Session Session)
        {
            string body = "firstmonthcheck start work now for project '" + Project.Name + "'";
            string emailto = "ahmed.t@ariany.com";
            SendEmail(body, "Base line Web Job", emailto);
            bool WBS = false;
            bool ucp = false;
            bool PSC = false;
            if (Project.UseCasePoints == null)
            {
                emailbody += "@UseCasePoints is <font color='red'> missing </font> <br />";
                ucp = true;
                if (createNc == true)
                {
                    CreateNCs((int)QANonComplains.NonComplianceItems.UseCasePoints, "is missing", Session, Project, null);
                }
            }
            else
            {
                //ATA remove the check on usecasepoints approvals cause of not required 3/20/2017[Start]
                //if (Project.UseCasePoints.ApproveStatus != QAUseCasePoints.ApproveStatu.Ready)
                //{
                //    emailbody += string.Format("UseCasePoints : '{0}' it's Status is not ready <br />", Project.UseCasePoints.Title);
                //    ucp = true;
                //    if (createNc == true)
                //    {
                //        CreateNCs((int)QANonComplains.NonComplianceItems.UseCasePoints, string.Format(" \"{0}\" Approval status is not ready", Project.UseCasePoints.Title), Session, Project, null);
                //    }
                //}
                //ATA remove the check on usecasepoints approvals cause of not required 3/20/2017[End]

                if (Project.UseCasePoints.WBS.Where(x => x.Month == 0).FirstOrDefault() == null)
                {
                    emailbody += "@Phase 0 is <font color='red'> missing in the project use case points </font> <br />";
                    WBS = true;
                    if (createNc == true)
                    {
                        CreateNCs((int)QANonComplains.NonComplianceItems.WBS, "Phase 0 is not created ", Session, Project, null);
                    }
                }
                else
                {
                    if (Project.UseCasePoints.WBS.Where(x => x.Month == 0).FirstOrDefault().AutoTaskID == 0)
                    {
                        emailbody += string.Format("Phase 0 is not created on auto task till now  <br />");
                        WBS = true;
                        if (createNc == true)
                        {
                            CreateNCs((int)QANonComplains.NonComplianceItems.WBS, string.Format(" \"{0}\" Phase 0 ", Project.UseCasePoints.Title), Session, Project, Project.UseCasePoints.WBS.Where(x => x.Month == 0).FirstOrDefault());
                        }
                    }
                }
            }
            if (Project.Risks.Count == 0)
            {
                emailbody += "@project Risks is <font color='red'> missing </font> <br />";
                PSC = true;
                if (createNc == true)
                {
                    CreateNCs((int)QANonComplains.NonComplianceItems.Risks, "is missing", Session, Project, null);
                }
            }
            if (Project.ManagementIssues.Count == 0)
            {
                emailbody += "@project Managment Issues is <font color='red'> missing </font> <br />";
                PSC = true;
                if (createNc == true)
                {
                    CreateNCs((int)QANonComplains.NonComplianceItems.ManagmentIssues, "is missing", Session, Project, null);
                }
            }

            //ATA add project scope to validation [Start]
            if (Project.Scope.Count == 0)
            {
                emailbody += "@project Scope is <font color='red'> missing </font> <br />";
                PSC = true;
                if (createNc == true)
                {
                    CreateNCs((int)QANonComplains.NonComplianceItems.ProjectScope, "is missing", Session, Project, null);
                }
            }
            else
            {
                if (Project.Scope.Where(x => x.Status == approvalstatus.Approved).Count() == 0)
                {

                    emailbody += "There are no project scop Approved yet";
                    PSC = true;
                    //if (createNc == true)
                    //{
                    //    CreateNCs((int)QANonComplains.NonComplianceItems.ProjectScope, string.Format("there is no project scop approved yet "), new Session(), Project,null);
                    //}
                }
            }
            if (Project.TestPlans.Count == 0)
            {
                emailbody += "@Test Plans is <font color='red'> missing </font> <br />";
                PSC = true;
                if (createNc == true)
                {
                    CreateNCs((int)QANonComplains.NonComplianceItems.TestPlan, "is missing", Session, Project, null);
                }
            }
            //ATA add project scope to validation [End]

            if (WBS || ucp || PSC)
            {

                string ToEmail = Project.ProjectOwner.CurrentEmailAddress;
                SendEmail(emailbody, "BaseLineNotification", ToEmail);
                //Project.Session.BeginTransaction();
                // Project.EmailSent = true;
                // Project.Save();
                // Project.Session.CommitTransaction();
            }
        }
        public void Secondmonthcheck(string emailbody, ProjectTemplate Project, bool createNc, Session Session)
        {
            string body = "Secondmonthcheck start work now for project '" + Project.Name + "'";
            string emailto = "ahmed.t@ariany.com";
            SendEmail(body, "Base line Web Job", emailto);
            bool requirement = false;
            bool uc = false;
            bool pe = false;
            bool ucp = false;
            bool PSC = false;
            bool WBS = false;
            bool Designtasks = false;
            int UCPcategorynumber = 0;
            if (Project.Requirements.Count == 0)
            {
                emailbody += "@Requirements is <font color='red'> missing </font> <br />";
                requirement = true;
                if (createNc == true)
                {
                    CreateNCs((int)QANonComplains.NonComplianceItems.Requirement, "is missing", Session, Project, null);
                }
            }
            else
            {
                foreach (Requirement existrequirement in Project.Requirements)
                {
                    if (existrequirement.ApproveStatus != Requirement.ApproveStatu.Ready)
                    {
                        emailbody += string.Format("Requirement : '{0}' it's Status is not ready <br />", existrequirement.Title);
                        requirement = true;
                        if (createNc == true)
                        {
                            CreateNCs((int)QANonComplains.NonComplianceItems.Requirement, string.Format(" \"{0}\" status is not ready", existrequirement.Title), Session, Project, existrequirement);
                        }
                    }
                }
            }
            if (Project.UseCasePoints != null)
            {
                if (Project.UseCasePoints.WBS.Where(x => x.Month == 1).FirstOrDefault() == null)
                {
                    emailbody += "@Phase 1 is <font color='red'> missing in the project use case points </font> <br />";
                    WBS = true;
                    if (createNc == true)
                    {
                        CreateNCs((int)QANonComplains.NonComplianceItems.WBS, "Phase 1 is not created ", Session, Project, null);
                    }
                }
                else
                {
                    if (Project.UseCasePoints.WBS.Where(x => x.Month == 1).FirstOrDefault().AutoTaskID == 0)
                    {
                        emailbody += string.Format("Phase 1 is not created on auto task till now  <br />");
                        WBS = true;
                        if (createNc == true)
                        {
                            CreateNCs((int)QANonComplains.NonComplianceItems.WBS, string.Format(" \"{0}\" Phase 1 ", Project.UseCasePoints.Title), Session, Project, Project.UseCasePoints.WBS.Where(x => x.Month == 1).FirstOrDefault());
                        }
                    }
                }
            }
            //ATA add project scope to validation [Start]
            if (Project.Scope.Count == 0)
            {
                emailbody += "@project Scope is <font color='red'> missing </font> <br />";
                PSC = true;
                if (createNc == true)
                {
                    CreateNCs((int)QANonComplains.NonComplianceItems.ProjectScope, "is missing", Session, Project, null);
                }
            }
            else
            {
                if (Project.Scope.Where(x => x.Status == approvalstatus.Approved).Count() == 0)
                {

                    emailbody += "There are no project scop Approved yet";
                    PSC = true;
                    if (createNc == true)
                    {
                        CreateNCs((int)QANonComplains.NonComplianceItems.ProjectScope, string.Format("There are no Project Scope Approved till now at least should be one Scope Approved"), Session, Project, null);
                    }
                }
            }
            //ATA add project scope to validation [End]
            if (Project.ProjectEntities.Count == 0)
            {
                emailbody += "@Project entities is <font color='red'> missing  </font><br />";
                pe = true;
                if (createNc == true)
                {
                    CreateNCs((int)QANonComplains.NonComplianceItems.ProjectEntities, "is missing", Session, Project, null);

                }
            }
            else
            {
                foreach (QAProjectEntity existprojectenity in Project.ProjectEntities)
                {
                    if (existprojectenity.ApproveStatus != QAProjectEntity.ApproveStatu.Ready)
                    {
                        emailbody += string.Format("Project Entity : '{0}' it's status is not ready<br />", existprojectenity.Name);
                        pe = true;
                        if (createNc == true)
                        {
                            CreateNCs((int)QANonComplains.NonComplianceItems.ProjectEntities, string.Format(" \"{0}\" status is not ready", existprojectenity.Name), Session, Project, existprojectenity);

                        }
                    }
                }
            }
            if (Project.UseCasePoints == null)
            {
                emailbody += "@Use Case Points is <font color='red'> missing </font><br />";
                ucp = true;
                if (createNc == true)
                {
                    CreateNCs((int)QANonComplains.NonComplianceItems.UseCasePoints, "is missing ", Session, Project, null);
                }
            }
            else
            {
                //if (Project.UseCasePoints.ApproveStatus != QAUseCasePoints.ApproveStatu.Ready)
                //{
                //    emailbody += @"Use Case Points Is not Approved Please approve it";
                //    ucp = true;
                //    if (createNc == true)
                //    {
                //        CreateNCs((int)QANonComplains.NonComplianceItems.UseCasePoints, "is not approved ", Session, Project, null);
                //    }
                //}
                foreach (QAUseCAsePointsCategory Category in Project.UseCasePoints.UseCAsePointsCategories)
                {
                    UCPcategorynumber += Category.Count;
                }
            }
            if (Project.UseCases.Count == 0)
            {
                emailbody += "@Use Cases is <font color='red'> missing </font> <br />";
                uc = true;
                if (createNc == true)
                {
                    CreateNCs((int)QANonComplains.NonComplianceItems.UseCases, "is missing", Session, Project, null);
                }
            }
            else
            {
                //ATA check or compare the use cases number and planned count 
                if (Project.UseCases.Count != UCPcategorynumber)
                {
                    emailbody += "@Use Cases Number is <font color='red'> Mismatch </font> with use case points category count <br />";
                    uc = true;
                    if (createNc == true)
                    {
                        CreateNCs((int)QANonComplains.NonComplianceItems.UseCases, "Use Cases Number is Mismatch with use case points category count ", Session, Project, null);
                    }
                }
                foreach (QAUseCase existusecase in Project.UseCases)
                {
                    if (existusecase.ApproveStatus != QAUseCase.ApproveStatu.Ready)
                    {
                        emailbody += string.Format("Use Case : '{0}' it's status is not ready<br />", existusecase.Name);
                        uc = true;
                        if (createNc == true)
                        {
                            CreateNCs((int)QANonComplains.NonComplianceItems.UseCases, string.Format("\"{0}\" status is not ready", existusecase.Name), Session, Project, existusecase);

                        }
                    }
                }
            }
            if (Project.TrackignTasks.Where(x => x.Task.Name.Contains("Design")).Count() == 0)
            {
                emailbody += "@Tasks Of Type Design is <font color='red'> Missing </font> <br />";
                Designtasks = true;
                if (createNc == true)
                {
                    CreateNCs((int)QANonComplains.NonComplianceItems.DesignTasks, "There are no design tasks created under this project ", Session, Project, null);
                }
            }
            if (requirement || pe || uc || ucp || PSC || WBS || Designtasks)
            {

                string ToEmail = Project.ProjectOwner.CurrentEmailAddress;
                SendEmail(emailbody, "BaseLineNotification", ToEmail);
                //Project.Session.BeginTransaction();
                // Project.EmailSent = true;
                // Project.Save();
                // Project.Session.CommitTransaction();
            }
        }
        public void Thirdmonthcheck(string emailbody, ProjectTemplate Project, bool createNc, Session Session)
        {
            string body = "Thirdmonthcheck start work now for project '" + Project.Name + "'";
            string emailto = "ahmed.t@ariany.com";
            SendEmail(body, "Base line Web Job", emailto);

            bool TC = false;
            bool WBS = false;
            bool DD = false;
            bool programmingtasks = false;
            if (Project.TestCases.Count == 0)
            {
                emailbody += "@Test Cases is <font color='red'> missing </font> <br />";
                TC = true;
                if (createNc == true)
                {
                    CreateNCs((int)QANonComplains.NonComplianceItems.TestCases, "is missing", Session, Project, null);
                }
            }
            else
            {
                foreach (TestCase testcase in Project.TestCases)
                {

                    if (testcase.IsOriginal == true && testcase.ApproveStatus != TestCase.ApproveStatu.Ready)
                    {
                        TC = true;
                        emailbody += string.Format("'{0}' status is not ready <br />", testcase.Title);
                        if (createNc == true)
                        {
                            CreateNCs((int)QANonComplains.NonComplianceItems.TestCases, string.Format(" \"{0}\" status is not ready", testcase.Title), Session, Project, testcase);
                        }
                    }

                }
            }

            if (Project.UseCasePoints != null)
            {
                if (Project.UseCasePoints.WBS.Where(x => x.Month == 2).FirstOrDefault() == null)
                {
                    emailbody += "@Phase 2 is <font color='red'> missing in the project use case points </font> <br />";
                    WBS = true;
                    if (createNc == true)
                    {
                        CreateNCs((int)QANonComplains.NonComplianceItems.WBS, "Phase 2 is not created ", Session, Project, null);
                    }
                }
                else
                {
                    if (Project.UseCasePoints.WBS.Where(x => x.Month == 2).FirstOrDefault().AutoTaskID == 0)
                    {
                        emailbody += string.Format("Phase 2 is not created on auto task till now  <br />");
                        WBS = true;
                        if (createNc == true)
                        {
                            CreateNCs((int)QANonComplains.NonComplianceItems.WBS, string.Format(" \"{0}\" Phase 2 ", Project.UseCasePoints.Title), Session, Project, Project.UseCasePoints.WBS.Where(x => x.Month == 2).FirstOrDefault());
                        }
                    }
                }
            }
            foreach (QAProjectEntity ProjectEntity in Project.ProjectEntities)
            {
                if (ProjectEntity.TrackingNumber != null)
                {
                    if (ProjectEntity.TrackingNumber.EntitySystemDesign.Count == 0)
                    {
                        emailbody += "@Detail Design is <font color='red'> missing </font> <br />";
                        DD = true;
                        if (createNc == true)
                        {
                            CreateNCs((int)QANonComplains.NonComplianceItems.DetailDesign, "is missing", Session, Project, null);
                        }
                    }
                    else
                    {
                        foreach (EntitySystemDesign DetailDesign in ProjectEntity.TrackingNumber.EntitySystemDesign)
                        {
                            if (DetailDesign.Status != EntitySystemDesign.StatusItems.Approved)
                            {
                                emailbody += string.Format("DetailDesign : '{0}' it's Status is not ready <br />", DetailDesign.MemberFullName);
                                DD = true;
                                if (createNc == true)
                                {
                                    CreateNCs((int)QANonComplains.NonComplianceItems.DetailDesign, string.Format(" \"{0}\" status is not ready", DetailDesign.MemberFullName), Session, Project, ProjectEntity);
                                }
                            }
                        }
                    }
                }
            }
            if (Project.TrackignTasks.Where(x => x.Task.Name.Contains("Programming")).Count() == 0)
            {
                emailbody += "@Tasks Of Type Programming is <font color='red'> Missing </font> <br />";
                programmingtasks = true;
                if (createNc == true)
                {
                    CreateNCs((int)QANonComplains.NonComplianceItems.ProgramingandTestingTasks, "There are no Programming tasks created under this project ", Session, Project, null);
                }
            }
            if (Project.TrackignTasks.Where(x => x.Task.Name.Contains("Testing")).Count() == 0)
            {
                emailbody += "@Tasks Of Type Testing is <font color='red'> Missing </font> <br />";
                programmingtasks = true;
                if (createNc == true)
                {
                    CreateNCs((int)QANonComplains.NonComplianceItems.ProgramingandTestingTasks, "There are no Testing tasks created under this project ", Session, Project, null);
                }
            }
            if (TC || WBS || DD || programmingtasks)
            {

                string ToEmail = Project.ProjectOwner.CurrentEmailAddress + ",quality@ariasystems.biz";
                SendEmail(emailbody, "BaseLineNotification", ToEmail);
                //Project.Session.BeginTransaction();
                // Project.EmailSent = true;
                // Project.Save();
                // Project.Session.CommitTransaction();
            }
        }
        public void Forthmonthcheck(string emailbody, ProjectTemplate Project, bool createNc, Session Session)
        {
            string body = "Forthmonthcheck start work now for project '" + Project.Name + "'";
            string emailto = "ahmed.t@ariany.com";
            SendEmail(body, "Base line Web Job", emailto);
            bool TT = false;
            bool TR = false;
            bool Defect = false;
            bool WBS = false;
            bool DD = false;
            bool createpackge = false;
            bool CheckIn = false;
            bool Technicaldoc = false;
            if (Project.TrackignTasks.Count > 0)
            {
                foreach (TrackingTask Task in Project.TrackignTasks)
                {
                    if (Task.Completedate == DateTime.MinValue || Task.Completedate == null)
                    {
                        TT = true;

                        if (createNc == true)
                        {
                            emailbody += string.Format("'{0}'  is not Completed yet <br />", Task.Tittle);
                            if (createNc == true)
                            {
                                CreateNCs((int)QANonComplains.NonComplianceItems.TrackingTask, string.Format(" \"{0}\" is not Completed yet", Task.Tittle), Session, Project, null);
                            }
                        }
                    }
                }
            }
            if (Project.TestRuns.Count == 0)
            {
                emailbody += "@Test Runs is <font color='red'> missing </font> <br />";
                TR = true;
                if (createNc == true)
                {
                    CreateNCs((int)QANonComplains.NonComplianceItems.TestRun, "is missing", Session, Project, null);
                }
            }

            if (Project.Defects.Count == 0)
            {
                emailbody += "@Defects is <font color='red'> missing </font> <br />";
                Defect = true;
                if (createNc == true)
                {
                    CreateNCs((int)QANonComplains.NonComplianceItems.Defects, "is missing", Session, Project, null);
                }
            }
            else
            {
                foreach (QADefect defect in Project.Defects)
                {

                    if (defect.Status != QADefect.DefectStatus.Completed)
                    {
                        Defect = true;
                        emailbody += string.Format("'{0}' Status is not completed <br />", defect.Title);
                        if (createNc == true)
                        {
                            CreateNCs((int)QANonComplains.NonComplianceItems.Defects, string.Format(" \"{0}\" status is not Completed", defect.Title), Session, Project, null);
                        }
                    }

                }
            }
            if (Project.UseCasePoints != null)
            {
                if (Project.UseCasePoints.WBS.Where(x => x.Month == 3).FirstOrDefault() == null)
                {
                    emailbody += "@Phase 3 is <font color='red'> missing in the project use case points </font> <br />";
                    WBS = true;
                    if (createNc == true)
                    {
                        CreateNCs((int)QANonComplains.NonComplianceItems.WBS, "Phase 3 is not created ", Session, Project, null);
                    }
                }
                else
                {
                    if (Project.UseCasePoints.WBS.Where(x => x.Month == 3).FirstOrDefault().AutoTaskID == 0)
                    {
                        emailbody += string.Format("Phase 3 is not created on auto task till now  <br />");
                        WBS = true;
                        if (createNc == true)
                        {
                            CreateNCs((int)QANonComplains.NonComplianceItems.WBS, string.Format(" \"{0}\" Phase 3 ", Project.UseCasePoints.Title), Session, Project, Project.UseCasePoints.WBS.Where(x => x.Month == 3).FirstOrDefault());
                        }
                    }
                    else
                    {
                        if (Project.UseCasePoints.WBS.Where(x => x.Month == 3).FirstOrDefault() != null && Project.UseCasePoints.WBS.Where(x => x.Month == 3).FirstOrDefault().QAActivities.Where(x => x.Activity == "Create Packaging & Release").FirstOrDefault() != null && Project.UseCasePoints.WBS.Where(x => x.Month == 3).FirstOrDefault().QAActivities.Where(x => x.Activity == "Create Packaging & Release").FirstOrDefault().CompletedDate == null)
                        {
                            WBS = true;
                            if (createNc == true)
                            {
                                CreateNCs((int)QANonComplains.NonComplianceItems.CreatePackage, string.Format("Create and release Package is not completed yet"), Session, Project, Project.UseCasePoints.WBS.Where(x => x.Month == 3).FirstOrDefault());
                            }
                        }
                    }
                }
            }
            if (Project.ProjectEntities.Count > 0)
            {
                foreach (QAProjectEntity Projectentity in Project.ProjectEntities)
                {
                    if (Projectentity.TrackingNumber != null)
                    {
                        if(Projectentity.TrackingNumber.AriaObjectShelves.Where(x=>x.CodeStatus != AriaObjectShelve.Status.CheckedIn).Count() > 0)
                        {
                            CheckIn = true;
                             if (createNc == true)
                            {
                                CreateNCs((int)QANonComplains.NonComplianceItems.CheckInFiles, string.Format("Create and release Package is not completed yet"),Session, Project, Projectentity);
                            }
                        }
                        if (Projectentity.TrackingNumber.TechnicalDocumentation == null)
                        {
                            Technicaldoc = true;
                            if (createNc == true)
                            {
                                CreateNCs((int)QANonComplains.NonComplianceItems.TechnicalDocumentation, string.Format("Technical Documentation for this Project entity '{0}' not uploaded ",Projectentity.Name), Session, Project, Projectentity);
                            }
                        }
                    }
                }
            }

            if (TT || TR || Defect || WBS || DD || createpackge|| CheckIn)
            {

                string ToEmail = Project.ProjectOwner.CurrentEmailAddress;
                SendEmail(emailbody, "BaseLineNotification", ToEmail);
                //Project.Session.BeginTransaction();
                // Project.EmailSent = true;
                // Project.Save();
                // Project.Session.CommitTransaction();
            }
        } 
        //ATA Add four function to check each phase NC's Activitiy 1/29/2017[End]




        public void SendEmail(string emailbody, string emailtitle, string emailTo)
        {
            Email notificationmail = new Email();
            // notificationmail.FromEmail = "khaled.m@ariasystems.biz";
            // notificationmail.EmailPassword = "Kamag@2016";

            notificationmail.FromEmail = "it@ariasystems.biz";
            notificationmail.EmailPassword = "Aria@5279";

            //notificationmail.FromEmail = "qua@ariasystems.biz";
            //notificationmail.EmailPassword = "quality_123";
            if (string.IsNullOrEmpty(emailTo.ToString()))
            {
                notificationmail.ToEmail = "ahmed.r@ariany.com";

            }
            else
            {
                notificationmail.ToEmail = emailTo.ToString();
            }
            emailbody = emailbody.Replace("@", System.Environment.NewLine);
            notificationmail.EmailBody = emailbody;
            notificationmail.EmailTitle = "Base line Notification ";
            notificationmail.EmailSubject = "Base line Notification ";
            notificationmail.SendEmail();
        }
        public void CreateNCs(int Object, string desc, Session Objectspace, ProjectTemplate project, BaseObject targetobject) 
        {
            QANonComplains newnoncompliance = new QANonComplains(Objectspace);
            Resources owner = Objectspace.FindObject<Resources>(CriteriaOperator.Parse("[Oid]= '" + project.ProjectOwner.Oid + "'"));
            ProjectTemplate Projecttemplate = Objectspace.FindObject<ProjectTemplate>(CriteriaOperator.Parse("[Oid]= '" + project.Oid + "'"));
            newnoncompliance.Description = desc;
            newnoncompliance.DueDate = DateTime.Now.Date;
            switch (((QANonComplains.NonComplianceItems)Object).ToString())
            {

                case "Requirement":
                    if (targetobject != null)
                    {
                        newnoncompliance.NonComplainActivity = Objectspace.FindObject<QAComplainActivity>(CriteriaOperator.Parse("[ActivityName] = 'Prepare UC’s Survey & Sizing'"));
                        newnoncompliance.Requirement = Objectspace.FindObject<Requirement>(CriteriaOperator.Parse("[Oid]= '" + targetobject.Oid + "'"));
                    }
                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    break;
                case "UseCases":
                    if (targetobject != null)
                    {
                        newnoncompliance.UseCase = Objectspace.FindObject<QAUseCase>(CriteriaOperator.Parse("[Oid]= '" + targetobject.Oid + "'"));
                    }
                    newnoncompliance.NonComplainActivity = Objectspace.FindObject<QAComplainActivity>(CriteriaOperator.Parse("[ActivityName] = 'Create Iteration Use Cases'"));
                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    break;
                case "ProjectEntities":
                    if (targetobject != null)
                    {
                        newnoncompliance.NonComplainActivity = Objectspace.FindObject<QAComplainActivity>(CriteriaOperator.Parse("[ActivityName] = 'Create Iteration Entities features & Create Tracking Entries(SRS Doc.)'"));
                        newnoncompliance.ProjectEntity = Objectspace.FindObject<QAProjectEntity>(CriteriaOperator.Parse("[Oid]= '" + targetobject.Oid + "'"));
                    }
                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    break;
                case "UseCasePoints":
                    newnoncompliance.NonComplainActivity = Objectspace.FindObject<QAComplainActivity>(CriteriaOperator.Parse("[ActivityName] = 'Prepare UC’s Survey & Sizing'"));
                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    break;
                case "TestCases":
                    if (targetobject != null)
                    {

                        newnoncompliance.TestCase = Objectspace.FindObject<TestCase>(CriteriaOperator.Parse("[Oid]= '" + targetobject.Oid + "'"));
                    }
                    newnoncompliance.NonComplainActivity = Objectspace.FindObject<QAComplainActivity>(CriteriaOperator.Parse("[ActivityName] = 'Create Iteration Testing Cases'"));

                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    // Objectspace.ExecuteQuery("insert into QANonComplains ([oid],[Owner],[RelatedProject],[Description],[NonComplainType],[Status],[NonComplianceObjects],[AlarmTime],[NotificationMessage]) Values ('" + Guid.NewGuid() + "','8A46A928-AE14-4C48-95DF-E226E256D695','" + project.Oid + "','" + desc + "','cbca1979-ec87-4f95-9321-c8b28c1bb177','" + Object + "','" + 1 + "','" + DateTime.Today + "','" + string.Format("Kindly note that you have Nc related to this project \"{0}\"", project.Name) + "')");
                    break;
                case "TestPlan":
                   
                 //   newnoncompliance.NonComplainActivity = Objectspace.FindObject<QAComplainActivity>(CriteriaOperator.Parse("[ActivityName] = 'Create Iteration Testing Cases'"));

                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    // Objectspace.ExecuteQuery("insert into QANonComplains ([oid],[Owner],[RelatedProject],[Description],[NonComplainType],[Status],[NonComplianceObjects],[AlarmTime],[NotificationMessage]) Values ('" + Guid.NewGuid() + "','8A46A928-AE14-4C48-95DF-E226E256D695','" + project.Oid + "','" + desc + "','cbca1979-ec87-4f95-9321-c8b28c1bb177','" + Object + "','" + 1 + "','" + DateTime.Today + "','" + string.Format("Kindly note that you have Nc related to this project \"{0}\"", project.Name) + "')");
                    break;
                case "Risks":
                    newnoncompliance.NonComplainActivity = Objectspace.FindObject<QAComplainActivity>(CriteriaOperator.Parse("[ActivityName] = 'Create Iteration Plan'"));
                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    break;
                case "ManagmentIssues":
                    newnoncompliance.NonComplainActivity = Objectspace.FindObject<QAComplainActivity>(CriteriaOperator.Parse("[ActivityName] = 'Create Iteration Plan'"));
                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    break;
                case "ProjectScope":
                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    break;
                case "TrackingTask":
                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    break;
                case "TestRun":
                    newnoncompliance.NonComplainActivity = Objectspace.FindObject<QAComplainActivity>(CriteriaOperator.Parse("[ActivityName] = 'Itration Programming & Unit Testing'"));

                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    break;
                case "Defects":
                    newnoncompliance.NonComplainActivity = Objectspace.FindObject<QAComplainActivity>(CriteriaOperator.Parse("[ActivityName] = 'Testing Feedback (Bug Fixing)'"));
                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    break;
                case "DetailDesign":
                    if (targetobject != null)
                    {

                        newnoncompliance.ProjectEntity = Objectspace.FindObject<QAProjectEntity>(CriteriaOperator.Parse("[Oid]= '" + targetobject.Oid + "'"));
                    }
                    newnoncompliance.NonComplainActivity = Objectspace.FindObject<QAComplainActivity>(CriteriaOperator.Parse("[ActivityName] = 'Create Iteration Detail Design'"));
                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    break;
                case "WBS":
                    if (targetobject != null)
                    {
                        newnoncompliance.WBS = Objectspace.FindObject<QAWBS>(CriteriaOperator.Parse("[Oid]= '" + targetobject.Oid + "'")); ;
                    }
                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    break;
                case "DesignTasks":
                    newnoncompliance.NonComplainActivity = Objectspace.FindObject<QAComplainActivity>(CriteriaOperator.Parse("[ActivityName] = 'Review M2 WBS (Detailed)'"));                    
                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    break;
                case"ProgramingandTestingTasks":
                    newnoncompliance.NonComplainActivity = Objectspace.FindObject<QAComplainActivity>(CriteriaOperator.Parse("[ActivityName] = 'Review M3 WBS (Detailed)'"));
                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    break;
                case "CreatePackage":
                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    break;
                case "CheckInFiles":
                    if (targetobject != null)
                    {
                        newnoncompliance.ProjectEntity = Objectspace.FindObject<QAProjectEntity>(CriteriaOperator.Parse("[Oid]= '" + targetobject.Oid + "'"));
                    }
                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    break;
                case "TechnicalDocumentation":
                     if (targetobject != null)
                    {
                        newnoncompliance.ProjectEntity = Objectspace.FindObject<QAProjectEntity>(CriteriaOperator.Parse("[Oid]= '" + targetobject.Oid + "'"));
                    }
                    saveNcs(Object, newnoncompliance, owner, Projecttemplate, Objectspace);
                    break;
                default:
                    break;
            }
            //if (Object == (int)QANonComplains.NonComplianceItems.TestCases)
            //{

            //}
            //else
            //{

            //  //Objectspace.ExecuteQuery("insert into QANonComplains ([oid],[Owner],[RelatedProject],[Description],[NonComplainType],[Status],[NonComplianceObjects],[AlarmTime],[NotificationMessage]) Values ('" + Guid.NewGuid() + "','" + project.ProjectOwner.Oid + "','" + project.Oid + "','" + desc + "','cbca1979-ec87-4f95-9321-c8b28c1bb177','" + Object + "','" + 1 + "','" + DateTime.Today + "','" + string.Format("Kindly note that you have Nc related to this project \"{0}\"", project.Name) + "')");
            //}
            
        }

        //ATA Add this function to collect the saving function for each nc 
        public void saveNcs(int Object, QANonComplains newnoncompliance, Resources Owner, ProjectTemplate Projecttemplate, Session Objectspace)
        {
            newnoncompliance.Owner = Owner;
            newnoncompliance.RelatedProject = Projecttemplate;
            newnoncompliance.NonComplianceObjects = (QANonComplains.NonComplianceItems)Object;
            newnoncompliance.NonComplainType = Objectspace.FindObject<QAComplainType>(CriteriaOperator.Parse("[Oid] = 'cbca1979-ec87-4f95-9321-c8b28c1bb177'"));
            newnoncompliance.AlarmTime = DateTime.Today;
            newnoncompliance.NotificationMessage = string.Format("Kindly note that you have Nc related to this project \"{0}\"", Projecttemplate.Name);
            newnoncompliance.Status = QANonComplains.ComplainStatus.New;
            newnoncompliance.Save();
        }
 

        //ATA add this function to check Nc's Solving 1/30/2017 [Start] 
        public void CheckNCssolved(ProjectTemplate project)
        {
            foreach (QANonComplains NonCompliance in project.NonComplians)
            {
                switch (NonCompliance.NonComplianceObjects.ToString())
                {

                    case "Requirement":
                        if (NonCompliance.Requirement == null)
                        {
                            if (project.Requirements.Count > 0 && project.Requirements.Where(x=>x.ApproveStatus == Requirement.ApproveStatu.New).Count() == 0)
                            {
                                NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                                NonCompliance.NotificationMessage = "";
                                NonCompliance.AlarmTime = null;
                                NonCompliance.Save();

                            }
                        }
                        else
                        {
                            if (NonCompliance.Requirement.ApproveStatus == Requirement.ApproveStatu.Ready)
                            {
                                NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                                NonCompliance.NotificationMessage = " ";
                                NonCompliance.AlarmTime = null;
                                NonCompliance.Save();

                            }
                        }
                        break;
                    case "UseCases":
                        if (NonCompliance.UseCase == null)
                        {
                            int UCPNUM = 0;
                            if (project.UseCasePoints != null)
                            {
                                foreach (QAUseCAsePointsCategory cat in project.UseCasePoints.UseCAsePointsCategories)
                                {
                                    UCPNUM += cat.Count;
                                }
                                if (project.UseCases.Count > 0 && project.UseCases.Where(x => x.ApproveStatus == QAUseCase.ApproveStatu.New).Count() == 0 && project.UseCases.Count == UCPNUM)
                                {
                                    NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                                    NonCompliance.NotificationMessage = " ";
                                    NonCompliance.AlarmTime = null;
                                    NonCompliance.Save();

                                }
                            }
                            
                        }
                        else
                        {
                            if (NonCompliance.UseCase.ApproveStatus == QAUseCase.ApproveStatu.Ready)
                            {
                                NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                                NonCompliance.NotificationMessage = " ";
                                NonCompliance.AlarmTime = null;
                                NonCompliance.Save();

                            }
                        }
                        break;
                    case "ProjectEntities":
                        if (NonCompliance.ProjectEntity == null)
                        {
                            if (project.ProjectEntities.Count > 0 && project.ProjectEntities.Where(x => x.ApproveStatus == QAProjectEntity.ApproveStatu.New).Count() == 0)
                            {
                                NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                                NonCompliance.NotificationMessage = " ";
                                NonCompliance.AlarmTime = null;
                                NonCompliance.Save();

                            }
                        }
                        else
                        {
                            if (NonCompliance.ProjectEntity.ApproveStatus == QAProjectEntity.ApproveStatu.Ready)
                            {
                                NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                                NonCompliance.NotificationMessage = " ";
                                NonCompliance.AlarmTime = null;
                                NonCompliance.Save();

                            }
                        }
                        break;
                    case "UseCasePoints":
                        if (project.UseCasePoints != null) //&& project.UseCasePoints.ApproveStatus == QAUseCasePoints.ApproveStatu.Ready)
                        {
                            NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                            NonCompliance.NotificationMessage = " ";
                            NonCompliance.AlarmTime = null;
                            NonCompliance.Save();
                        }
                        break;
                    case "TestCases":
                        if (NonCompliance.TestCase == null)
                        {
                            if (project.TestCases.Where(x=>x.IsOriginal == true).Count() > 0 && project.TestCases.Where(x=>x.IsOriginal == true).Where(i=>i.ApproveStatus == TestCase.ApproveStatu.New).Count() == 0)
                            {
                                NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                                NonCompliance.NotificationMessage = " ";
                                NonCompliance.AlarmTime = null;
                                NonCompliance.Save();

                            }
                        }
                        else
                        {
                            if (NonCompliance.TestCase.ApproveStatus == TestCase.ApproveStatu.Ready)
                            {
                                NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                                NonCompliance.NotificationMessage = " ";
                                NonCompliance.AlarmTime = null;
                                NonCompliance.Save();

                            }
                        }
                        break;
                    case "TestPlan":
                        if (project.TestPlans.Count > 0)
                        {
                            NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                            NonCompliance.NotificationMessage = " ";
                            NonCompliance.AlarmTime = null;
                            NonCompliance.Save();
                        }
                        break;
                    case "Risks":
                        if (project.Risks.Count > 0)
                        {
                            NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                            NonCompliance.NotificationMessage = " ";
                            NonCompliance.AlarmTime = null;
                            NonCompliance.Save();
                        }
                        break;
                    case "ManagmentIssues":
                        if (project.ManagementIssues.Count > 0)
                        {
                            NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                            NonCompliance.NotificationMessage = " ";
                            NonCompliance.AlarmTime = null;
                            NonCompliance.Save();
                        }
                        break;
                    case "ProjectScope":
                        if (project.Scope.Count > 0 && project.Scope.Where(S=>S.Status == approvalstatus.Approved).Count() > 0)
                        {
                            NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                            NonCompliance.NotificationMessage = " ";
                            NonCompliance.AlarmTime = null;
                            NonCompliance.Save();
                        }
                        break;
                    case "TrackingTask":
                        if (project.TrackignTasks.Count > 0)
                        {
                            if (project.TrackignTasks.Where(x => x.Completedate == null).Count() == 0)
                            {
                                NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                                NonCompliance.NotificationMessage = " ";
                                NonCompliance.AlarmTime = null;
                                NonCompliance.Save();
                            }

                        }
                        break;
                    case "TestRun":
                        if (project.TestRuns.Count > 0)
                        {
                            NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                            NonCompliance.NotificationMessage = " ";
                            NonCompliance.AlarmTime = null;
                            NonCompliance.Save();
                        }
                        break;
                    case "Defects":
                        if (project.Defects.Count > 0)
                        {
                            if (project.Defects.Where(i => i.Status != QADefect.DefectStatus.Completed).Count() == 0)
                            {
                                NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                                NonCompliance.NotificationMessage = " ";
                                NonCompliance.AlarmTime = null;
                                NonCompliance.Save();
                            }
                        }
                        break;
                    case "DetailDesign":
                        if (NonCompliance.ProjectEntity != null)
                        {
                            if (NonCompliance.ProjectEntity.EntitySystemDesign.Count > 0 && NonCompliance.ProjectEntity.EntitySystemDesign.Where(y => y.Status != EntitySystemDesign.StatusItems.Approved).Count() == 0)
                            {
                                NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                                NonCompliance.NotificationMessage = " ";
                                NonCompliance.AlarmTime = null;
                                NonCompliance.Save();
                            }
                        }
                        break;
                    case "WBS":
                        if (NonCompliance.WBS != null)
                        {
                            if (NonCompliance.WBS.AutoTaskID > 0)
                            {
                                NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                                NonCompliance.NotificationMessage = " ";
                                NonCompliance.AlarmTime = null;
                                NonCompliance.Save();
                            }
                        }
                        break;
                    case "DesignTasks":
                       
                            if (project.TrackignTasks.Where(x=>x.Task.Name.Contains("Design")).Count() == project.ProjectEntities.Count)
                            {
                                NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                                NonCompliance.NotificationMessage = " ";
                                NonCompliance.AlarmTime = null;
                                NonCompliance.Save();
                            }
                        break;
                    case "ProgramingandTestingTasks":

                        if (project.TrackignTasks.Where(x => x.Task.Name.Contains("Programming")).Count() == project.ProjectEntities.Count && project.TrackignTasks.Where(x => x.Task.Name.Contains("Testing")).Count() == project.ProjectEntities.Count)
                            {
                                NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                                NonCompliance.NotificationMessage = " ";
                                NonCompliance.AlarmTime = null;
                                NonCompliance.Save();
                            }
                        break;
                    case "CreatePackage":
                        if (project.UseCasePoints.WBS.Where(x => x.Month == 3).FirstOrDefault() != null && project.UseCasePoints.WBS.Where(x => x.Month == 3).FirstOrDefault().QAActivities.Where(x => x.Activity == "Create Packaging & Release").FirstOrDefault() != null && project.UseCasePoints.WBS.Where(x => x.Month == 3).FirstOrDefault().QAActivities.Where(x => x.Activity == "Create Packaging & Release").FirstOrDefault().CompletedDate != null)
                        {
                            NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                            NonCompliance.NotificationMessage = " ";
                            NonCompliance.AlarmTime = null;
                            NonCompliance.Save();
                        }
                        break;
                    case "CheckInFiles":
                        if (NonCompliance.ProjectEntity != null && NonCompliance.ProjectEntity.TrackingNumber != null)
                        {
                            if (NonCompliance.ProjectEntity.TrackingNumber.AriaObjectShelves.Where(x => x.CodeStatus != AriaObjectShelve.Status.CheckedIn).Count() == 0)
                            {
                                NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                                NonCompliance.NotificationMessage = " ";
                                NonCompliance.AlarmTime = null;
                                NonCompliance.Save();
                            }
                        }
                        break;
                    case"TechnicalDocumentation":
                        if (NonCompliance.ProjectEntity != null && NonCompliance.ProjectEntity.TrackingNumber != null)
                        {
                            if (NonCompliance.ProjectEntity.TrackingNumber.TechnicalDocumentation != null)
                            {
                                NonCompliance.Status = QANonComplains.ComplainStatus.Completed;
                                NonCompliance.NotificationMessage = " ";
                                NonCompliance.AlarmTime = null;
                                NonCompliance.Save();
                            }
                        }
                        break;
                    default:
                        break;

                }
            }
        }
        //ATA add this function to check Nc's Solving 1/30/2017 [End] 
    }
}
