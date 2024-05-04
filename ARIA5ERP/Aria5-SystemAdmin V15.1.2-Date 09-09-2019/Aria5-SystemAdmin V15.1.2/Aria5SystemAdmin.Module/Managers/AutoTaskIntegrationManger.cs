 using Aria5SystemAdmin.Module.SubAutoTask1;
using System;
using System.Collections.Generic;
using System.Linq;
using System.ServiceModel;
using System.Text;
using System.Threading.Tasks;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.ExpressApp;
using System.Windows.Forms;
using DevExpress.Data.Filtering;
using System.Text.RegularExpressions;
using DevExpress.Xpo;
using DevExpress.ExpressApp.Web;
using System.Net;

namespace Aria5SystemAdmin.Module.Managers
{
    public class AutoTaskIntegrationManger
    {

        # region Declarations
        
        private string auth_user_id ="IT@ariasystems.biz"; // user@domain.com
        private string auth_user_password = "Aria@2016";
        private ATWSZoneInfo zoneInfo = null;
        private BasicHttpBinding myBinding;
        public static Dictionary<string, string> ResourceName = new Dictionary<string, string>();
        public static Dictionary<string, string> ResourceID = new Dictionary<string, string>();
        public static Dictionary<string, string> LastName = new Dictionary<string, string>();
        public static Dictionary<string, string> UserName = new Dictionary<string, string>();
        AutotaskIntegrations at_integrations = new AutotaskIntegrations();
        SubAutoTask1.ATWSSoapClient clientAuto = new SubAutoTask1.ATWSSoapClient();
        private string[,] DepartementsList = new string[150, 2];
        public string[,] resourcesList = new string[150, 6];
        private string[,] resourcesRoles = new string[1000, 2];
        #endregion
        # region Constructor

        //  Sets Connection with Autotask 
        public AutoTaskIntegrationManger()
        {
            clientAuto = new SubAutoTask1.ATWSSoapClient();
            System.Net.ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12;
            zoneInfo = clientAuto.getZoneInfo(auth_user_id);
            clientAuto = new ATWSSoapClient();
            myBinding = new BasicHttpBinding();
            myBinding.Security.Mode = BasicHttpSecurityMode.Transport;
            myBinding.Security.Transport.ClientCredentialType = HttpClientCredentialType.Basic;
            myBinding.MaxReceivedMessageSize = 2147483647;
            Uri myUri = new Uri("https://webservices.autotask.net/atservices/1.5/atws.asmx", UriKind.Absolute);
            //EndpointAddress ea = new EndpointAddress(myUri);//(zoneInfo.URL);
            EndpointAddress ea = new EndpointAddress(zoneInfo.URL);
            clientAuto = new ATWSSoapClient(myBinding, ea);
            clientAuto.ClientCredentials.UserName.UserName = auth_user_id;
            clientAuto.ClientCredentials.UserName.Password = auth_user_password;
        }
        #endregion
        # region Project  Creation and Update

        #region Commentedarea
        //public long CreateNewAutoTaskProject(ProjectTemplate Project)
        //{
        //    AutotaskIntegrations at_integrations = new AutotaskIntegrations();
        //    //StringBuilder sb = new StringBuilder();
        //    //sb.Append("<queryxml><entity>ResourceRole</entity>").Append(System.Environment.NewLine);
        //    //sb.Append("<query><field>ResourceID<expression op=\"Equals\">29834747</expression></field></query>").Append(System.Environment.NewLine);
        //    //sb.Append("</queryxml>").Append(System.Environment.NewLine);
        //    //var r2p = clientAuto.query(at_integrations, sb.ToString());
        //    //if (r2p.ReturnCode == 1)
        //    //{ }
        //    Project newProject = new Project();
        //    newProject.AccountID = 18894566;//Aria In house
        //    newProject.StartDateTime = Project.StartDate;
        //    newProject.EndDateTime = Project.EndDate;
        //    newProject.ProjectName = Project.Name;
        //    newProject.ProjectLeadResourceID = 29834747;//PMO
        //    newProject.Status = 1;
        //    newProject.Type = 4;
        //    ATWSResponse ResponseAuto2;
        //    SubAutoTask1.Entity[] entArr2 = new SubAutoTask1.Entity[1];
        //    entArr2[0] = newProject;
        //    ResponseAuto2 = clientAuto.create(at_integrations, entArr2);
        //    if (ResponseAuto2.ReturnCode == 1)
        //    {
        //        Project.AutoTaskID = ResponseAuto2.EntityReturnInfoResults[0].EntityId;
        //        Project.Save();
        //        Project.Session.CommitTransaction();
        //        //DevExpress.XtraEditors.XtraMessageBox.Show(" Project created and it's ID is '" + Project.AutoTaskID + "'");
        //        if (Project.AutoTaskID != null || Project.AutoTaskID != 0)
        //        {
        //            CreateNewPhase(Project.AutoTaskID, Project.UseCasePoints);
        //            CreateActivity(Project.AutoTaskID, Project.UseCasePoints);
        //        }
        //    }
        //    return Project.AutoTaskID; // Project  AutoTask ID
        //}
        //public long CreateNewPhase(long ProjectAutotaskID, QAUseCasePoints USP)
        //{
        //    foreach (QAWBS Phase in USP.WBS)
        //    {
        //        if (Phase.AutoTaskID == 0)
        //        {
        //            string phaseName = "Phase M" + Phase.Month;
        //            Phase newPhase = new Phase();
        //            newPhase.Title = phaseName;
        //            newPhase.ProjectID = ProjectAutotaskID;
        //            newPhase.CreateDate = DateTime.Now.Date;
        //            newPhase.Description = phaseName;
        //            newPhase.StartDate = DateTime.Now.Date;
        //            //newPhase.DueDate = DateTime.Now.Date;
        //            switch (Phase.Month)
        //            {
        //                case 0:
        //                    newPhase.DueDate = CalcEndate.CalcEnddate(DateTime.Now.Date, 1);
        //                    break;
        //                case 1:
        //                    newPhase.DueDate = CalcEndate.CalcEnddate(DateTime.Now.Date, 2);
        //                    break;
        //                case 2:
        //                    newPhase.DueDate = CalcEndate.CalcEnddate(DateTime.Now.Date, 3);
        //                    break;
        //                case 3:
        //                    newPhase.DueDate = CalcEndate.CalcEnddate(DateTime.Now.Date, 4);
        //                    break;
        //            }
        //            ATWSResponse ResponseAuto;
        //            SubAutoTask1.Entity[] entArr = new SubAutoTask1.Entity[1];
        //            entArr[0] = (SubAutoTask1.Entity)newPhase;
        //            ResponseAuto = clientAuto.create(at_integrations, entArr);
        //            if (ResponseAuto.ReturnCode == 1)
        //            {
        //                Phase.AutoTaskID = ResponseAuto.EntityReturnInfoResults[0].EntityId;
        //                Phase.Save();
        //                Phase.Session.CommitTransaction();
        //            }
        //            else
        //            {

        //            }
        //        }
        //    }
        //    return 0;
        //    // AutoTask ID
        //}
        #endregion 
        public long CreateNewAutoTaskProject(ProjectTemplate Project)
        {
            AutotaskIntegrations at_integrations = new AutotaskIntegrations();
            //StringBuilder sb = new StringBuilder();
            //sb.Append("<queryxml><entity>ResourceRole</entity>").Append(System.Environment.NewLine);
            //sb.Append("<query><field>ResourceID<expression op=\"Equals\">29834747</expression></field></query>").Append(System.Environment.NewLine);
            //sb.Append("</queryxml>").Append(System.Environment.NewLine);
            //var r2p = clientAuto.query(at_integrations, sb.ToString());
            //if (r2p.ReturnCode == 1)
            //{ }
            Project newProject = new Project();
            newProject.AccountID = 18894566;//Aria In house
            newProject.StartDateTime = Project.StartDate;
            newProject.EndDateTime = Project.EndDate;
            newProject.ProjectName = Project.Name;
            newProject.ProjectLeadResourceID = Project.ProjectOwner.AutoTaskID;//PMO
            newProject.Status = 1;
            newProject.Type = 4;
            ATWSResponse ResponseAuto2;
            SubAutoTask1.Entity[] entArr2 = new SubAutoTask1.Entity[1];
            entArr2[0] = newProject;
            ResponseAuto2 = clientAuto.create(at_integrations, entArr2);
            if (ResponseAuto2.ReturnCode == 1)
            {
                Project.AutoTaskID = ResponseAuto2.EntityReturnInfoResults[0].EntityId;
                Project.Save();
                Project.Session.CommitTransaction();
                //DevExpress.XtraEditors.XtraMessageBox.Show(" Project created and it's ID is '" + Project.AutoTaskID + "'");
                if (Project.AutoTaskID != null || Project.AutoTaskID != 0)
                {
                    //ATA 1/9/2017 Create just Phase 0 for the project as per SPIG enhancment [Start]
                    QAWBS Phase = Project.Session.FindObject<QAWBS>(CriteriaOperator.Parse("[Month] = 0 and [Project] = '" + Project.Oid + "' "));
                    if (Phase != null)
                    {
                        CreateNewPhasenew(Project.AutoTaskID, Phase);
                    }
                    //CreateNewPhase(Project.AutoTaskID, Project.UseCasePoints);
                   // CreateActivity(Project.AutoTaskID, Project.UseCasePoints);
                    //ATA 1/9/2017 Create just Phase 0 for the project as per SPIG enhancment [End]

                }
            }
            return Project.AutoTaskID; // Project  AutoTask ID
        }
        /// <summary>
        /// update the project data from system admin to autotask 
        /// </summary>
        /// <param name="Project"></param>
        /// <returns></returns>
        public int UpdateAutoTaskProject(ProjectTemplate Project,IObjectSpace Objectspace)
        {
            StringBuilder SelectProject = new StringBuilder();
            SelectProject.Append("<queryxml><entity>Project</entity>").Append(System.Environment.NewLine);
            SelectProject.Append("<query><field>id<expression op=\"Equals\">" + Project.AutoTaskID + "</expression></field></query>").Append(System.Environment.NewLine);
            SelectProject.Append("</queryxml>").Append(System.Environment.NewLine);
            var completion = clientAuto.query(at_integrations, SelectProject.ToString());
            if (completion.ReturnCode == 1)
            {
                foreach (Project Project1 in completion.EntityResults)
                {
                    Project1.ProjectName = Project.Name;
                    Project1.StartDateTime = Project.StartDate;
                    Project1.EndDateTime = Project.EndDate;
                    Project1.Status = Project.Status;
                    Project1.ProjectLeadResourceID = Project.ProjectOwner.AutoTaskID;//PMO
                    ATWSResponse ResponseAuto2;
                    SubAutoTask1.Entity[] entArr2 = new SubAutoTask1.Entity[1];
                    entArr2[0] = Project1;
                    ResponseAuto2 = clientAuto.update(at_integrations, entArr2);
                    // var estimate = Regex.Match(((Project)Project1).EstimatedTime.ToString(), @"\d+").Value;
                   // var actuall = Regex.Match(((Project)Project1).ActualHours.ToString(), @"\d+").Value;
                   // var division = (double)int.Parse(actuall) / (double)int.Parse(estimate);
                   ////  Project.Completness = int.Parse(((((Project)Project1).EstimatedTime.ToString()).Replace("M", "")).Trim()) - int.Parse(((((Project)Project1).ActualHours.ToString()).Replace("M", "")).Trim());
                   // if (double.Parse(Regex.Match(((Project)Project1).EstimatedTime.ToString(), @"\d+").Value) != 0)
                   // {
                   //     Project.Completness = (double.Parse(Regex.Match(((Project)Project1).ActualHours.ToString(), @"\d+").Value) / double.Parse(Regex.Match(((Project)Project1).EstimatedTime.ToString(), @"\d+").Value)) * 100;
                   //     Project.Save();
                   //     Project.Session.CommitTransaction();
                   // }
                }
            }

            foreach (QAWBS Wbs in Project.UseCasePoints.WBS)
            {
                if (Wbs.Month == 1)
                {
                    foreach (QAActivity Activity in Wbs.QAActivities)
                    {
                        //if (Activity.Activity == "Create Iteration Entities features (SRS Doc.)")
                        if(Activity.Activity.Contains("SRS"))
                        {

                            StringBuilder Selecttask = new StringBuilder();
                            Selecttask.Append("<queryxml><entity>TimeEntry</entity>").Append(System.Environment.NewLine);
                            Selecttask.Append("<query><field>TaskID<expression op=\"Equals\">" + Activity.AutoTaskID + "</expression></field></query>").Append(System.Environment.NewLine);
                            Selecttask.Append("</queryxml>").Append(System.Environment.NewLine);
                            var TaskActual = clientAuto.query(at_integrations, Selecttask.ToString());
                            if (TaskActual.ReturnCode == 1)
                            {
                                var Entityspecification = 0;
                                foreach (var timeentry in TaskActual.EntityResults)
                                {
                                    Entityspecification += int.Parse(Regex.Match(((TimeEntry)timeentry).HoursWorked.ToString(), @"\d+").Value);

                                }
                                Project.EntitySpecificationEstimation = Activity.AvgEstVal;
                                Project.EntitySpecificationActual = Entityspecification;
                                Project.Save();
                                Project.Session.CommitTransaction();
                            }

                        }
                    }
                }
            }

            //ATA check all takss that not under specific phase at autotask 
            StringBuilder SelectProjecttasks = new StringBuilder();
            SelectProjecttasks.Append("<queryxml><entity>Task</entity>").Append(System.Environment.NewLine);
            SelectProjecttasks.Append("<query><Condition><field>PhaseID<expression op=\"IsNull\"></expression></field></Condition>").Append(System.Environment.NewLine);
            SelectProjecttasks.Append("<Condition><field>ProjectID<expression op=\"Equals\">" + Project.AutoTaskID + "</expression></field></Condition></query>").Append(System.Environment.NewLine);            
            SelectProjecttasks.Append("</queryxml>").Append(System.Environment.NewLine);
            var Selecttasks = clientAuto.query(at_integrations, SelectProjecttasks.ToString());
            if (Selecttasks.ReturnCode == 1)
            {
                foreach (Aria5SystemAdmin.Module.SubAutoTask1.Task Task in Selecttasks.EntityResults)
                {
                    QAWBS mislenusphase = Objectspace.FindObject<QAWBS>(CriteriaOperator.Parse("[Month] = 9 and [UseCasePoints] = '"+Project.UseCasePoints.Oid+"'"));
                    if (mislenusphase == null)
                    {
                        string phaseName = "Mislenus" ;
                        Phase newPhase = new Phase();
                        newPhase.Title = phaseName;
                        newPhase.ProjectID = Project.AutoTaskID;
                        newPhase.CreateDate = DateTime.Now.Date;
                        newPhase.Description = "phase for the tasks that created on this project from auto task and not created on system admin";
                        ATWSResponse ResponseAuto;
                        SubAutoTask1.Entity[] entArr = new SubAutoTask1.Entity[1];
                        entArr[0] = (SubAutoTask1.Entity)newPhase;
                        ResponseAuto = clientAuto.create(at_integrations, entArr);
                        if (ResponseAuto.ReturnCode == 1)
                        {
                            mislenusphase = Objectspace.CreateObject<QAWBS>();
                            mislenusphase.AutoTaskID = ResponseAuto.EntityReturnInfoResults[0].EntityId;
                            mislenusphase.Month = 9;
                            mislenusphase.UseCasePoints = Project.UseCasePoints;
                            mislenusphase.Save();
                            mislenusphase.Session.CommitTransaction();
                           
                            //Create all activity that related to this phase 1/9/2017  
                        }

                    }
                    Task.PhaseID = mislenusphase.AutoTaskID;
                    SubAutoTask1.Entity[] Tasks = new SubAutoTask1.Entity[1];
                    Tasks[0] = Task;
                    ATWSResponse ResponseAuto1 = clientAuto.update(at_integrations, Tasks);
                    if (ResponseAuto1.ReturnCode == 1)
                    {
                        QAActivity newactivity = Objectspace.CreateObject<QAActivity>();
                        newactivity.Activity = Task.Title.ToString();
                        newactivity.AvgEstVal = int.Parse(Regex.Match(Task.EstimatedHours.ToString(), @"\d+").Value);
                        newactivity.AutoTaskID = long.Parse(Task.id.ToString());
                        newactivity.QAWBS = mislenusphase;
                        newactivity.Save();
                        Objectspace.CommitChanges();
                        //  DevExpress.XtraEditors.XtraMessageBox.Show("'" + ResponseAuto1.EntityResults.Length + "' Tasks Updated");
                    }
                    
                }
            }
            // DevExpress.XtraEditors.XtraMessageBox.Show(" Project Updated ");
            return 0;// AutoTask ID
        }

       

        #endregion
        # region Phases Creation and Update

        //ATA add new phase by phase not all phases at one time 1/9/2017 [start]
        public void CreateNewPhasenew(long ProjectAutotaskID, QAWBS Phase)
        {
            // StringBuilder sb = new StringBuilder();
            //sb.Append("<queryxml><entity>Task</entity>").Append(System.Environment.NewLine);
            //sb.Append("<query><field>ProjectID<expression op=\"Equals\">" + ProjectAutotaskID + "</expression></field></query>").Append(System.Environment.NewLine);
            //sb.Append("</queryxml>").Append(System.Environment.NewLine);
            //var Autotask_response = clientAuto.query(at_integrations, sb.ToString());
            //if (Autotask_response.ReturnCode == 1)
            //{
            //    if (Autotask_response.EntityResults.Length > 0)
            //    {
            //        foreach (Aria5SystemAdmin.Module.SubAutoTask1.Task PhaseResponse in Autotask_response.EntityResults)
            //        {
            //            if (int.Parse(PhaseResponse.Status.ToString()) != 5)
            //            {
            //                QAWBS Taskphase = Phase.Session.FindObject<QAWBS>(CriteriaOperator.Parse("[AutoTaskID]= '" + double.Parse(PhaseResponse.PhaseID.ToString()) + "'"));
            //                throw new Exception("Please complete phase unmber '" + Taskphase + "' to be able to create this phase ");
            //            }
            //        }
            //    }
            //}
                if (Phase.AutoTaskID == 0)
                {
                    string phaseName = "Phase M" + Phase.Month;
                    if (!string.IsNullOrEmpty(Phase.PhaseTitle))
                        phaseName = Phase.PhaseTitle;
                    Phase newPhase = new Phase();
                    newPhase.Title = phaseName;
                    newPhase.ProjectID = ProjectAutotaskID;
                    newPhase.CreateDate = DateTime.Now.Date;
                    newPhase.Description = phaseName;
                    if (Phase.StartDate == DateTime.MinValue)
                    {
                        newPhase.StartDate = DateTime.Now.Date;
                    }
                    else
                    {
                        newPhase.StartDate = Phase.StartDate;
                    }
                   //ATA 9/27/2017 enhancement after adding end date for each phase in system admin
                    if (Phase.EndDate < Phase.StartDate || Phase.EndDate == DateTime.MinValue)
                    {
                        newPhase.DueDate = CalcEndate.CalcEnddate(DateTime.Today, 1);
                    }
                    else
                    {
                        newPhase.DueDate = Phase.EndDate;
                    }
                    //newPhase.DueDate = DateTime.Now.Date;
                    //switch (Phase.Month)
                    //{
                    //    case 0:
                            
                    //        break;
                    //    case 1:
                    //        newPhase.DueDate = CalcEndate.CalcEnddate(DateTime.Now.Date, 2);
                    //        break;
                    //    case 2:
                    //        newPhase.DueDate = CalcEndate.CalcEnddate(DateTime.Now.Date, 3);
                    //        break;
                    //    case 3:
                    //        newPhase.DueDate = CalcEndate.CalcEnddate(DateTime.Now.Date, 4);
                    //        break;
                    //}
                    ATWSResponse ResponseAuto;
                    SubAutoTask1.Entity[] entArr = new SubAutoTask1.Entity[1];
                    entArr[0] = (SubAutoTask1.Entity)newPhase;
                    ResponseAuto = clientAuto.create(at_integrations, entArr);
                    if (ResponseAuto.ReturnCode == 1)
                    {
                        Phase.AutoTaskID = ResponseAuto.EntityReturnInfoResults[0].EntityId;
                        Phase.Save();
                        Phase.Session.CommitTransaction();
                        //Create all activity that related to this phase 1/9/2017  
                        CreateActivityNew(ProjectAutotaskID, Phase);
                    }
                    else
                    {
                        throw new Exception("There are error happen while creating the phase in autotask API ");
                    }
             
            }
           // return 0;
            // AutoTask ID
        }
        //ATA add new phase by phase not all phases at one time 1/9/2017 [End]

        public long CreateNewPhase(long ProjectAutotaskID, QAUseCasePoints USP)
        {
            foreach (QAWBS Phase in USP.WBS)
            {
                if (Phase.AutoTaskID == 0)
                {
                    string phaseName = "Phase M" + Phase.Month;
                    Phase newPhase = new Phase();
                    newPhase.Title = phaseName;
                    newPhase.ProjectID = ProjectAutotaskID;
                    newPhase.CreateDate = DateTime.Now.Date;
                    newPhase.Description = phaseName;
                    newPhase.StartDate = DateTime.Now.Date;
                    //newPhase.DueDate = DateTime.Now.Date;
                    switch (Phase.Month)
                    {
                        case 0:
                            newPhase.DueDate = CalcEndate.CalcEnddate(DateTime.Now.Date, 1);
                            break;
                        case 1:
                            newPhase.DueDate = CalcEndate.CalcEnddate(DateTime.Now.Date, 2);
                            break;
                        case 2:
                            newPhase.DueDate = CalcEndate.CalcEnddate(DateTime.Now.Date, 3);
                            break;
                        case 3:
                            newPhase.DueDate = CalcEndate.CalcEnddate(DateTime.Now.Date, 4);
                            break;
                    }
                    ATWSResponse ResponseAuto;
                    SubAutoTask1.Entity[] entArr = new SubAutoTask1.Entity[1];
                    entArr[0] = (SubAutoTask1.Entity)newPhase;
                    ResponseAuto = clientAuto.create(at_integrations, entArr);
                    if (ResponseAuto.ReturnCode == 1)
                    {
                        Phase.AutoTaskID = ResponseAuto.EntityReturnInfoResults[0].EntityId;
                        Phase.Save();
                        Phase.Session.CommitTransaction();
                    }
                    else
                    {

                    }
                }
            }
            return 0;
            // AutoTask ID
        }
        public long UpdatePhase(long ProjectAutotaskID, QAUseCasePoints USP)
        {
            StringBuilder sb = new StringBuilder();
            sb.Append("<queryxml><entity>Phase</entity>").Append(System.Environment.NewLine);
            sb.Append("<query><field>ProjectID<expression op=\"Equals\">" + ProjectAutotaskID + "</expression></field></query>").Append(System.Environment.NewLine);
            sb.Append("</queryxml>").Append(System.Environment.NewLine);
            var Autotask_response = clientAuto.query(at_integrations, sb.ToString());
            if (Autotask_response.ReturnCode == 1)
            {
                if (Autotask_response.EntityResults.Length > 0)
                {
                    foreach (var PhaseResponse in Autotask_response.EntityResults)
                    {
                        foreach (QAWBS Phase1 in USP.WBS)
                        {
                            if (((Phase)PhaseResponse).id == Phase1.AutoTaskID)
                            {
                                //need to add property called autotask estimation in phase screen 
                                if (((Phase)PhaseResponse).EstimatedHours != null)
                                {
                                    Phase1.Autotaskestimaiton = int.Parse(Regex.Match(((Phase)PhaseResponse).EstimatedHours.ToString(), @"\d+").Value);
                                    Phase1.Save();
                                    Phase1.Session.CommitTransaction();
                                }
                               
                                foreach (QAActivity Activity in Phase1.QAActivities)
                                {
                                    if (Activity.AutoTaskID == 0)
                                    {
                                        if (Activity.Phase == true)
                                        {
                                            //string phaseName = "Phase M" + Phase.Month;
                                            Phase newPhase = new Phase();
                                            newPhase.Title = Activity.Activity;
                                            newPhase.ParentPhaseID = Phase1.AutoTaskID;
                                            newPhase.ProjectID = ProjectAutotaskID;
                                            newPhase.CreateDate = DateTime.Now.Date;
                                            newPhase.Description = Activity.Activity;
                                            newPhase.EstimatedHours = Activity.AvgEstVal;
                                            newPhase.StartDate = DateTime.Now.Date;
                                            newPhase.DueDate = DateTime.Now.Date;
                                            ATWSResponse ResponseAuto;
                                            SubAutoTask1.Entity[] entArr = new SubAutoTask1.Entity[1];
                                            entArr[0] = (SubAutoTask1.Entity)newPhase;
                                            ResponseAuto = clientAuto.create(at_integrations, entArr);
                                            if (ResponseAuto.ReturnCode == 1)
                                            {
                                                Activity.AutoTaskID = ResponseAuto.EntityReturnInfoResults[0].EntityId;
                                                Activity.Save();
                                                Activity.Session.CommitTransaction();
                                                // DevExpress.XtraEditors.XtraMessageBox.Show("'" + ResponseAuto.EntityResults.Length + "' Activities added");
                                            }
                                        }
                                        else
                                        {
                                            Aria5SystemAdmin.Module.SubAutoTask1.Task tsk = new Aria5SystemAdmin.Module.SubAutoTask1.Task();
                                            tsk.Title = Activity.Activity;
                                            if (Activity.Resource.AllocationCode != null)
                                            {
                                                tsk.AllocationCodeID = Activity.Resource.AllocationCode;
                                            }
                                            else
                                            {
                                                tsk.AllocationCodeID = 30043275;
                                            }
                                           // tsk.PhaseID = Phase1.AutoTaskID;
                                            tsk.ProjectID = ProjectAutotaskID;
                                            tsk.Status = 1;
                                            tsk.EstimatedHours = Activity.AvgEstVal;
                                            tsk.TaskType = 1;
                                            tsk.DepartmentID = 30042970;
                                            if (Activity.Resource.AutoTaskID == null)
                                            {
                                                tsk.AssignedResourceRoleID = 18337455;
                                                tsk.AssignedResourceID = 29834747;//readerAct.GetValue(2).ToString();
                                                tsk.DepartmentID = 30142058;
                                            }
                                            else
                                            {
                                                tsk.AssignedResourceID = Activity.Resource.AutoTaskID;
                                                tsk.AssignedResourceRoleID = Activity.Resource.AutoTaskRoleID;
                                                tsk.DepartmentID = Activity.Resource.AutoTaskDepartementID;
                                            }
                                            ATWSResponse ResponseAuto;
                                            SubAutoTask1.Entity[] entArr = new SubAutoTask1.Entity[1];
                                            entArr[0] = (SubAutoTask1.Entity)tsk;
                                            ResponseAuto = clientAuto.create(at_integrations, entArr);
                                            if (ResponseAuto.ReturnCode == 1)
                                            {
                                                Activity.AutoTaskID = ResponseAuto.EntityReturnInfoResults[0].EntityId;
                                                Activity.Save();
                                                Activity.Session.CommitTransaction();
                                                // DevExpress.XtraEditors.XtraMessageBox.Show("'" + ResponseAuto.EntityResults.Length + "' Activities added");
                                            }
                                        }
                                    }

                                }
                            }
                             else if (Phase1.AutoTaskID == 0 || Phase1 == null)
                            {
                                string phaseName = "Phase M" + Phase1.Month;
                                Phase newPhase = new Phase();
                                newPhase.Title = phaseName;
                                newPhase.ProjectID = ProjectAutotaskID;
                                newPhase.CreateDate = DateTime.Now.Date;
                                newPhase.Description = phaseName;
                                newPhase.StartDate = DateTime.Now.Date;
                                //newPhase.DueDate = DateTime.Now.Date;
                                switch (Phase1.Month)
                                {
                                    case 0:
                                        newPhase.DueDate = CalcEndate.CalcEnddate(DateTime.Now.Date, 1);
                                        break;
                                    case 1:
                                        newPhase.DueDate = CalcEndate.CalcEnddate(DateTime.Now.Date, 2);
                                        break;
                                    case 2:
                                        newPhase.DueDate = CalcEndate.CalcEnddate(DateTime.Now.Date, 3);
                                        break;
                                    case 3:
                                        newPhase.DueDate = CalcEndate.CalcEnddate(DateTime.Now.Date, 4);
                                        break;
                                }

                                ATWSResponse ResponseAuto;
                                SubAutoTask1.Entity[] entArr = new SubAutoTask1.Entity[1];
                                entArr[0] = (SubAutoTask1.Entity)newPhase;
                                ResponseAuto = clientAuto.create(at_integrations, entArr);
                                if (ResponseAuto.ReturnCode == 1)
                                {
                                    Phase1.AutoTaskID = ResponseAuto.EntityReturnInfoResults[0].EntityId;
                                    Phase1.Save();
                                    Phase1.Session.CommitTransaction();
                                }
                                CreateActivity(ProjectAutotaskID, USP);
                            }
                        }

                    }
                }
                else
                {
                    CreateNewPhase(ProjectAutotaskID, USP);
                    CreateActivity(ProjectAutotaskID, USP);
                }
            }
            return 0;
        }

        public void UpdatePhaseNew(long ProjectAutotaskID, IObjectSpace Objectspace)
        {
            StringBuilder sb = new StringBuilder();
            sb.Append("<queryxml><entity>Phase</entity>").Append(System.Environment.NewLine);
            sb.Append("<query><field>ProjectID<expression op=\"Equals\">" + ProjectAutotaskID + "</expression></field></query>").Append(System.Environment.NewLine);
            sb.Append("</queryxml>").Append(System.Environment.NewLine);
            var Autotask_response = clientAuto.query(at_integrations, sb.ToString());
            if (Autotask_response.ReturnCode == 1)
            {
                if (Autotask_response.EntityResults.Length > 0)
                {
                    foreach (Phase PhaseResponse in Autotask_response.EntityResults)
                    {
                        if (PhaseResponse.id != null)
                        {
                            QAWBS ExistingPhase = Objectspace.FindObject<QAWBS>(CriteriaOperator.Parse("[AutoTaskID] = '" + PhaseResponse.id.ToString() + "'"));
                            if (ExistingPhase != null)
                            {
                                if (((Phase)PhaseResponse).EstimatedHours != null)
                                {
                                    ExistingPhase.Autotaskestimaiton = int.Parse(Regex.Match(((Phase)PhaseResponse).EstimatedHours.ToString(), @"\d+").Value);
                                    ExistingPhase.Save();
                                    ExistingPhase.Session.CommitTransaction();
                                }
                            }
                        }
                    }
                }               
            }
        }
        #endregion
        #region Activity create and update
        //ATA create Activity for phase by phase not all phases 1/9/2017 [Start]

        public long CreateActivityNew(long ProjectID,QAWBS Phase)
        {
                foreach (QAActivity Activity in Phase.QAActivities)
                {
                    if (Activity.AutoTaskID == 0)
                    {

                        if (Activity.Phase == true)
                        {
                            //string phaseName = "Phase M" + Phase.Month;
                            Phase newPhase = new Phase();
                            newPhase.Title = Activity.Activity;
                            newPhase.ParentPhaseID = Phase.AutoTaskID;
                            newPhase.ProjectID = ProjectID;
                            newPhase.CreateDate = DateTime.Now.Date;
                            newPhase.Description = Activity.Activity;
                            newPhase.StartDate = DateTime.Now.Date;
                            newPhase.DueDate = DateTime.Now.Date;
                            ATWSResponse ResponseAuto;
                            SubAutoTask1.Entity[] entArr = new SubAutoTask1.Entity[1];
                            entArr[0] = (SubAutoTask1.Entity)newPhase;
                            ResponseAuto = clientAuto.create(at_integrations, entArr);
                            if (ResponseAuto.ReturnCode == 1)
                            {
                                Activity.AutoTaskID = ResponseAuto.EntityReturnInfoResults[0].EntityId;
                            }
                            else
                            {
                                throw new Exception("There are Errors while crating this Activity '" + Activity.Activity + "' as a phase at Autotask API ");

                            }
                        }
                        else
                        {
                            Aria5SystemAdmin.Module.SubAutoTask1.Task tsk = new Aria5SystemAdmin.Module.SubAutoTask1.Task();
                            tsk.Title = Activity.Activity;
                            if (Activity.Resource.AllocationCode != null)
                            {
                                tsk.AllocationCodeID = Activity.Resource.AllocationCode;
                            }
                            else
                            {
                                tsk.AllocationCodeID = 30043275;
                            }
                            tsk.PhaseID = Phase.AutoTaskID;
                            tsk.ProjectID = ProjectID;
                            tsk.Status = 1;
                            tsk.EstimatedHours = Activity.AvgEstVal;
                            tsk.TaskType = 1;
                            if (Activity.Resource.AutoTaskID == null)
                            {
                                tsk.AssignedResourceID = 29834747;
                                tsk.AssignedResourceRoleID = 18337452;
                                tsk.DepartmentID = 30042970;
                            }
                            else
                            {
                                tsk.AssignedResourceID = Activity.Resource.AutoTaskID;
                                tsk.AssignedResourceRoleID = Activity.Resource.AutoTaskRoleID;
                                tsk.DepartmentID = Activity.Resource.AutoTaskDepartementID;
                            }
                            tsk.StartDateTime = Activity.StartDate;
                            tsk.EndDateTime = Activity.EndDate;
                            ATWSResponse ResponseAuto;
                            SubAutoTask1.Entity[] entArr = new SubAutoTask1.Entity[1];
                            entArr[0] = (SubAutoTask1.Entity)tsk;
                            ResponseAuto = clientAuto.create(at_integrations, entArr);
                            if (ResponseAuto.ReturnCode == 1)
                            {
                                Activity.AutoTaskID = ResponseAuto.EntityReturnInfoResults[0].EntityId;
                                Activity.Save();
                                Activity.Session.CommitTransaction();
                            }
                            else
                            {
                                throw new Exception("There are Errors while creating this Activity '" + Activity.Activity + "' at Autotask API ");
                            }
                        }

                    }

            }
            return 0;

        }
        //ATA create Activity for phase by phase not all phases 1/9/2017 [End]
        public long CreateActivity(long ProjectID, QAUseCasePoints USP)
        {
            foreach (QAWBS Phase in USP.WBS)
            {
                foreach (QAActivity Activity in Phase.QAActivities)
                {
                    if (Activity.AutoTaskID == null || Activity.AutoTaskID == 0)
                    {

                        if (Activity.Phase == true)
                        {
                            //string phaseName = "Phase M" + Phase.Month;
                            Phase newPhase = new Phase();
                            newPhase.Title = Activity.Activity;
                            newPhase.ParentPhaseID = Phase.AutoTaskID;
                            newPhase.ProjectID = ProjectID;
                            newPhase.CreateDate = DateTime.Now.Date;
                            newPhase.Description = Activity.Activity;
                            newPhase.StartDate = DateTime.Now.Date;
                            newPhase.DueDate = DateTime.Now.Date;
                            ATWSResponse ResponseAuto;
                            SubAutoTask1.Entity[] entArr = new SubAutoTask1.Entity[1];
                            entArr[0] = (SubAutoTask1.Entity)newPhase;
                            ResponseAuto = clientAuto.create(at_integrations, entArr);
                            if (ResponseAuto.ReturnCode == 1)
                            {
                                Activity.AutoTaskID = ResponseAuto.EntityReturnInfoResults[0].EntityId;
                            }
                        }
                        else
                        {
                            Aria5SystemAdmin.Module.SubAutoTask1.Task tsk = new Aria5SystemAdmin.Module.SubAutoTask1.Task();
                            tsk.Title = Activity.Activity;
                            if (Activity.Resource.AllocationCode != null)
                            {
                                tsk.AllocationCodeID = Activity.Resource.AllocationCode;
                            }
                            else
                            {
                                tsk.AllocationCodeID = 30043275;
                            }
                            tsk.PhaseID = Phase.AutoTaskID;
                            tsk.ProjectID = ProjectID;
                            tsk.Status = 1;
                            tsk.EstimatedHours = Activity.AvgEstVal;
                            tsk.TaskType = 1;
                            if (Activity.Resource.AutoTaskID == null)
                            {
                                tsk.AssignedResourceID = 29834747;
                                tsk.AssignedResourceRoleID = 18337452;
                                tsk.DepartmentID = 30042970;
                            }
                            else
                            {
                                tsk.AssignedResourceID = Activity.Resource.AutoTaskID;
                                tsk.AssignedResourceRoleID = Activity.Resource.AutoTaskRoleID;
                                tsk.DepartmentID = Activity.Resource.AutoTaskDepartementID;
                            }
                            ATWSResponse ResponseAuto;
                            SubAutoTask1.Entity[] entArr = new SubAutoTask1.Entity[1];
                            entArr[0] = (SubAutoTask1.Entity)tsk;
                            ResponseAuto = clientAuto.create(at_integrations, entArr);
                            if (ResponseAuto.ReturnCode == 1)
                            {
                                Activity.AutoTaskID = ResponseAuto.EntityReturnInfoResults[0].EntityId;
                                Activity.Save();
                                Activity.Session.CommitTransaction();
                            }
                            else
                            {

                            }
                        }
                      
                    }

                }
            }
            return 0;

        }
        #endregion
        # region Tasks Creation and Update
        //ATA new function to create the each task separate not related to project entity loop 1/9/2017 [start]
        public long CreateTaskNew(long ProjectID, TrackingTask task)
        {
           
                if (task.AutotaskID == 0)
                {
                    Aria5SystemAdmin.Module.SubAutoTask1.Task NewTask = new Aria5SystemAdmin.Module.SubAutoTask1.Task();
                    //tsk2.Title = "[Tracking" + projectentity.TrackingNumber.ID + "+" + projectentity.Name + "]_" + TASK.Task.Name.ToString();
                    NewTask.Title = task.Tittle;
                    if (task.WBSMonth != null && task.WBSMonth.AutoTaskID == 0)
                    {
                        CreateNewPhasenew(task.TrackingEntry.ProjectTemplate.AutoTaskID, task.WBSMonth);
                    }
                    //tsk3.id = 0;
                    if (task.WBSActivity != null && task.WBSActivity.Phase == true)
                    {
                        NewTask.PhaseID = task.WBSMonth.AutoTaskID;
                    }
                    else
                    {
                        NewTask.PhaseID = task.WBSMonth.AutoTaskID;
                    }
                    NewTask.ProjectID = ProjectID.ToString();
                    // tsk.Status = TASK.Status.ToString();
                    NewTask.Status = 1;
                    NewTask.StartDateTime = task.StartDate;
                    NewTask.EndDateTime = task.EndDate;
                    NewTask.EstimatedHours = task.Duration;
                    NewTask.TaskType = 1;
                    NewTask.DepartmentID = task.Resources.AutoTaskDepartementID;
                    NewTask.AssignedResourceID = task.Resources.AutoTaskID;
                    NewTask.AssignedResourceRoleID = task.Resources.AutoTaskRoleID;
                    NewTask.AllocationCodeID = task.Resources.AllocationCode;
                    ATWSResponse ResponseAuto;
                    SubAutoTask1.Entity[] entArr = new SubAutoTask1.Entity[1];
                    entArr[0] = (SubAutoTask1.Entity)NewTask;
                    ResponseAuto = clientAuto.create(at_integrations, entArr);
                    if (ResponseAuto.ReturnCode == 1)
                    {
                        task.AutotaskID = ResponseAuto.EntityReturnInfoResults[0].EntityId;
                        task.Save();
                        task.Session.CommitTransaction();
                    }
                    else
                    {
                        if (ResponseAuto.Errors.Count() > 0)
                            throw new Exception(ResponseAuto.Errors[0].Message);
                        else
                            throw new Exception("Please contact your administrator some error happened");
                    }
                }
            return 0;
        }
        //ATA new function to create the each task separate not related to project entity loop 1/9/2017 [start]

        public long CreateTask(long ProjectID, QAProjectEntity Entity)
        {
            foreach (TrackingTask task in Entity.TrackingNumber.TrackingTasks)
            {
                if (task.AutotaskID == null || task.AutotaskID == 0)
                {
                    Aria5SystemAdmin.Module.SubAutoTask1.Task NewTask = new Aria5SystemAdmin.Module.SubAutoTask1.Task();
                    //tsk2.Title = "[Tracking" + projectentity.TrackingNumber.ID + "+" + projectentity.Name + "]_" + TASK.Task.Name.ToString();
                    NewTask.Title = task.Tittle;
                    //tsk3.id = 0;
                    NewTask.AllocationCodeID = 30043275;
                    if (task.WBSActivity != null && task.WBSActivity.Phase == true)
                    {
                        NewTask.PhaseID = task.WBSMonth.AutoTaskID;
                    }
                    else
                    {
                        NewTask.PhaseID = task.WBSMonth.AutoTaskID;
                    }
                    NewTask.ProjectID = ProjectID.ToString();
                    // tsk.Status = TASK.Status.ToString();
                    NewTask.Status = 1;
                    NewTask.StartDateTime = task.StartDate;
                    NewTask.EndDateTime = task.EndDate;
                    NewTask.EstimatedHours = task.Duration;
                    NewTask.TaskType = 1;
                    NewTask.DepartmentID = task.Resources.AutoTaskDepartementID;
                    NewTask.AssignedResourceID = task.Resources.AutoTaskID;
                    NewTask.AssignedResourceRoleID = task.Resources.AutoTaskRoleID;
                    ATWSResponse ResponseAuto;
                    SubAutoTask1.Entity[] entArr = new SubAutoTask1.Entity[1];
                    entArr[0] = (SubAutoTask1.Entity)NewTask;
                    ResponseAuto = clientAuto.create(at_integrations, entArr);
                    if (ResponseAuto.ReturnCode == 1)
                    {
                        task.AutotaskID = ResponseAuto.EntityReturnInfoResults[0].EntityId;
                        task.Save();
                        task.Session.CommitTransaction();
                    }
                    else
                    {

                    }
                }
            }
            return 0;
        }
        public double[] UpdateTask(ProjectTemplate Project,IObjectSpace Objectspace)
        {
            double[] gauges = new double[4];
            foreach (QAWBS WBS in Project.UseCasePoints.WBS)
            {
                List<Aria5SystemAdmin.Module.SubAutoTask1.Task> TracingtaskToUpdate = new List<Aria5SystemAdmin.Module.SubAutoTask1.Task>();
                double phase_actual = 0;
                double phase_estimate = 0;
                bool iscompleted = true;
                #region childphase
                StringBuilder phase = new StringBuilder();
                phase.Append("<queryxml><entity>Phase</entity>").Append(System.Environment.NewLine);
                phase.Append("<query><condition><field>ParentPhaseID<expression op=\"Equals\">" + WBS.AutoTaskID + "</expression></field></condition>").Append(System.Environment.NewLine);
                phase.Append("<condition><field>ProjectID<expression op=\"Equals\">" + Project.AutoTaskID + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                phase.Append("</queryxml>").Append(System.Environment.NewLine);
                var Autotaskchildphase = clientAuto.query(at_integrations, phase.ToString());
                if (Autotaskchildphase.ReturnCode == 1)
                {
                    if (Autotaskchildphase.EntityResults.Length > 0)
                    {
                        foreach (var ChiledPhase in Autotaskchildphase.EntityResults)
                        {
                            StringBuilder sb = new StringBuilder();
                            sb.Append("<queryxml><entity>Task</entity>").Append(System.Environment.NewLine);
                            sb.Append("<query><condition><field>PhaseID<expression op=\"Equals\">" + ((Phase)ChiledPhase).id + "</expression></field></condition>").Append(System.Environment.NewLine);
                            sb.Append("<condition><field>ProjectID<expression op=\"Equals\">" + Project.AutoTaskID + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                            sb.Append("</queryxml>").Append(System.Environment.NewLine);
                            var Autotaskchildtasks = clientAuto.query(at_integrations, sb.ToString());
                            if (Autotaskchildtasks.ReturnCode == 1)
                            {
                                if (Autotaskchildtasks.EntityResults.Length > 0)
                                {

                                    foreach (var Autotask_task in Autotaskchildtasks.EntityResults)
                                    {
                                        //List<Aria5SystemAdmin.Module.SubAutoTask1.Task> TrackingchildtaskToAdd = new List<Aria5SystemAdmin.Module.SubAutoTask1.Task>();
                                        int childtask_actuall = 0;
                                        StringBuilder stringbuilder = new StringBuilder();
                                        //ATA 19/7/2016
                                        //ATA 12/22/2016  select completion user defined field from user defined fields in the task because it's index is change with each type [Start]
                                        UserDefinedField completion = ((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).UserDefinedFields.Where(x => x.Name == "Completion %").FirstOrDefault();
                                        //ATA 12/22/2016  select completion user defined field from user defined fields in the task because it's index is change with each type [End]
                                        if (completion != null)
                                        {
                                            phase_estimate += (int.Parse(Regex.Match(completion.Value.ToString(), @"\d+").Value) * int.Parse(Regex.Match(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).EstimatedHours.ToString(), @"\d+").Value)) / 100;
                                        }
                                        //ATA 19/7/2016
                                            if (Convert.ToInt32(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).Status) != 5)
                                            {
                                                 iscompleted =false;
                                            }
                                             
                                        stringbuilder.Append("<queryxml><entity>TimeEntry</entity>").Append(System.Environment.NewLine);
                                        stringbuilder.Append("<query><condition><field>TaskID<expression op=\"Equals\">" + int.Parse(Regex.Match(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).id.ToString(), @"\d+").Value) + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                                        // stringbuilder.Append("<condition><field>ProjectID<expression op=\"Equals\">" + Project.AutoTaskID + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                                        stringbuilder.Append("</queryxml>").Append(System.Environment.NewLine);
                                        var childtask_time = clientAuto.query(at_integrations, stringbuilder.ToString());
                                        if (childtask_time.ReturnCode == 1)
                                        {
                                            if (childtask_time.EntityResults.Length > 0)
                                            {
                                                foreach (var timeentry in childtask_time.EntityResults)
                                                {
                                                    phase_actual += double.Parse(Regex.Match(((TimeEntry)timeentry).HoursWorked.ToString(), @"\d+").Value);
                                                    childtask_actuall += int.Parse(Regex.Match(((TimeEntry)timeentry).HoursWorked.ToString(), @"\d+").Value);
                                                }
                                            }
                                            else
                                            {
                                                if (int.Parse(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).Status.ToString()) == 5)
                                                {
                                                    phase_actual += 0;
                                                    //int.Parse(Regex.Match(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).EstimatedHours.ToString(), @"\d+").Value);
                                                }
                                                else
                                                {
                                                    phase_actual += 0;
                                                    //int.Parse(Regex.Match(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).EstimatedHours.ToString(), @"\d+").Value) - int.Parse(Regex.Match(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).RemainingHours.ToString(), @"\d+").Value);
                                                }
                                            }
                                          //  TrackingTask Childtask = Project.Session.FindObject<TrackingTask>(CriteriaOperator.Parse("[AutotaskID] = '" + ((Aria5SystemAdmin.Module.SubAutoTask1.Task)(Autotask_task)).id + "'"));
                                           
                                            TrackingTask Childtask = Objectspace.FindObject<TrackingTask>(CriteriaOperator.Parse("[AutotaskID] = '" + ((Aria5SystemAdmin.Module.SubAutoTask1.Task)(Autotask_task)).id + "'"));
                                            if (Childtask != null)
                                            {
                                                if (Childtask.Actuall != childtask_actuall || Childtask.Actuall == 0)
                                                {
                                                    Childtask.Actuall = childtask_actuall;
                                                    Childtask.Remaining = int.Parse(Regex.Match(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).EstimatedHours.ToString(), @"\d+").Value) - Childtask.Actuall;

                                                }
                                                if (((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).CompletedDateTime != null)
                                                {
                                                    Childtask.Completedate = DateTime.Parse(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).CompletedDateTime.ToString());
                                                }
                                                Childtask.Save();
                                                Childtask.Session.CommitTransaction();
                                            }
                                        }
                                        foreach (QAProjectEntity projectentity in Project.ProjectEntities)
                                        {
                                            if (projectentity.TrackingNumber != null && projectentity.TrackingNumber.TrackingTasks != null)
                                            {
                                                foreach (TrackingTask TASK in projectentity.TrackingNumber.TrackingTasks)
                                                {
                                                    if (TASK.WBSActivity.AutoTaskID.ToString() == ((Phase)ChiledPhase).id.ToString())//&& (TASK.AutotaskID == null || TASK.AutotaskID == 0))// && //Autotask ID not null or 0)
                                                    {
                                                        if (TASK.AutotaskID == ((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).id)
                                                        {
                                                            Aria5SystemAdmin.Module.SubAutoTask1.Task ExistTask = new Aria5SystemAdmin.Module.SubAutoTask1.Task();
                                                            ExistTask.Title = TASK.Tittle;
                                                            ExistTask.id = TASK.AutotaskID;
                                                            ExistTask.AllocationCodeID = 30043275;
                                                            ExistTask.PhaseID = ((Phase)ChiledPhase).id;
                                                            ExistTask.ProjectID = Project.AutoTaskID;
                                                            if (TASK.AutotaskID == ((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).id)
                                                            {
                                                                ExistTask.Status = ((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).Status;
                                                            }
                                                            //tsk2.Status = 1;
                                                            ExistTask.StartDateTime = TASK.StartDate;
                                                            ExistTask.EndDateTime = TASK.EndDate;
                                                            ExistTask.EstimatedHours = TASK.Duration;
                                                            ExistTask.TaskType = 1;
                                                            ExistTask.DepartmentID = TASK.Resources.AutoTaskDepartementID;
                                                            ExistTask.AssignedResourceID = TASK.Resources.AutoTaskID;
                                                            ExistTask.AssignedResourceRoleID = TASK.Resources.AutoTaskRoleID;
                                                            if (!TracingtaskToUpdate.Contains(ExistTask))
                                                            {
                                                                TracingtaskToUpdate.Add(ExistTask);
                                                            }
                                                            //}
                                                        }

                                                        if (TracingtaskToUpdate.Count > 0)
                                                        {
                                                            SubAutoTask1.Entity[] entArr1 = TracingtaskToUpdate.ToArray();
                                                            ATWSResponse ResponseAuto1 = clientAuto.update(at_integrations, entArr1);
                                                            if (ResponseAuto1.ReturnCode == 1)
                                                            {

                                                                //  DevExpress.XtraEditors.XtraMessageBox.Show("'" + ResponseAuto1.EntityResults.Length + "' Tasks Updated");
                                                            }
                                                        }
                                                    }
                                                }

                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                #endregion
                StringBuilder newsb = new StringBuilder();
                newsb.Append("<queryxml><entity>Task</entity>").Append(System.Environment.NewLine);
                newsb.Append("<query><condition><field>PhaseID<expression op=\"Equals\">" + WBS.AutoTaskID + "</expression></field></condition>").Append(System.Environment.NewLine);
                newsb.Append("<condition><field>ProjectID<expression op=\"Equals\">" + Project.AutoTaskID + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                newsb.Append("</queryxml>").Append(System.Environment.NewLine);
                var Autotask_query = clientAuto.query(at_integrations, newsb.ToString());
                if (Autotask_query.ReturnCode == 1)
                {
                    List<Aria5SystemAdmin.Module.SubAutoTask1.Task> TrackingtaskToAdd = new List<Aria5SystemAdmin.Module.SubAutoTask1.Task>();
                    if (Autotask_query.EntityResults.Length > 0)
                    {
                            foreach (var Autotask_task in Autotask_query.EntityResults)
                            {
                                var task_actuall = 0;
                                //ATA 12/22/2016  select completion user defined field from user defined fields in the task because it's index is change with each type [Start]
                                 UserDefinedField completion = ((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).UserDefinedFields.Where(x => x.Name == "Completion %").FirstOrDefault();
                                 //ATA 12/22/2016  select completion user defined field from user defined fields in the task because it's index is change with each type [End] 
                                if (completion.Value != null && ((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).EstimatedHours != null)
                                phase_estimate += (int.Parse(Regex.Match(completion.Value.ToString(), @"\d+").Value) * int.Parse(Regex.Match(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).EstimatedHours.ToString(), @"\d+").Value)) / 100;
                                
                                if (Convert.ToInt32(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).Status) != 5)
                                {
                                    iscompleted = false;
                                }
                                StringBuilder stringbuilder = new StringBuilder();
                                stringbuilder.Append("<queryxml><entity>TimeEntry</entity>").Append(System.Environment.NewLine);
                                stringbuilder.Append("<query><condition><field>TaskID<expression op=\"Equals\">" + int.Parse(Regex.Match(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).id.ToString(), @"\d+").Value) + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                                // stringbuilder.Append("<condition><field>ProjectID<expression op=\"Equals\">" + Project.AutoTaskID + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                                stringbuilder.Append("</queryxml>").Append(System.Environment.NewLine);
                                var task_time = clientAuto.query(at_integrations, stringbuilder.ToString());
                                if (task_time.ReturnCode == 1)
                                {
                                    if (task_time.EntityResults.Length > 0)
                                    {

                                        foreach (var TimeEntry in task_time.EntityResults)
                                        {
                                            phase_actual += double.Parse(Regex.Match(((TimeEntry)TimeEntry).HoursWorked.ToString(), @"\d+").Value);
                                            task_actuall += int.Parse(Regex.Match(((TimeEntry)TimeEntry).HoursWorked.ToString(), @"\d+").Value);
                                        }
                                    }
                                    //TrackingTask trackingtask = Project.Session.FindObject<TrackingTask>(CriteriaOperator.Parse("[AutotaskID] = '" + ((Aria5SystemAdmin.Module.SubAutoTask1.Task)(Autotask_task)).id + "'"));
                                    TrackingTask trackingtask = Objectspace.FindObject<TrackingTask>(CriteriaOperator.Parse("[AutotaskID] = '" + ((Aria5SystemAdmin.Module.SubAutoTask1.Task)(Autotask_task)).id + "'"));
                                    if (trackingtask != null)
                                    {
                                        if (trackingtask.Actuall != task_actuall || trackingtask.Actuall == 0)
                                        {
                                            trackingtask.Actuall = task_actuall;
                                            trackingtask.Remaining = int.Parse(Regex.Match(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).EstimatedHours.ToString(), @"\d+").Value) - trackingtask.Actuall;
                                        }
                                        if (((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).CompletedDateTime != null)
                                        {
                                            trackingtask.Completedate = DateTime.Parse(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).CompletedDateTime.ToString());
                                        }
                                        trackingtask.Save();
                                        trackingtask.Session.CommitTransaction();
                                    }
                                }
                                foreach (QAProjectEntity projectentity in Project.ProjectEntities)
                                {
                                    if (projectentity.TrackingNumber != null && projectentity.TrackingNumber.TrackingTasks != null)
                                    {
                                        foreach (TrackingTask TASK in projectentity.TrackingNumber.TrackingTasks)
                                        {
                                            if (TASK.WBSMonth.Oid.ToString() == WBS.Oid.ToString())//&& (TASK.AutotaskID == null || TASK.AutotaskID == 0))// && //Autotask ID not null or 0)
                                            {
                                                if (TASK.AutotaskID == ((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).id)
                                                {
                                                    Aria5SystemAdmin.Module.SubAutoTask1.Task ExistTask = new Aria5SystemAdmin.Module.SubAutoTask1.Task();
                                                    ExistTask.Title = TASK.Tittle;
                                                    ExistTask.id = TASK.AutotaskID;
                                                    ExistTask.AllocationCodeID = 30043275;
                                                    ExistTask.PhaseID = WBS.AutoTaskID;
                                                    ExistTask.ProjectID = Project.AutoTaskID;
                                                    if (TASK.AutotaskID == ((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).id)
                                                    {
                                                        ExistTask.Status = ((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).Status;
                                                    }
                                                    //tsk2.Status = 1;
                                                    ExistTask.StartDateTime = TASK.StartDate;
                                                    ExistTask.EndDateTime = TASK.EndDate;
                                                    ExistTask.EstimatedHours = TASK.Duration;
                                                    ExistTask.TaskType = 1;
                                                    ExistTask.DepartmentID = TASK.Resources.AutoTaskDepartementID;
                                                    ExistTask.AssignedResourceID = TASK.Resources.AutoTaskID;
                                                    ExistTask.AssignedResourceRoleID = TASK.Resources.AutoTaskRoleID;
                                                    if (!TracingtaskToUpdate.Contains(ExistTask))
                                                    {
                                                        TracingtaskToUpdate.Add(ExistTask);
                                                    }
                                                    //}
                                                }
                                                else if (TASK.AutotaskID == 0)
                                                {
                                                    // New Task Has No Autotask ID So it will be added to Autotask
                                                    Aria5SystemAdmin.Module.SubAutoTask1.Task NewTask = new Aria5SystemAdmin.Module.SubAutoTask1.Task();
                                                    NewTask.Title = TASK.Tittle;
                                                    if (TASK.Resources.AllocationCode != null)
                                                    {
                                                        NewTask.AllocationCodeID = int.Parse(TASK.Resources.AllocationCode);
                                                    }
                                                    else
                                                    {
                                                        NewTask.AllocationCodeID = 30043275;
                                                    }
                                                    if (TASK.WBSActivity != null && TASK.WBSActivity.Phase == true)
                                                    {
                                                        NewTask.PhaseID = TASK.WBSActivity.AutoTaskID;
                                                    }
                                                    else
                                                    {
                                                        NewTask.PhaseID = TASK.WBSMonth.AutoTaskID;
                                                    }
                                                    NewTask.ProjectID = Project.AutoTaskID;
                                                    NewTask.Status = 1;
                                                    NewTask.StartDateTime = TASK.StartDate;
                                                    NewTask.EndDateTime = TASK.EndDate;
                                                    NewTask.EstimatedHours = TASK.Duration;
                                                    NewTask.TaskType = 1;
                                                    NewTask.DepartmentID = TASK.Resources.AutoTaskDepartementID;
                                                    NewTask.AssignedResourceID = TASK.Resources.AutoTaskID;
                                                    NewTask.AssignedResourceRoleID = TASK.Resources.AutoTaskRoleID;
                                                    if (!TrackingtaskToAdd.Contains(NewTask))
                                                    {
                                                       // TrackingtaskToAdd.Add(NewTask);
                                                        SubAutoTask1.Entity[] entArr1 = new SubAutoTask1.Entity[1];
                                                        entArr1[0] = NewTask;
                                                        ATWSResponse ResponseAuto1 = clientAuto.create(at_integrations, entArr1);
                                                        if (ResponseAuto1.ReturnCode == 1)
                                                        {
                                                            foreach (var Respons_task in ResponseAuto1.EntityResults)
                                                            {
                                                                Aria5SystemAdmin.Module.SubAutoTask1.Task createdtask = (Aria5SystemAdmin.Module.SubAutoTask1.Task)Respons_task;
                                                                TASK.AutotaskID = createdtask.id;
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                //if (TrackingtaskToAdd.Count > 0)
                                //{

                                //    SubAutoTask1.Entity[] entArr1 = TrackingtaskToAdd.ToArray();
                                //    ATWSResponse ResponseAuto1 = clientAuto.create(at_integrations, entArr1);

                                //    if (ResponseAuto1.ReturnCode == 1)
                                //    {
                                //        //New
                                //        foreach (var Respons_task in ResponseAuto1.EntityResults)
                                //        {
                                //            Aria5SystemAdmin.Module.SubAutoTask1.Task createdtask = (Aria5SystemAdmin.Module.SubAutoTask1.Task)Respons_task;
                                //           TrackingTask Systemtask = Objectspace.FindObject<TrackingTask>(CriteriaOperator.Parse("[Tittle] = '" + createdtask.Title + "' and [WBSMonth] = '" + WBS.Oid + "' "));
                                //            Systemtask.AutotaskID = createdtask.id;
                                //            Systemtask.Save();
                                //            Systemtask.Session.CommitTransaction();
                                //            //  DevExpress.XtraEditors.XtraMessageBox.Show("'" + ResponseAuto1.EntityResults.Length + "' Tasks Updated");
                                //            UpdatePhase(Project.AutoTaskID, Project.UseCasePoints);
                                //        }
                                //        TrackingtaskToAdd.Clear();
                                //    }
                                //}
                            }
                        }
                        //create rthe tasks if the phase don't contain the any tasks 
                        else
                        {
                        iscompleted = false;
                            foreach (QAProjectEntity projectentity in Project.ProjectEntities)
                            {
                                if (projectentity.TrackingNumber != null && projectentity.TrackingNumber.TrackingTasks != null)
                                {
                                    foreach (TrackingTask TASK in projectentity.TrackingNumber.TrackingTasks)
                                    {
                                        if (TASK.WBSMonth.Oid.ToString() == WBS.Oid.ToString())//&& (TASK.AutotaskID == null || TASK.AutotaskID == 0))// && //Autotask ID not null or 0)
                                        {
                                            if (TASK.AutotaskID == 0)
                                            {
                                                // New Task Has No Autotask ID So it will be added to Autotask
                                                Aria5SystemAdmin.Module.SubAutoTask1.Task NewTask = new Aria5SystemAdmin.Module.SubAutoTask1.Task();
                                                NewTask.Title = TASK.Tittle;
                                                if (TASK.Resources.AllocationCode != null)
                                                {
                                                    NewTask.AllocationCodeID = int.Parse(TASK.Resources.AllocationCode);
                                                }
                                                else
                                                {
                                                    NewTask.AllocationCodeID = 30043275;
                                                }
                                                if (TASK.WBSActivity != null && TASK.WBSActivity.Phase == true)
                                                {
                                                    NewTask.PhaseID = TASK.WBSActivity.AutoTaskID;
                                                }
                                                else
                                                {
                                                    NewTask.PhaseID = TASK.WBSMonth.AutoTaskID;
                                                }
                                                NewTask.ProjectID = Project.AutoTaskID;
                                                NewTask.Status = 1;
                                                NewTask.StartDateTime = TASK.StartDate;
                                                NewTask.EndDateTime = TASK.EndDate;
                                                NewTask.EstimatedHours = TASK.Duration;
                                                NewTask.TaskType = 1;
                                                NewTask.DepartmentID = TASK.Resources.AutoTaskDepartementID;
                                                NewTask.AssignedResourceID = TASK.Resources.AutoTaskID;
                                                NewTask.AssignedResourceRoleID = TASK.Resources.AutoTaskRoleID;
                                                if (!TrackingtaskToAdd.Contains(NewTask))
                                                {
                                                   // TrackingtaskToAdd.Add(NewTask);
                                                    SubAutoTask1.Entity[] entArr1 = new SubAutoTask1.Entity[1];
                                                    entArr1[0] = NewTask;
                                                    ATWSResponse ResponseAuto1 = clientAuto.create(at_integrations, entArr1);
                                                    if (ResponseAuto1.ReturnCode == 1)
                                                    {
                                                        foreach (var Respons_task in ResponseAuto1.EntityResults)
                                                        {
                                                            Aria5SystemAdmin.Module.SubAutoTask1.Task createdtask = (Aria5SystemAdmin.Module.SubAutoTask1.Task)Respons_task;
                                                            TASK.AutotaskID = createdtask.id;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            if (TrackingtaskToAdd.Count > 0)
                            {

                                //SubAutoTask1.Entity[] entArr1 = TrackingtaskToAdd.ToArray();
                                //ATWSResponse ResponseAuto1 = clientAuto.create(at_integrations, entArr1);

                                //if (ResponseAuto1.ReturnCode == 1)
                                //{
                                //    //New
                                //    foreach (var Respons_task in ResponseAuto1.EntityResults)
                                //    {
                                //        Aria5SystemAdmin.Module.SubAutoTask1.Task createdtask = (Aria5SystemAdmin.Module.SubAutoTask1.Task)Respons_task;
                                //        // command.CommandText = "Update [TrackingTask] set AutoTaskID = " + ts.id.ToString() + " Where [Tittle] = '" + ts.Title + "' and [WBSMonth]='" + item.Key +"'";
                                //      //  TrackingTask Systemtask = Project.Session.FindObject<TrackingTask>(CriteriaOperator.Parse("[Tittle] = '" + createdtask.Title + "' and [WBSMonth] = '" + WBS.Oid + "' "));
                                        
                                //        TrackingTask Systemtask = Objectspace.FindObject<TrackingTask>(CriteriaOperator.Parse("[Tittle] = '" + createdtask.Title + "' and [WBSMonth] = '" + WBS.Oid + "' "));
                                //        Systemtask.AutotaskID = createdtask.id;
                                //        Systemtask.Save();
                                //        Systemtask.Session.CommitTransaction();
                                        //  DevExpress.XtraEditors.XtraMessageBox.Show("'" + ResponseAuto1.EntityResults.Length + "' Tasks Updated");
                                        UpdatePhase(Project.AutoTaskID, Project.UseCasePoints);
                                  //  }
                               // }
                            }
                        }

                    }
                else
                {
                    throw new Exception("there is an error occured during updating");
                }
                if (TracingtaskToUpdate.Count > 0)
                {
                    SubAutoTask1.Entity[] entArr1 = TracingtaskToUpdate.ToArray();
                    ATWSResponse ResponseAuto1 = clientAuto.update(at_integrations, entArr1);
                    if (ResponseAuto1.ReturnCode == 1)
                    {

                      //  DevExpress.XtraEditors.XtraMessageBox.Show("'" + ResponseAuto1.EntityResults.Length + "' Tasks Updated");
                    }
                }
                
                gauges[WBS.Month] = phase_estimate;
                WBS.Autotaskactual = phase_actual;
                WBS.Iscompleted = iscompleted;
               // WBS.Autotaskestimaiton = phase_estimate;
                WBS.Save();
                WBS.Session.CommitTransaction();
                //  updateprojectphases(Project, WBS, phase_actual);

                #region commented
                //// ProjectTemplate modifiedproject = Objectspace.FindObject<ProjectTemplate>(CriteriaOperator.Parse("[Oid] = '" + Project.Oid + "'"));
                // switch ("phase M" + WBS.Month)
                // {
                //     case "phase M0":
                //         phasem0 = (((double)phase_actual / (double)WBS.Autotaskestimaiton)) * 100;
                //         //modifiedproject.PhaseM0 = (((double)phase_actual / (double)WBS.Autotaskestimaiton)) * 100;
                //         //modifiedproject.Save();
                //         // Objectspace.CommitChanges();
                //         //  modifiedproject.Session.CommitTransaction();
                //         break;
                //     case "phase M1":
                //         phasem1 = (((double)phase_actual / (double)WBS.Autotaskestimaiton)) * 100;
                //         //modifiedproject.PhaseM1 = (((double)phase_actual / (double)WBS.Autotaskestimaiton)) * 100;
                //         //modifiedproject.Save();
                //         //Objectspace.CommitChanges();

                //         //  modifiedproject.Session.CommitTransaction();
                //         break;
                //     case "phase M2":
                //         phasem2 = (((double)phase_actual / (double)WBS.Autotaskestimaiton)) * 100;
                //         //modifiedproject.PhaseM2 = (((double)phase_actual / (double)WBS.Autotaskestimaiton)) * 100;
                //         //modifiedproject.Save();
                //         //Objectspace.CommitChanges();

                //         // modifiedproject.Session.CommitTransaction();
                //         break;
                //     case "phase M3":
                //         phasem3 = (((double)phase_actual / (double)WBS.Autotaskestimaiton)) * 100;
                //         //modifiedproject.PhaseM3 = (((double)phase_actual / (double)WBS.Autotaskestimaiton)) * 100;
                //         //modifiedproject.Save();
                //         //Objectspace.CommitChanges();

                //         //modifiedproject.Session.CommitTransaction();
                //         break;
                // } 
                #endregion

            }
            return gauges;
        }

        //ATA New function to update tasks without from auto task data not update from the system admin side
        public double[] UpdateTaskNew(ProjectTemplate Project, IObjectSpace Objectspace)
        {
            double[] gauges = new double[4];
            foreach (QAWBS WBS in Project.UseCasePoints.WBS)
            {
                ICollection<QAActivity> phaseactivity = WBS.QAActivities;
                ICollection<TrackingTask> PhaseTasks = Objectspace.GetObjects<TrackingTask>(CriteriaOperator.Parse("[WBSMonth] = '" + WBS.Oid + "'"));
                 double phase_actual = 0;
                double phase_estimate = 0;
                int Phase_autotaskRequiredHours = 0;
                bool iscompleted = true;
                #region childphase
                StringBuilder phase = new StringBuilder();
                phase.Append("<queryxml><entity>Phase</entity>").Append(System.Environment.NewLine);
                phase.Append("<query><condition><field>ParentPhaseID<expression op=\"Equals\">" + WBS.AutoTaskID + "</expression></field></condition>").Append(System.Environment.NewLine);
                phase.Append("<condition><field>ProjectID<expression op=\"Equals\">" + Project.AutoTaskID + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                phase.Append("</queryxml>").Append(System.Environment.NewLine);
                var Autotaskchildphase = clientAuto.query(at_integrations, phase.ToString());
                if (Autotaskchildphase.ReturnCode == 1)
                {
                    if (Autotaskchildphase.EntityResults.Length > 0)
                    {
                        foreach (var ChiledPhase in Autotaskchildphase.EntityResults)
                        {
                            StringBuilder sb = new StringBuilder();
                            sb.Append("<queryxml><entity>Task</entity>").Append(System.Environment.NewLine);
                            sb.Append("<query><condition><field>PhaseID<expression op=\"Equals\">" + ((Phase)ChiledPhase).id + "</expression></field></condition>").Append(System.Environment.NewLine);
                            sb.Append("<condition><field>ProjectID<expression op=\"Equals\">" + Project.AutoTaskID + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                            sb.Append("</queryxml>").Append(System.Environment.NewLine);
                            var Autotaskchildtasks = clientAuto.query(at_integrations, sb.ToString());
                            if (Autotaskchildtasks.ReturnCode == 1)
                            {
                                if (Autotaskchildtasks.EntityResults.Length > 0)
                                {

                                    foreach (Aria5SystemAdmin.Module.SubAutoTask1.Task Autotask_task in Autotaskchildtasks.EntityResults)
                                    {
                                        //List<Aria5SystemAdmin.Module.SubAutoTask1.Task> TrackingchildtaskToAdd = new List<Aria5SystemAdmin.Module.SubAutoTask1.Task>();
                                        int childtask_actuall = 0;
                                        StringBuilder stringbuilder = new StringBuilder();
                                        //ATA 19/7/2016
                                        //ATA 12/22/2016  select completion user defined field from user defined fields in the task because it's index is change with each type [Start]
                                        UserDefinedField completion = ((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).UserDefinedFields.Where(x => x.Name == "Completion %").FirstOrDefault();
                                        //ATA 12/22/2016  select completion user defined field from user defined fields in the task because it's index is change with each type [End]
                                        if (completion != null)
                                        {
                                            phase_estimate += (int.Parse(Regex.Match(completion.Value.ToString(), @"\d+").Value) * int.Parse(Regex.Match(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).EstimatedHours.ToString(), @"\d+").Value)) / 100;
                                        }
                                        //ATA 19/7/2016
                                            if (Convert.ToInt32(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).Status) != 5)
                                            {
                                                 iscompleted =false;
                                            }
                                            Phase_autotaskRequiredHours = int.Parse(Regex.Match(Autotask_task.EstimatedHours.ToString(), @"\d+").Value);
                                        stringbuilder.Append("<queryxml><entity>TimeEntry</entity>").Append(System.Environment.NewLine);
                                        stringbuilder.Append("<query><condition><field>TaskID<expression op=\"Equals\">" + int.Parse(Regex.Match(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).id.ToString(), @"\d+").Value) + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                                        // stringbuilder.Append("<condition><field>ProjectID<expression op=\"Equals\">" + Project.AutoTaskID + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                                        stringbuilder.Append("</queryxml>").Append(System.Environment.NewLine);
                                        var childtask_time = clientAuto.query(at_integrations, stringbuilder.ToString());
                                        if (childtask_time.ReturnCode == 1)
                                        {
                                            if (childtask_time.EntityResults.Length > 0)
                                            {
                                                foreach (var timeentry in childtask_time.EntityResults)
                                                {
                                                    phase_actual += double.Parse(Regex.Match(((TimeEntry)timeentry).HoursWorked.ToString(), @"\d+").Value);
                                                    childtask_actuall += int.Parse(Regex.Match(((TimeEntry)timeentry).HoursWorked.ToString(), @"\d+").Value);
                                                }
                                            }
                                        }
                                          //  TrackingTask Childtask = Project.Session.FindObject<TrackingTask>(CriteriaOperator.Parse("[AutotaskID] = '" + ((Aria5SystemAdmin.Module.SubAutoTask1.Task)(Autotask_task)).id + "'"));
                                        QAActivity existactivity = phaseactivity.Where(x => x.AutoTaskID == long.Parse(Autotask_task.id.ToString())).FirstOrDefault();
                                        if (existactivity == null)
                                        {
                                            TrackingTask Existtask = PhaseTasks.Where(x => x.AutotaskID == long.Parse(Autotask_task.id.ToString())).FirstOrDefault();
                                            if (Existtask == null)
                                            {
                                                QAActivity newactivity = Objectspace.CreateObject<QAActivity>();
                                                newactivity.Activity = Autotask_task.Title.ToString();
                                                newactivity.AvgEstVal = int.Parse(Regex.Match(Autotask_task.EstimatedHours.ToString(), @"\d+").Value);
                                                newactivity.AutoTaskID = long.Parse(Autotask_task.id.ToString());
                                                newactivity.QAWBS = WBS;
                                                newactivity.Save();
                                                Objectspace.CommitChanges();
                                            }
                                            else
                                            {

                                                if (Existtask.Actuall != childtask_actuall || Existtask.Actuall == 0 || Existtask.Duration !=int.Parse(Regex.Match(Autotask_task.EstimatedHours.ToString(), @"\d+").Value))
                                                    {
                                                        Existtask.Actuall = childtask_actuall;
                                                        Existtask.Remaining = int.Parse(Regex.Match(Autotask_task.EstimatedHours.ToString(), @"\d+").Value) - Existtask.Actuall;
                                                        Existtask.Duration = int.Parse(Regex.Match(Autotask_task.EstimatedHours.ToString(), @"\d+").Value);

                                                    }
                                                    if (((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).CompletedDateTime != null)
                                                    {
                                                        Existtask.Completedate = DateTime.Parse(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).CompletedDateTime.ToString());
                                                    }
                                                    Existtask.Save();
                                                    Existtask.Session.CommitTransaction();
                                                
                                            }
                                        }
                                        else
                                        {
                                            if (existactivity.AvgEstVal != int.Parse(Regex.Match(Autotask_task.EstimatedHours.ToString(), @"\d+").Value))
                                            {
                                                existactivity.AvgEstVal = int.Parse(Regex.Match(Autotask_task.EstimatedHours.ToString(), @"\d+").Value);
                                                //ATA add completed date to check if the activity completed or not 1/31/2017
                                                if (((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).CompletedDateTime != null)
                                                {
                                                    existactivity.CompletedDate = DateTime.Parse(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Autotask_task).CompletedDateTime.ToString());
                                                }
                                                existactivity.Save();
                                                existactivity.Session.CommitTransaction();
                                            }
                                        }
                                            //TrackingTask Childtask = Objectspace.FindObject<TrackingTask>(CriteriaOperator.Parse("[AutotaskID] = '" + ((Aria5SystemAdmin.Module.SubAutoTask1.Task)(Autotask_task)).id + "'"));
                                    }
                                }
                                else
                                {
                                    iscompleted = false;
                                }
                            }
                           
                        }
                    }
                }
                #endregion
                StringBuilder newsb = new StringBuilder();
                newsb.Append("<queryxml><entity>Task</entity>").Append(System.Environment.NewLine);
                newsb.Append("<query><condition><field>PhaseID<expression op=\"Equals\">" + WBS.AutoTaskID + "</expression></field></condition>").Append(System.Environment.NewLine);
                newsb.Append("<condition><field>ProjectID<expression op=\"Equals\">" + Project.AutoTaskID + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                newsb.Append("</queryxml>").Append(System.Environment.NewLine);
                var Autotask_query = clientAuto.query(at_integrations, newsb.ToString());
                if (Autotask_query.ReturnCode == 1)
                {
                    if (Autotask_query.EntityResults.Length > 0)
                    {
                        foreach (Aria5SystemAdmin.Module.SubAutoTask1.Task Task in Autotask_query.EntityResults)
                        {
                            
                            var task_actuall = 0;
                            //ATA 12/22/2016  select completion user defined field from user defined fields in the task because it's index is change with each type [Start]
                            UserDefinedField completion = ((Aria5SystemAdmin.Module.SubAutoTask1.Task)Task).UserDefinedFields.Where(x => x.Name == "Completion %").FirstOrDefault();
                            //ATA 12/22/2016  select completion user defined field from user defined fields in the task because it's index is change with each type [End] 
                            if (completion.Value != null && ((Aria5SystemAdmin.Module.SubAutoTask1.Task)Task).EstimatedHours != null)
                                phase_estimate += (int.Parse(Regex.Match(completion.Value.ToString(), @"\d+").Value) * int.Parse(Regex.Match(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Task).EstimatedHours.ToString(), @"\d+").Value)) / 100;

                            if (Convert.ToInt32(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Task).Status) != 5)
                            {
                                iscompleted = false;
                            }
                            Phase_autotaskRequiredHours += int.Parse(Regex.Match(Task.EstimatedHours.ToString(), @"\d+").Value);
                            StringBuilder stringbuilder = new StringBuilder();
                            stringbuilder.Append("<queryxml><entity>TimeEntry</entity>").Append(System.Environment.NewLine);
                            stringbuilder.Append("<query><condition><field>TaskID<expression op=\"Equals\">" + int.Parse(Regex.Match(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Task).id.ToString(), @"\d+").Value) + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                            // stringbuilder.Append("<condition><field>ProjectID<expression op=\"Equals\">" + Project.AutoTaskID + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                            stringbuilder.Append("</queryxml>").Append(System.Environment.NewLine);
                            var task_time = clientAuto.query(at_integrations, stringbuilder.ToString());
                            if (task_time.ReturnCode == 1)
                            {
                                if (task_time.EntityResults.Length > 0)
                                {

                                    foreach (var TimeEntry in task_time.EntityResults)
                                    {
                                        phase_actual += double.Parse(Regex.Match(((TimeEntry)TimeEntry).HoursWorked.ToString(), @"\d+").Value);
                                        task_actuall += int.Parse(Regex.Match(((TimeEntry)TimeEntry).HoursWorked.ToString(), @"\d+").Value);
                                    }
                                }
                            }
                                //TrackingTask trackingtask = Project.Session.FindObject<TrackingTask>(CriteriaOperator.Parse("[AutotaskID] = '" + ((Aria5SystemAdmin.Module.SubAutoTask1.Task)(Autotask_task)).id + "'"));
                                //ATA search in the list that retrived instead of search in all data base 
                                // TrackingTask trackingtask = Objectspace.FindObject<TrackingTask>(CriteriaOperator.Parse("[AutotaskID] = '" + ((Aria5SystemAdmin.Module.SubAutoTask1.Task)(Task)).id + "'"));
                               // TrackingTask trackingtask = PhaseTasks.Where(x => x.AutotaskID == Task.id).First();
                                //ATA search in the list that retrived instead of search in all data base 
                            QAActivity existactivity = phaseactivity.Where(x => x.AutoTaskID == long.Parse(Task.id.ToString())).FirstOrDefault();
                            if (existactivity == null)
                            {
                                TrackingTask trackingtask = PhaseTasks.Where(x => x.AutotaskID == long.Parse(Task.id.ToString())).FirstOrDefault();
                                if (trackingtask == null)
                                {
                                    QAActivity newactivity = Objectspace.CreateObject<QAActivity>();
                                    newactivity.Activity = Task.Title.ToString();
                                    newactivity.AvgEstVal = int.Parse(Regex.Match(Task.EstimatedHours.ToString(), @"\d+").Value);
                                    newactivity.AutoTaskID = long.Parse(Task.id.ToString());
                                    newactivity.QAWBS = WBS;
                                    newactivity.Save();
                                    Objectspace.CommitChanges();

                                }
                                else
                                {
                                    if (trackingtask != null)
                                    {
                                        if (trackingtask.Actuall != task_actuall || trackingtask.Actuall == 0 || trackingtask.Duration != int.Parse(Regex.Match(Task.EstimatedHours.ToString(), @"\d+").Value))
                                        {
                                            trackingtask.Actuall = task_actuall;
                                            trackingtask.Remaining = int.Parse(Regex.Match(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Task).EstimatedHours.ToString(), @"\d+").Value) - trackingtask.Actuall;
                                            trackingtask.Duration = int.Parse(Regex.Match(Task.EstimatedHours.ToString(), @"\d+").Value);
                                        }
                                        if (((Aria5SystemAdmin.Module.SubAutoTask1.Task)Task).CompletedDateTime != null)
                                        {
                                            trackingtask.Completedate = DateTime.Parse(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Task).CompletedDateTime.ToString());
                                        }
                                        trackingtask.Save();
                                        trackingtask.Session.CommitTransaction();
                                    }
                                }
                            }
                            else
                            {
                                if (existactivity.AvgEstVal != int.Parse(Regex.Match(Task.EstimatedHours.ToString(), @"\d+").Value))
                                {
                                    existactivity.AvgEstVal = int.Parse(Regex.Match(Task.EstimatedHours.ToString(), @"\d+").Value);
                                   //ATA add completed date to check if the activity completed or not 1/31/2017
                                    if (((Aria5SystemAdmin.Module.SubAutoTask1.Task)Task).CompletedDateTime != null)
                                    {
                                        existactivity.CompletedDate = DateTime.Parse(((Aria5SystemAdmin.Module.SubAutoTask1.Task)Task).CompletedDateTime.ToString());
                                    }
                                    existactivity.Save();
                                    existactivity.Session.CommitTransaction();
                                }
                            }
                               
                            
                        }
                    }
                    else
                    {
                        iscompleted = false;
                    }
                }
                if (WBS.Month < 4)
                {
                    gauges[WBS.Month] = phase_estimate;
                }
                WBS.Autotaskestimaiton = Phase_autotaskRequiredHours;
                WBS.Autotaskactual = phase_actual;
                WBS.Iscompleted = iscompleted;
                // WBS.Autotaskestimaiton = phase_estimate;
                WBS.Save();
                WBS.Session.CommitTransaction();
            }
            return gauges;
        }
        //ATA test code refien 
        //public long updatetask(ProjectTemplate Project, QAWBS WBS)
        //{

        //        StringBuilder newsb = new StringBuilder();
        //        newsb.Append("<queryxml><entity>Task</entity>").Append(System.Environment.NewLine);
        //        newsb.Append("<query><condition><field>PhaseID<expression op=\"Equals\">" + WBS.AutoTaskID + "</expression></field></condition>").Append(System.Environment.NewLine);
        //        newsb.Append("<condition><field>ProjectID<expression op=\"Equals\">" + Project.AutoTaskID + "</expression></field></condition></query>").Append(System.Environment.NewLine);
        //        newsb.Append("</queryxml>").Append(System.Environment.NewLine);
        //        var Autotask_query = clientAuto.query(at_integrations, newsb.ToString());
        //        if (Autotask_query.ReturnCode == 1)
        //        {

        //        }
            
             
        //}
        //public void taskstoadd(Aria5SystemAdmin.Module.SubAutoTask1.Task[] task, IObjectSpace Objectspace, QAWBS WBS)
        //{
        //    ATWSResponse ResponseAuto1 = clientAuto.create(at_integrations, task);

        //    if (ResponseAuto1.ReturnCode == 1)
        //    {
        //        //New
        //        foreach (var Respons_task in ResponseAuto1.EntityResults)
        //        {
        //            Aria5SystemAdmin.Module.SubAutoTask1.Task createdtask = (Aria5SystemAdmin.Module.SubAutoTask1.Task)Respons_task;
        //            TrackingTask Systemtask = Objectspace.FindObject<TrackingTask>(CriteriaOperator.Parse("[Tittle] = '" + createdtask.Title + "' and [WBSMonth] = '" + WBS.Oid + "' "));
        //            Systemtask.AutotaskID = createdtask.id;
        //            Systemtask.Save();
        //            Systemtask.Session.CommitTransaction();
                    
        //        }
        //    }
        //}
        //ATA test code refien 
        public long updateprojectentity(ProjectTemplate project, TrackingEntry projectentity)
        {

            double totaldesignactual = 0;
            double totaldesignestimate = 0;
            double totalprogrammingestimate = 0;
            double totalprogramming = 0;
            if (projectentity.TrackingTasks != null)
            {
                foreach (TrackingTask TASK1 in projectentity.TrackingTasks)
                {
                    if (TASK1.Task.Name == "Design")
                    {
                        totaldesignactual += TASK1.Actuall;
                        totaldesignestimate += (double)TASK1.Duration;
                    }
                    else if (TASK1.Task.Name == "Programming")
                    {
                        totalprogramming += TASK1.Actuall;
                        totalprogrammingestimate += (double)TASK1.Duration;
                    }
                }
                //projectentity.TotalActualDetaildesign = totaldesignactual;
                //projectentity.DetailDesignTime = totaldesignestimate;
                //projectentity.TotalActualProgramming = totalprogramming;
                //projectentity.ProgrammingTime = totalprogrammingestimate;
                projectentity.Save();
                projectentity.Session.CommitTransaction();
               // DevExpress.XtraEditors.XtraMessageBox.Show(" Project Entity '" + projectentity.Name + "' Updated ");
            }
            return 0;
        }
        public long Phaseshours(ProjectTemplate project, QAUseCasePoints usp , double [] values)
        {
           
            double phasem0 = 0;
            double phasem1 = 0;
            double phasem2 = 0;
            double phasem3 = 0;
            double completepercentage = 0;
            double estimate = 0;
            foreach (QAWBS phase in usp.WBS)
            {
               
                switch (phase.Month)
                {
                    case 0:
                        //ATA autotask estimation that ll required hours in this phase 
                        //ATA Values[0] is the current required hours in this phase based on each task completed percentage (task estimation * completed percentage)
                        if ((double)phase.Autotaskestimaiton > 0  && values[0] >= 0)
                        {
                            //ATA that what the gauges display the already completed hours based on completed percentage vS all the estimat hours 
                            phasem0 = (values[0] / (double)phase.Autotaskestimaiton) * 100;
                            completepercentage += values[0];
                            estimate += (double)phase.Autotaskestimaiton;
                            //ATA check if the acual time equal the required time based on comlted percenatge taht user enter or not 
                            if (phase.Autotaskactual - values[0] < 0)
                            {
                                project.PhaseM0_SPI = string.Format("Ahead Of Schedule by ({0})", -1*(phase.Autotaskactual - values[0]));
                            }
                            else if (phase.Autotaskactual - values[0] > 0)
                            {
                                project.PhaseM0_SPI = string.Format("Behind Schedule by ({0})", phase.Autotaskactual - values[0]);

                            }
                            else
                            {
                                project.PhaseM0_SPI = string.Format("On Schedule ({0})", phase.Autotaskactual - values[0]);

                            }
                        }
                        if (phase.Iscompleted)
                        {
                            project.PhaseM0_statues = ProjectTemplate.status.Complete;
                        }
                        else if (!phase.Iscompleted)
                        {
                            project.PhaseM0_statues = ProjectTemplate.status.InWork;
                        }
                        break;
                    case 1:
                        if ((double)phase.Autotaskestimaiton > 0 && values[1] >= 0)
                        {
                            phasem1 = (values[1] / (double)phase.Autotaskestimaiton) * 100;
                            completepercentage += values[1];
                            estimate += (double)phase.Autotaskestimaiton;
                            if (phase.Autotaskactual - values[1] < 0)
                            {
                                project.PhaseM1_SPI = string.Format("Ahead Of Schedule by ({0})", -1 * (phase.Autotaskactual - values[1]));
                            }
                            else if (phase.Autotaskactual - values[1] > 0)
                            {
                                project.PhaseM1_SPI = string.Format("Behind Schedule by ({0})", phase.Autotaskactual - values[1]);

                            }
                            else
                            {
                                project.PhaseM1_SPI = string.Format("On Schedule ({0})", phase.Autotaskactual - values[1]);

                            }

                        }
                        if (phase.Iscompleted)
                        {
                            project.PhaseM1_statues = ProjectTemplate.status.Complete;
                        }
                        else if (!phase.Iscompleted)
                        {
                            project.PhaseM1_statues = ProjectTemplate.status.InWork;
                        }
                        
                        break;
                    case 2:
                        if ((double)phase.Autotaskestimaiton > 0 && values[2] >= 0)
                        {
                             phasem2 = (values[2] / (double)phase.Autotaskestimaiton) * 100;
                             completepercentage += values[2];
                             estimate += (double)phase.Autotaskestimaiton;
                             if (phase.Autotaskactual - values[2] < 0)
                             {
                                 project.PhaseM2_SPI = string.Format("Ahead Of Schedule by ({0})", -1 * (phase.Autotaskactual - values[2]));
                             }
                             else if (phase.Autotaskactual - values[2] > 0)
                             {
                                 project.PhaseM2_SPI = string.Format("Behind Schedule by ({0})", phase.Autotaskactual - values[2]);

                             }
                             else
                             {
                                 project.PhaseM2_SPI = string.Format("On Schedule ({0})", phase.Autotaskactual - values[2]);

                             }
                        }
                        if (phase.Iscompleted)
                        {
                            project.PhaseM2_statues = ProjectTemplate.status.Complete;
                        }
                        else if (!phase.Iscompleted)
                        {
                            project.PhaseM2_statues = ProjectTemplate.status.InWork;
                        }
                       
                        break;
                    case 3:
                        if ((double)phase.Autotaskestimaiton > 0 && values[3] >= 0)
                        {
                                                 
                        phasem3 = (values[3] / (double)phase.Autotaskestimaiton) * 100;
                        completepercentage += values[3];
                        estimate += (double)phase.Autotaskestimaiton;
                        if (phase.Autotaskactual - values[3] < 0)
                        {
                            project.PhaseM3_SPI = string.Format("Ahead Of Schedule by ({0})", -1 * (phase.Autotaskactual - values[3]));
                        }
                        else if (phase.Autotaskactual - values[3] > 0)
                        {
                            project.PhaseM3_SPI = string.Format("Behind Schedule by ({0})", phase.Autotaskactual - values[3]);

                        }
                        else
                        {
                            project.PhaseM3_SPI = string.Format("On Schedule ({0})", phase.Autotaskactual - values[3]);

                        }
                        }
                        if (phase.Iscompleted)
                        {
                            project.PhaseM3_statues = ProjectTemplate.status.Complete;
                        }
                        else if (!phase.Iscompleted)
                        {
                            project.PhaseM3_statues = ProjectTemplate.status.InWork;
                        }
                      
                        break;
                }
            }
            if (estimate > 0)
            {
                project.Completness = (completepercentage / estimate) * 100; 
            }
            project.PhaseM0 = phasem0;
            project.PhaseM1 = phasem1;
            project.PhaseM2 = phasem2;
            project.PhaseM3 = phasem3;
            project.Save();
            project.Session.CommitTransaction();
            return 0;
        }
        #endregion
        # region Collect Resources
        public void GetResourceInfo(Resources oneresource)
        {
            if (oneresource.CurrentEmailAddress != null)
            {
                string DepartementID = "";
                string DepartementName = "";
                Dictionary<string, string> Resour = new Dictionary<string, string>();
                StringBuilder Dep = new StringBuilder();
                Dep.Append("<queryxml><entity>Department</entity>").Append(System.Environment.NewLine);
                //ATA add oneresource instead of this
                Dep.Append("<query><condition><field>Name<expression op=\"Equals\">" + oneresource.Department.Description.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                Dep.Append("</queryxml>").Append(System.Environment.NewLine);

                var r12 = clientAuto.query(at_integrations, Dep.ToString());
                if (r12.ReturnCode == 1)
                {
                    if (r12.EntityResults.Length > 0)
                    {
                        for (int cc = 0; cc < r12.EntityResults.Length; cc++)
                        {
                            SubAutoTask1.Department rs = new SubAutoTask1.Department();
                            rs = (SubAutoTask1.Department)r12.EntityResults[cc];
                            DepartementID = rs.id.ToString();
                            DepartementName = rs.Name.ToString();
                            // this.AutoTaskDepartementID = rs.Id.ToString();
                            //  this.Department.Name = rs.Name.ToString();
                        }

                    }
                }
                if (DepartementName == "IT")
                {
                    StringBuilder Allocationcodequery = new StringBuilder();
                    Allocationcodequery.Append("<queryxml><entity>AllocationCode</entity>").Append(System.Environment.NewLine);
                    Allocationcodequery.Append("<query><condition><field>Department<expression op=\"Equals\">" + DepartementID + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                    Allocationcodequery.Append("</queryxml>").Append(System.Environment.NewLine);

                    var AllocationCoderesponse = clientAuto.query(at_integrations, Allocationcodequery.ToString());
                    if (AllocationCoderesponse.ReturnCode == 1)
                    {
                        if (AllocationCoderesponse.EntityResults.Length > 0)
                        {
                            foreach (AllocationCode allocationcode in AllocationCoderesponse.EntityResults)
                            {
                                if (allocationcode.Name.ToString().Contains("Que"))
                                {
                                    oneresource.AllocationCode = allocationcode.id.ToString();
                                }
                            }
                        }
                    }
                }
                else
                {
                    StringBuilder Allocationcodequery = new StringBuilder();
                    Allocationcodequery.Append("<queryxml><entity>AllocationCode</entity>").Append(System.Environment.NewLine);
                    Allocationcodequery.Append("<query><condition><field>Department<expression op=\"Equals\">" + DepartementID + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                    Allocationcodequery.Append("</queryxml>").Append(System.Environment.NewLine);

                    var AllocationCoderesponse = clientAuto.query(at_integrations, Allocationcodequery.ToString());
                    if (AllocationCoderesponse.ReturnCode == 1)
                    {
                        if (AllocationCoderesponse.EntityResults.Length > 0)
                        {
                            foreach (AllocationCode allocationcode in AllocationCoderesponse.EntityResults)
                            {
                                if (allocationcode.Name.ToString().Contains("Queue"))
                                {
                                    oneresource.AllocationCode = allocationcode.id.ToString();
                                }
                            }
                        }
                    }
                }   
             
                // Developmenmt Dep ID  30042970
                // Programming Departemnet ID  29723153
                //1-Get Resource Roles List 
                // Get Resource Roles for Development Departement 
                StringBuilder sb = new StringBuilder();
                sb.Append("<queryxml><entity>ResourceRole</entity>").Append(System.Environment.NewLine);
                sb.Append("<query><field>DepartmentID<expression op=\"Equals\">" + DepartementID + "</expression></field></query>").Append(System.Environment.NewLine);
                sb.Append("</queryxml>").Append(System.Environment.NewLine);
                var r2p = clientAuto.query(at_integrations, sb.ToString());
                if (r2p.ReturnCode == 1)
                {
                    if (r2p.EntityResults.Length > 0)
                    {
                        foreach (var x in r2p.EntityResults)
                        {
                            bool flag = false;
                            var ResID = ((ResourceRole)x).ResourceID;

                            StringBuilder sb3 = new StringBuilder();
                            sb3.Append("<queryxml><entity>Resource</entity>").Append(System.Environment.NewLine);
                            sb3.Append("<query><field>id<expression op=\"Equals\">" + ResID + "</expression></field></query>").Append(System.Environment.NewLine);
                            sb3.Append("</queryxml>").Append(System.Environment.NewLine);

                            var r1 = clientAuto.query(at_integrations, sb3.ToString());
                            if (r1.ReturnCode == 1)
                            {
                                if (r1.EntityResults.Length > 0)
                                {
                                    for (int cc = 0; cc < r1.EntityResults.Length; cc++)
                                    {
                                        if (!Resour.ContainsKey(ResID.ToString()) && (((SubAutoTask1.Resource)r1.EntityResults[cc]).Active.Equals(true)))
                                        {
                                            // Resour.Add(ResID.ToString(), (((SubAutoTask1.Resource)r1.EntityResults[cc]).LastName.ToString() + "," + ((SubAutoTask1.Resource)r1.EntityResults[cc]).FirstName.ToString()));
                                            SubAutoTask1.Resource rs = ((SubAutoTask1.Resource)r1.EntityResults[cc]);
                                            if (oneresource.CurrentEmailAddress.Trim() == rs.Email.ToString().Trim())
                                            {
                                                oneresource.AutoTaskName = rs.FirstName.ToString()+" "+rs.LastName.ToString();
                                                oneresource.AutoTaskID = rs.id.ToString();
                                                oneresource.CurrentEmailAddress = rs.Email.ToString();
                                                oneresource.AutoTaskRoleID = (((ResourceRole)x).RoleID).ToString();// Resource Role ID
                                                oneresource.AutoTaskDepartementID = DepartementID;
                                                oneresource.AutoTaskDepartementName = DepartementName;
                                                oneresource.Save();
                                                oneresource.Session.CommitTransaction();
                                                flag = true;
                                                break;

                                            }
                                            //resourcesList[cc, 0] = rs.LastName.ToString() + "," + rs.FirstName.ToString();
                                            //resourcesList[cc, 1] = rs.id.ToString();
                                            //resourcesList[cc, 2] = rs.Email.ToString();
                                            //resourcesList[cc, 3] = ResID.ToString();// Resource Role ID
                                            //resourcesList[cc, 4] = DepartementID;
                                            //resourcesList[cc, 5] = DepartementName;
                                        }
                                    }
                                    if (flag)
                                    {
                                        break;
                                    }
                                }
                            }

                            //var dd = GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Resources>("dd");
                            // dd.Load();

                            // foreach (Resources item in dd)
                            // {

                            // }
                        }
                        //for (int i = 0; i < 150 ; i++)
                        //{
                        //    //Resources ccd = dd.Where(r => r.CurrentEmailAddress == resourcesList[i, 2]).FirstOrDefault();
                        //    //ccd.AutoTaskName = resourcesList[i, 0];
                        //    //ccd.AutoTaskID = resourcesList[i, 1];
                        //    //ccd.CurrentEmailAddress = resourcesList[i, 2];
                        //    //ccd.AutoTaskRoleID = resourcesList[i, 3];
                        //    //ccd.AutoTaskDepartementID = resourcesList[i, 4];
                        //    //ccd.AutoTaskDepartementName = resourcesList[i, 5];

                        //    //ccd.Save();
                        //    if (oneresource.CurrentEmailAddress == resourcesList[i, 2])
                        //    {
                        //        oneresource.AutoTaskName = resourcesList[i, 0];
                        //        oneresource.AutoTaskID = resourcesList[i, 1];
                        //        oneresource.CurrentEmailAddress = resourcesList[i, 2];
                        //        oneresource.AutoTaskRoleID = resourcesList[i, 3];
                        //        oneresource.AutoTaskDepartementID = resourcesList[i, 4];
                        //        oneresource.AutoTaskDepartementName = resourcesList[i, 5];
                        //        oneresource.Save();
                        //        View.Refresh();
                        //        break;
                        //    }

                        //}
                    }
                }
            }
            //return resourcesList;
        }
        #endregion
        #region AHT and DOT 
        //ATA 17/5/2016 calculate AHT for eachticket task  [start ]

        #region commented function 
        //ATA comment this function cause it is not be used and use instead of it function from AHTandOTDCalculation 
        //public void get_tickets_Aht_by_date_range(DateTime from, DateTime to, IObjectSpace newobject, AverageHandleTime AHT, Session currentsession)
        //{

        //    StringBuilder Selecttiecket = new StringBuilder();
        //    Selecttiecket.Append("<queryxml><entity>Ticket</entity>").Append(System.Environment.NewLine);
        //    Selecttiecket.Append("<query><condition><field>CompletedDate<expression op=\"greaterthan\">" + from + "</expression></field></condition>").Append(System.Environment.NewLine);
        //    Selecttiecket.Append("<condition><field>CompletedDate<expression op=\"LessThan\">" + to + "</expression></field></condition></query>").Append(System.Environment.NewLine);
        //    Selecttiecket.Append("</queryxml>").Append(System.Environment.NewLine);
        //    var tieckets = clientAuto.query(at_integrations, Selecttiecket.ToString());
        //    if (tieckets.ReturnCode == 1)
        //    {
        //        string[] array = new string[5];
        //        if (AHT.ERP == true)
        //        {
        //            array[0] = "ERP";
        //        }
        //        if (AHT.CRM == true)
        //        {
        //            array[1] = "CWA";
        //        }
        //        if (AHT.EDI == true)
        //        {
        //            array[2] = "EDI";
        //        }
        //        if (AHT.SD == true)
        //        {
        //            array[3] = "SD";
        //        }
        //        if (AHT.IT == true)
        //        {
        //            array[4] = "IT";
        //        }
        //        IList<TicketAHT> allcalculatedtickets = new List<TicketAHT>();
        //       // IList<TicketAHT> allexiststtickets = newobject.GetObjects<TicketAHT>(CriteriaOperator.Parse("[Completedate] >= '"+from+"' and [Completedate] <= '"+to+"'"));
        //        AverageHandleTime Averagehandlingtimeconfig = currentsession.FindObject<AverageHandleTime>(CriteriaOperator.Parse("[oid] = '" + AHT.Oid + "'"));
        //        //Dictionary<string, object[]> Queues_without_pmo_dic = new Dictionary<string, object[]>();
        //        //Dictionary<string, object[]> Queues_with_pmo_dic = new Dictionary<string, object[]>();
        //        Dictionary<string, object[]> Allqueuesdic = new Dictionary<string, object[]>();
        //        System.Collections.ICollection queuesName = currentsession.GetObjects(currentsession.Dictionary.GetClassInfo(typeof(QueueName)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
        //        IList<QueueName> allqueues = queuesName.Cast<QueueName>().ToList();
        //        foreach (var item in allqueues)
        //        {
        //            Allqueuesdic.Add(item.Name, new object[6]);
        //        }

        //        //ATA Add resource AHT for the aht calc 12/6/2016
        //        Dictionary<string, object[]> Allresoudic = new Dictionary<string, object[]>();
        //        System.Collections.ICollection Resources = currentsession.GetObjects(currentsession.Dictionary.GetClassInfo(typeof(Resources)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
        //        IList<Resources> allresources = Resources.Cast<Resources>().ToList();
        //        foreach (var Resourcename in allresources)
        //        {
        //            Allresoudic.Add(((Resources)Resourcename).AutoTaskName, new object[6]);
        //        }
        //        //ATA Add resource AHT for the aht calc 12/6/2016
        //        #region dectionary filled commented 
        //        //#region dectionary Queues_without_pmo_dic
        //        /////<summary>
        //        ///// this array of objects  element of index [0] is the datetime that ticket forwared to the queue element of index [1]
        //        ///// is the datetiem that ticket forwared from the queue and the element of index [2] is a temproray datetime to save the date 
        //        ///// if ticket moved from the queue to the same queue element of index [3] is the total of days that ticket spend in the queue 
        //        /////</summary>

        //        ////fill the dectionary 
        //        //Queues_without_pmo_dic.Add("00 Sales", new object[4]);
        //        //Queues_without_pmo_dic.Add("10 Customer Care", new object[4]);
        //        //Queues_without_pmo_dic.Add("20 Help desk", new object[4]);
        //        //Queues_without_pmo_dic.Add("21 Web Based Support", new object[4]);
        //        //Queues_without_pmo_dic.Add("23 Consulting to Dispatch", new object[4]);
        //        //Queues_without_pmo_dic.Add("24 Pc waiting for sign of", new object[4]);
        //        //Queues_without_pmo_dic.Add("50 Customer Acceptance", new object[4]);
        //        //Queues_without_pmo_dic.Add("69 Validation", new object[4]);
        //        //Queues_without_pmo_dic.Add("98 Pending Enhancements", new object[4]);
        //        //Queues_without_pmo_dic.Add("99 Hold Entries", new object[4]);
        //        //Queues_without_pmo_dic.Add("Product management ", new object[4]);
        //        //Queues_without_pmo_dic.Add("Billing Disputes.", new object[4]);
        //        //Queues_without_pmo_dic.Add("Postal Sale", new object[4]);
        //        //Queues_without_pmo_dic.Add("80 Review", new object[4]);
        //        //Queues_without_pmo_dic.Add("90 Defects Waiting Sch.", new object[4]);
        //        //Queues_without_pmo_dic.Add("91 Defects (In Dev.)", new object[4]);
        //        //Queues_without_pmo_dic.Add("95 Distribution", new object[4]);
        //        //Queues_without_pmo_dic.Add("96 IT", new object[4]);
        //        //#endregion
        //        //#region dictionary Queues_with_pmo_dic
        //        /////<summary>
        //        ///// this array of objects  element of index [0] is the datetime that ticket forwared to the queue element of index [1]
        //        ///// is the datetiem that ticket forwared from the queue and the element of index [2] is a temproray datetime to save the date 
        //        ///// if ticket moved from the queue to the same queue element of index [3] is the date time that the ticket is forwared to the queue 
        //        ///// but the primary resource is project manager 
        //        ///// the element of index [4] is the total of days that ticket spend in the queue 
        //        ///// the element of index [5] is the total of days ticket spend in the queue with priject manager resource
        //        ///// </summary>

        //        //// fill the dictionary
        //        //Queues_with_pmo_dic.Add("70 ERP", new object[6]);
        //        //Queues_with_pmo_dic.Add("71 EDI", new object[6]);
        //        //Queues_with_pmo_dic.Add("72 CWA", new object[6]);
        //        //Queues_with_pmo_dic.Add("73 HTML5", new object[6]);
        //        //// end of fill the dictionary 
        //        //#endregion
        //        #endregion 
        //        if (tieckets.EntityResults.Count() > 0)
        //        {
        //            foreach (Ticket ticket in tieckets.EntityResults)
        //            {
        //                //check if this ticket have user defined field or not if it have we can calculate it's AHT if not we needn't to calculate it 
        //                if (ticket.UserDefinedFields.Count() > 0 && ticket.UserDefinedFields[8].Value != null)
        //                {
        //                    if (((Ticket)ticket).TicketNumber != null)
        //                    {
        //                        TicketAHT existticket = currentsession.FindObject<TicketAHT>(CriteriaOperator.Parse("[TiecketNum] = '" + ((Ticket)ticket).TicketNumber + "'"));
        //                       // TicketAHT existticket = allexiststtickets.FirstOrDefault(ticketone => ticketone.TiecketNum == ticket.TicketNumber.ToString());
        //                        if (existticket != null && existticket.Ticketage == Math.Round(DateTime.Parse(ticket.CompletedDate.ToString()).Date.Subtract(DateTime.Parse(ticket.CreateDate.ToString()).Date).TotalDays,0))
        //                        {
        //                            //CreateAHTlist(array,AHT, existticket, newobject);
        //                            allcalculatedtickets.Add(existticket);
        //                        }
        //                        else
        //                        {
        //                            AHTandDOTCalculation AHTandOTD = new AHTandDOTCalculation();
        //                            TicketAHT AHT_for_ticket = AHTandOTD.CalculateAHT(currentsession, ticket, Allqueuesdic, allqueues,Allresoudic);//Queues_without_pmo_dic, Queues_with_pmo_dic);
        //                            if (AHT_for_ticket != null)
        //                            {
        //                                //TicketAHT ticket_to_create2 = newobject.FindObject<TicketAHT>(CriteriaOperator.Parse("[Oid] = '" + AHT_for_ticket.Oid + "'"));
        //                               // CreateAHTlist(array, AHT, AHT_for_ticket, newobject);
        //                                allcalculatedtickets.Add(AHT_for_ticket);
        //                            }
        //                        }
        //                    }
                           
        //                }
        //            }
        //            Averagehandlingtimeconfig.Tickets.AddRange(allcalculatedtickets);
        //        }
        //    }
        //}
        #endregion 
        public TicketAHT get_ticket_Aht_by_number(IObjectSpace newobject, string ticketnumber, Session currentsession)
        {
            TicketAHT createdticket = null;

            StringBuilder Selecttiecketpernumber = new StringBuilder();
            Selecttiecketpernumber.Append("<queryxml><entity>Ticket</entity>").Append(System.Environment.NewLine);
            Selecttiecketpernumber.Append("<query><condition><field>TicketNumber<expression op=\"equals\">" + ticketnumber + "</expression></field></condition></query>").Append(System.Environment.NewLine);
            //Selecttiecketpernumber.Append("<condition><field>CompletedDate<expression op=\"LessThan\">" + to + "</expression></field></condition></query>").Append(System.Environment.NewLine);
            Selecttiecketpernumber.Append("</queryxml>").Append(System.Environment.NewLine);
            var tiecket = clientAuto.query(at_integrations, Selecttiecketpernumber.ToString());
            if (tiecket.ReturnCode == 1)
            {
                //Dictionary<string, object[]> one = new Dictionary<string, object[]>();
                //Dictionary<string, object[]> two = new Dictionary<string, object[]>();
                Dictionary<string, object[]> Allqueuesdic = new Dictionary<string, object[]>();
                System.Collections.ICollection queuesName = currentsession.GetObjects(currentsession.Dictionary.GetClassInfo(typeof(QueueName)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
                IList<QueueName> allqueues = queuesName.Cast<QueueName>().ToList();
                foreach (var item in allqueues)
                {
                    Allqueuesdic.Add(item.Name, new object[6]);
                }

                //ATA Add resource AHT for the aht calc 12/6/2016
                Dictionary<string, object[]> Allresoudic = new Dictionary<string, object[]>();
                System.Collections.ICollection Resources = currentsession.GetObjects(currentsession.Dictionary.GetClassInfo(typeof(Resources)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
                IList<Resources> allresources = Resources.Cast<Resources>().ToList();
                foreach (var Resourcename in allresources)
                {
                    if (((Resources)Resourcename).AutoTaskName != null)
                    Allresoudic.Add(((Resources)Resourcename).AutoTaskName, new object[6]);
                }
                //ATA Add resource AHT for the aht calc 12/6/2016
                #region commented
                //#region dectionary one
                /////<summary>
                ///// this array of objects  element of index [0] is the datetime that ticket forwared to the queue element of index [1]
                ///// is the datetiem that ticket forwared from the queue and the element of index [2] is a temproray datetime to save the date 
                ///// if ticket moved from the queue to the same queue element of index [3] is the total of days that ticket spend in the queue 
                /////</summary>
            
                ////fill the dectionary 
                //one.Add("00 Sales", new object[4]);
                //one.Add("10 Customer Care", new object[4]);
                //one.Add("20 Help desk", new object[4]);
                //one.Add("21 Web Based Support", new object[4]);
                //one.Add("23 Consulting to Dispatch", new object[4]);
                //one.Add("24 Pc waiting for sign of", new object[4]);
                //one.Add("50 Customer Acceptance", new object[4]);
                //one.Add("69 Validation", new object[4]);
                //one.Add("98 Pending Enhancement", new object[4]);
                //one.Add("99 Hold Entries", new object[4]);
                //one.Add("Product management ", new object[4]);
                //one.Add("Billing Disputes", new object[4]);
                //one.Add("Postal Sale", new object[4]);
                //one.Add("80 Review", new object[4]);
                //one.Add("90 Defects Waiting Sch.", new object[4]);
                //one.Add("91 Defects (In Dev.)", new object[4]);
                //one.Add("95 Distribution", new object[4]);
                //one.Add("96 IT", new object[4]);
                //#endregion
                //#region dictionary two
                /////<summary>
                ///// this array of objects  element of index [0] is the datetime that ticket forwared to the queue element of index [1]
                ///// is the datetiem that ticket forwared from the queue and the element of index [2] is a temproray datetime to save the date 
                ///// if ticket moved from the queue to the same queue element of index [3] is the date time that the ticket is forwared to the queue 
                ///// but the primary resource is project manager 
                ///// the element of index [4] is the total of days that ticket spend in the queue 
                ///// the element of index [5] is the total of days ticket spend in the queue with priject manager resource
                ///// </summary>
           
                //// fill the dictionary
                //two.Add("70 ERP", new object[6]);
                //two.Add("71 EDI", new object[6]);
                //two.Add("72 CWA", new object[6]);
                //two.Add("73 HTML5", new object[6]);
                //// end of fill the dictionary 
                //#endregion
                #endregion 
                foreach (var ticket in tiecket.EntityResults)
                {
                    //check if this ticket have user defined field or not if it have we can calculate it's AHT if not we needn't to calculate it 
                    TicketAHT existingticket = newobject.FindObject<TicketAHT>(CriteriaOperator.Parse("[TiecketNum] = '" + ((Ticket)ticket).TicketNumber.ToString() + "'"));
                    if (existingticket != null)
                    {
                        if (existingticket.Ticketage != Math.Round((DateTime.Parse(((Ticket)ticket).CompletedDate.ToString()).Date).Subtract(DateTime.Parse(((Ticket)ticket).CreateDate.ToString()).Date).TotalDays, 2))
                        {
                            if (((Ticket)ticket).UserDefinedFields.Count() > 0 && ((Ticket)ticket).UserDefinedFields[8].Value != null)
                            {
                                AHTandDOTCalculation calculationobject = new AHTandDOTCalculation();
                                createdticket = calculationobject.CalculateAHT(currentsession, ((Ticket)ticket), Allqueuesdic, allqueues, Allresoudic);//one, two);
                            }
                            else
                            {
                                throw new Exception("this ticket " + ((Ticket)ticket) .TicketNumber.ToString()+ " don't have need scheduled field so we can't calculate AHT");
                            }
                        }
                    }
                    else
                    {
                        if (((Ticket)ticket).UserDefinedFields.Count() > 0 && ((Ticket)ticket).UserDefinedFields[8].Value != null)
                        {
                            AHTandDOTCalculation calculationobject = new AHTandDOTCalculation();
                            createdticket = calculationobject.CalculateAHT(currentsession, ((Ticket)ticket), Allqueuesdic, allqueues,Allresoudic);//, one, two);
                        }
                        else
                        {
                            throw new Exception("this ticket don't have user defined field so we can't calculate AHT");
                        }
                    }
                    
                   

                }
                return createdticket;
            }
            else
            {
                return createdticket;
            }

        }
        //public TicketAHT CalculateAHT(IObjectSpace newobject, Ticket ti, Dictionary<string, object[]> Queues_without_pmo_dic, Dictionary<string, object[]> Queues_with_pmo_dic)
        //{
        //    // make sure that all dictionary values empty 
        //    foreach (var one_record in Queues_without_pmo_dic)
        //    {
        //        Array.Clear(one_record.Value,0,one_record.Value.Length);
        //    }
        //    foreach (var two_record in Queues_with_pmo_dic)
        //    {
        //        Array.Clear(two_record.Value, 0, two_record.Value.Length);
        //    }
        //    TicketAHT newtiecket = newobject.CreateObject<TicketAHT>();
        //    newtiecket.Session.LockingOption = LockingOption.None;
        //    TicketDetail new_ticket_detail = newobject.CreateObject<TicketDetail>();
        //    StringBuilder selectnote = new StringBuilder();
        //    //select from auto task ticket not only which it's discription contain the word forward from to make ensure that all ticket retrived to will be used in calculation  
        //    selectnote.Append("<queryxml><entity>TicketNote</entity>").Append(System.Environment.NewLine);
        //    selectnote.Append("<query><condition><field>TicketID<expression op=\"Equals\">" + ((Ticket)ti).id.ToString() + "</expression></field>").Append(System.Environment.NewLine);
        //    selectnote.Append("<field>Description<expression op=\"Contains\">" + "Forwarded From:" + "</expression></field></condition></query>").Append(System.Environment.NewLine);
        //    selectnote.Append("</queryxml>").Append(System.Environment.NewLine);
        //    var notes = clientAuto.query(at_integrations, selectnote.ToString());
        //    if (notes.ReturnCode == 1)
        //    {
        //        if (notes.EntityResults.Count() > 0)
        //        {
        //            // reorder all ticket notes ascending according to ticket note date 
        //            TicketNote[] array123 = Array.ConvertAll(notes.EntityResults, item => (TicketNote)item);
        //            List<TicketNote> autotasknotes = array123.OrderBy(si => si.LastActivityDate).ToList();
        //            for (int i = 0; i < autotasknotes.Count; i++)//TicketNote n in autotasknotes)
        //            { 
        //                #region newcodecommented
        //                //if (flag && flag1 && flag2 && flag3)
        //                //{
        //                //    to = DateTime.MinValue;
        //                //    totemp = DateTime.MinValue; ;
        //                //    topmo = DateTime.MinValue;
        //                //    from = DateTime.MinValue;
        //                //}

        //                //TicketNote note2 = (TicketNote)n;
        //                //int index1 = note2.Description.ToString().IndexOf("Forwarded From:");
        //                //int index = note2.Description.ToString().IndexOf("Forwarded To:");
        //                //if (index >= 0)
        //                //{
        //                //    string str = note2.Description.ToString().Substring(index);
        //                //    if (str.IndexOf("00") > 0)
        //                //    {
        //                //        string newsub = str.Substring(str.IndexOf("00"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            //erpto = pmoto;
        //                //            topmo = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //            to = DateTime.MinValue;
        //                //        }
        //                //        else
        //                //        {
        //                //            if (to != DateTime.MinValue)
        //                //                totemp = to;
        //                //            to = DateTime.Parse(note2.LastActivityDate.ToString());

        //                //        }
        //                //    }
        //                //    if (str.IndexOf("10") > 0)
        //                //    {
        //                //        string newsub = str.Substring(str.IndexOf("10"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            //erpto = pmoto;
        //                //            topmo = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //            to = DateTime.MinValue;
        //                //        }
        //                //        else
        //                //        {
        //                //            if (to != DateTime.MinValue)
        //                //                totemp = to;
        //                //            to = DateTime.Parse(note2.LastActivityDate.ToString());

        //                //        }
        //                //    }
        //                //    if (str.IndexOf("20") > 0)
        //                //    {
        //                //        string newsub = str.Substring(str.IndexOf("20"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            //erpto = pmoto;
        //                //            topmo = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //            to = DateTime.MinValue;
        //                //        }
        //                //        else
        //                //        {
        //                //            if (to != DateTime.MinValue)
        //                //                totemp = to;
        //                //            to = DateTime.Parse(note2.LastActivityDate.ToString());

        //                //        }
        //                //    }
        //                //    if (str.IndexOf("21") > 0)
        //                //    {
        //                //        string newsub = str.Substring(str.IndexOf("21"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            //erpto = pmoto;
        //                //            topmo = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //            to = DateTime.MinValue;
        //                //        }
        //                //        else
        //                //        {
        //                //            if (to != DateTime.MinValue)
        //                //                totemp = to;
        //                //            to = DateTime.Parse(note2.LastActivityDate.ToString());

        //                //        }
        //                //    }
        //                //    if (str.IndexOf("23") > 0)
        //                //    {
        //                //        string newsub = str.Substring(str.IndexOf("23"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            //erpto = pmoto;
        //                //            topmo = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //            to = DateTime.MinValue;
        //                //        }
        //                //        else
        //                //        {
        //                //            if (to != DateTime.MinValue)
        //                //                totemp = to;
        //                //            to = DateTime.Parse(note2.LastActivityDate.ToString());

        //                //        }
        //                //    }
        //                //    if (str.IndexOf("24") > 0)
        //                //    {
        //                //        string newsub = str.Substring(str.IndexOf("24"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            //erpto = pmoto;
        //                //            topmo = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //            to = DateTime.MinValue;
        //                //        }
        //                //        else
        //                //        {
        //                //            if (to != DateTime.MinValue)
        //                //                totemp = to;
        //                //            to = DateTime.Parse(note2.LastActivityDate.ToString());

        //                //        }
        //                //    }
        //                //    if (str.IndexOf("50") > 0)
        //                //    {
        //                //        string newsub = str.Substring(str.IndexOf("50"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            //erpto = pmoto;
        //                //            topmo = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //            to = DateTime.MinValue;
        //                //        }
        //                //        else
        //                //        {
        //                //            if (to != DateTime.MinValue)
        //                //                totemp = to;
        //                //            to = DateTime.Parse(note2.LastActivityDate.ToString());

        //                //        }
        //                //    }
        //                //    if (str.IndexOf("69") > 0)
        //                //    {
        //                //        string newsub = str.Substring(str.IndexOf("69"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            //erpto = pmoto;
        //                //            topmo = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //            to = DateTime.MinValue;
        //                //        }
        //                //        else
        //                //        {
        //                //            if (to != DateTime.MinValue)
        //                //                totemp = to;
        //                //            to = DateTime.Parse(note2.LastActivityDate.ToString());

        //                //        }
        //                //    }
        //                //    if (str.IndexOf("70") > 0)
        //                //    {
        //                //        string newsub = str.Substring(str.IndexOf("70"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            //erpto = pmoto;
        //                //            topmo = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //            to = DateTime.MinValue;
        //                //        }
        //                //        else
        //                //        {
        //                //            if (to != DateTime.MinValue)
        //                //                totemp = to;
        //                //            to = DateTime.Parse(note2.LastActivityDate.ToString());

        //                //        }
        //                //    }
        //                //    if (str.IndexOf("71") > 0)
        //                //    {
        //                //        string newsub = str.Substring(str.IndexOf("71"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            //erpto = pmoto;
        //                //            topmo = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //            to = DateTime.MinValue;
        //                //        }
        //                //        else
        //                //        {
        //                //            if (to != DateTime.MinValue)
        //                //                totemp = to;
        //                //            to = DateTime.Parse(note2.LastActivityDate.ToString());

        //                //        }
        //                //    }
        //                //    if (str.IndexOf("72") > 0)
        //                //    {
        //                //        string newsub = str.Substring(str.IndexOf("72"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            //erpto = pmoto;
        //                //            topmo = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //            to = DateTime.MinValue;
        //                //        }
        //                //        else
        //                //        {
        //                //            if (to != DateTime.MinValue)
        //                //                totemp = to;
        //                //            to = DateTime.Parse(note2.LastActivityDate.ToString());

        //                //        }
        //                //    }
        //                //    if (str.IndexOf("73") > 0)
        //                //    {
        //                //        string newsub = str.Substring(str.IndexOf("73"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            //erpto = pmoto;
        //                //            topmo = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //            to = DateTime.MinValue;
        //                //        }
        //                //        else
        //                //        {
        //                //            if (to != DateTime.MinValue)
        //                //                totemp = to;
        //                //            to = DateTime.Parse(note2.LastActivityDate.ToString());

        //                //        }
        //                //    }
        //                //    if (str.IndexOf("80") > 0)
        //                //    {
        //                //        string newsub = str.Substring(str.IndexOf("80"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            //erpto = pmoto;
        //                //            topmo = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //            to = DateTime.MinValue;
        //                //        }
        //                //        else
        //                //        {
        //                //            if (to != DateTime.MinValue)
        //                //                totemp = to;
        //                //            to = DateTime.Parse(note2.LastActivityDate.ToString());

        //                //        }
        //                //    }
        //                //    if (str.IndexOf("90") > 0)
        //                //    {
        //                //        string newsub = str.Substring(str.IndexOf("90"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            //erpto = pmoto;
        //                //            topmo = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //            to = DateTime.MinValue;
        //                //        }
        //                //        else
        //                //        {
        //                //            if (to != DateTime.MinValue)
        //                //                totemp = to;
        //                //            to = DateTime.Parse(note2.LastActivityDate.ToString());

        //                //        }
        //                //    }
        //                //    if (str.IndexOf("91") > 0)
        //                //    {
        //                //        string newsub = str.Substring(str.IndexOf("91"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            //erpto = pmoto;
        //                //            topmo = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //            to = DateTime.MinValue;
        //                //        }
        //                //        else
        //                //        {
        //                //            if (to != DateTime.MinValue)
        //                //                totemp = to;
        //                //            to = DateTime.Parse(note2.LastActivityDate.ToString());

        //                //        }
        //                //    }
        //                //    if (str.IndexOf("95") > 0)
        //                //    {
        //                //        string newsub = str.Substring(str.IndexOf("95"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            //erpto = pmoto;
        //                //            topmo = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //            to = DateTime.MinValue;
        //                //        }
        //                //        else
        //                //        {
        //                //            if (to != DateTime.MinValue)
        //                //                totemp = to;
        //                //            to = DateTime.Parse(note2.LastActivityDate.ToString());

        //                //        }
        //                //    }
        //                //    if (str.IndexOf("96") > 0)
        //                //    {
        //                //        string newsub = str.Substring(str.IndexOf("96"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            //erpto = pmoto;
        //                //            topmo = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //            to = DateTime.MinValue;
        //                //        }
        //                //        else
        //                //        {
        //                //            if (to != DateTime.MinValue)
        //                //                totemp = to;
        //                //            to = DateTime.Parse(note2.LastActivityDate.ToString());

        //                //        }
        //                //    }
        //                //    if (str.IndexOf("Product management") > 0)
        //                //    {
        //                //        string newsub = str.Substring(str.IndexOf("Product management"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            //erpto = pmoto;
        //                //            topmo = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //            to = DateTime.MinValue;
        //                //        }
        //                //        else
        //                //        {
        //                //            if (to != DateTime.MinValue)
        //                //                totemp = to;
        //                //            to = DateTime.Parse(note2.LastActivityDate.ToString());

        //                //        }
        //                //    }
        //                //    // if (((((str.IndexOf("70") > 0) || (str.IndexOf("71") > 0)) || ((str.IndexOf("72") > 0) || (str.IndexOf("80") > 0))) || (((str.IndexOf("90") > 0) || (str.IndexOf("91") > 0)) || (str.IndexOf("95") > 0))) || (str.IndexOf("96") > 0))
        //                //    //{
        //                //    //if (!flag3)
        //                //    //{
        //                //    // num7 = num2;
        //                //    //num7 = int.Parse(pair.Value.Date.Subtract(time2.Date).TotalDays.ToString());
        //                //    //if ((note2.CreatorResourceID != null) && ((note2.CreatorResourceID.ToString().CompareTo("30321561") == 0) || (note2.CreatorResourceID.ToString().CompareTo("31378124") == 0)))
        //                //    //{
        //                //    //    num7++;
        //                //    //}
        //                //    //  flag3 = true;
        //                //    //}
        //                //    //flag2 = true;
        //                //    //var days = note2.LastActivityDate;
        //                //    // }
        //                //    //else
        //                //    //{
        //                //    //fl//ag2 = false;
        //                //    //}
        //                //}
        //                //if (index1 >= 0)
        //                //{
        //                //    string str1 = note2.Description.ToString().Substring(index1, 100);
        //                //    if (str1.IndexOf("00") > 0)
        //                //    {
        //                //        from = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //        if (from != DateTime.MinValue && to != DateTime.MinValue)
        //                //        {
        //                //            if (from == to && totemp != DateTime.MinValue)
        //                //            {
        //                //                newtiecket.Sales += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //                totemp = DateTime.MinValue;
        //                //                flag = true;
        //                //            }
        //                //            else if (from != to && to == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.Sales += Math.Round(from.Subtract(DateTime.Parse(((Ticket)ti).CreateDate.ToString())).TotalDays, 2);
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //            else if (from != to && to != DateTime.MinValue && totemp == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.Sales += Math.Round(from.Subtract(to).TotalDays, 2);
        //                //                to = DateTime.MinValue;
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //        }

        //                //    }
        //                //    if (str1.IndexOf("10") > 0)
        //                //    {

        //                //        from = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //        if (from != DateTime.MinValue && to != DateTime.MinValue)
        //                //        {
        //                //            if (from == to && totemp != DateTime.MinValue)
        //                //            {
        //                //                newtiecket.CustomerCare += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //                totemp = DateTime.MinValue;
        //                //                flag = true;
        //                //            }
        //                //            else if (from != to && to == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.CustomerCare += Math.Round(from.Subtract(DateTime.Parse(((Ticket)ti).CreateDate.ToString())).TotalDays, 2);
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //            else if (from != to && to != DateTime.MinValue && totemp == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.CustomerCare += Math.Round(from.Subtract(to).TotalDays, 2);
        //                //                to = DateTime.MinValue;
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //        }
        //                //    }
        //                //    if (str1.IndexOf("20") > 0)
        //                //    {

        //                //        from = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //        if (from != DateTime.MinValue && to != DateTime.MinValue)
        //                //        {
        //                //            if (from == to && totemp != DateTime.MinValue)
        //                //            {
        //                //                newtiecket.Helpdesk += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //                totemp = DateTime.MinValue;
        //                //                flag = true;
        //                //            }
        //                //            else if (from != to && to == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.Helpdesk += Math.Round(from.Subtract(DateTime.Parse(((Ticket)ti).CreateDate.ToString())).TotalDays, 2);
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //            else if (from != to && to != DateTime.MinValue && totemp == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.Helpdesk += Math.Round(from.Subtract(to).TotalDays, 2);
        //                //                to = DateTime.MinValue;
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //        }
        //                //    }
        //                //    if (str1.IndexOf("21") > 0)
        //                //    {

        //                //        from = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //        if (from != DateTime.MinValue && to != DateTime.MinValue)
        //                //        {
        //                //            if (from == to && totemp != DateTime.MinValue)
        //                //            {
        //                //                newtiecket.Webbased += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //                totemp = DateTime.MinValue;
        //                //                flag = true;
        //                //            }
        //                //            else if (from != to && to == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.Webbased += Math.Round(from.Subtract(DateTime.Parse(((Ticket)ti).CreateDate.ToString())).TotalDays, 2);
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //            else if (from != to && to != DateTime.MinValue && totemp == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.Webbased += Math.Round(from.Subtract(to).TotalDays, 2);
        //                //                to = DateTime.MinValue;
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //        }
        //                //    }
        //                //    if (str1.IndexOf("23") > 0)
        //                //    {

        //                //        from = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //        if (from != DateTime.MinValue && to != DateTime.MinValue)
        //                //        {
        //                //            if (from == to && totemp != DateTime.MinValue)
        //                //            {
        //                //                newtiecket.ConsultDispatch += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //                totemp = DateTime.MinValue;
        //                //                flag = true;
        //                //            }
        //                //            else if (from != to && to == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.ConsultDispatch += Math.Round(from.Subtract(DateTime.Parse(((Ticket)ti).CreateDate.ToString())).TotalDays, 2);
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //            else if (from != to && to != DateTime.MinValue && totemp == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.ConsultDispatch += Math.Round(from.Subtract(to).TotalDays, 2);
        //                //                to = DateTime.MinValue;
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //        }
        //                //    }
        //                //    if (str1.IndexOf("24") > 0)
        //                //    {
        //                //        from = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //        if (from != DateTime.MinValue && to != DateTime.MinValue)
        //                //        {
        //                //            if (from == to && totemp != DateTime.MinValue)
        //                //            {
        //                //                newtiecket.P_C_Waiting += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //                totemp = DateTime.MinValue;
        //                //                flag = true;
        //                //            }
        //                //            else if (from != to && to == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.P_C_Waiting += Math.Round(from.Subtract(DateTime.Parse(((Ticket)ti).CreateDate.ToString())).TotalDays, 2);
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //            else if (from != to && to != DateTime.MinValue && totemp == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.P_C_Waiting += Math.Round(from.Subtract(to).TotalDays, 2);
        //                //                to = DateTime.MinValue;
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //        }
        //                //    }
        //                //    if (str1.IndexOf("50") > 0)
        //                //    {
        //                //        from = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //        if (from != DateTime.MinValue && to != DateTime.MinValue)
        //                //        {
        //                //            if (from == to && totemp != DateTime.MinValue)
        //                //            {
        //                //                newtiecket.CustomerAcceptance += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //                totemp = DateTime.MinValue;
        //                //                flag = true;
        //                //            }
        //                //            else if (from != to && to == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.CustomerAcceptance += Math.Round(from.Subtract(DateTime.Parse(((Ticket)ti).CreateDate.ToString())).TotalDays, 2);
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //            else if (from != to && to != DateTime.MinValue && totemp == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.CustomerAcceptance += Math.Round(from.Subtract(to).TotalDays, 2);
        //                //                to = DateTime.MinValue;
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //        }
        //                //    }
        //                //    if (str1.IndexOf("69") > 0)
        //                //    {

        //                //        from = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //        if (from != DateTime.MinValue && to != DateTime.MinValue)
        //                //        {
        //                //            if (from == to && totemp != DateTime.MinValue)
        //                //            {
        //                //                newtiecket.Validation += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //                totemp = DateTime.MinValue;
        //                //                flag = true;
        //                //            }
        //                //            else if (from != to && to == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.Validation += Math.Round(from.Subtract(DateTime.Parse(((Ticket)ti).CreateDate.ToString())).TotalDays, 2);
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //            else if (from != to && to != DateTime.MinValue && totemp == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.Validation += Math.Round(from.Subtract(to).TotalDays, 2);
        //                //                to = DateTime.MinValue;
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //        }
        //                //    }
        //                //    if (str1.IndexOf("70") > 0)
        //                //    {
        //                //        from = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //        string newsub = str1.Substring(str1.IndexOf("70"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            if (from != DateTime.MinValue && topmo != DateTime.MinValue)
        //                //                detail.QERPpmo += Math.Round(from.Subtract(topmo).TotalDays, 2);
        //                //        }
        //                //        else
        //                //        {
        //                //            if (from != DateTime.MinValue && totemp != DateTime.MinValue)
        //                //                detail.QERP += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //        }
        //                //        if (to == from && totemp != DateTime.MinValue)
        //                //        {
        //                //            newtiecket.QERP += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //            totemp = DateTime.MinValue;
        //                //            flag = true;
        //                //        }
        //                //        else if (topmo != from && topmo != DateTime.MinValue)
        //                //        {
        //                //            newtiecket.QERP += Math.Round(from.Subtract(topmo).TotalDays, 2);
        //                //            topmo = DateTime.MinValue;
        //                //            flag1 = true;
        //                //        }
        //                //        else if (to != from && totemp == DateTime.MinValue && to != DateTime.MinValue)
        //                //        {
        //                //            newtiecket.QERP += Math.Round(from.Subtract(to).TotalDays, 2);
        //                //            to = DateTime.MinValue;
        //                //            flag2 = true;
        //                //            flag3 = true;
        //                //        }
        //                //    }
        //                //    if (str1.IndexOf("71") > 0)
        //                //    {
        //                //        from = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //        string newsub = str1.Substring(str1.IndexOf("71"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            if (from != DateTime.MinValue && topmo != DateTime.MinValue)
        //                //                detail.EDIpmo += Math.Round(from.Subtract(topmo).TotalDays, 2);
        //                //        }
        //                //        else
        //                //        {
        //                //            if (from != DateTime.MinValue && totemp != DateTime.MinValue)
        //                //                detail.EDI += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //        }
        //                //        if (to == from && totemp != DateTime.MinValue)
        //                //        {
        //                //            newtiecket.EDI += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //            totemp = DateTime.MinValue;
        //                //            flag = true;
        //                //        }
        //                //        else if (topmo != from && topmo != DateTime.MinValue)
        //                //        {
        //                //            newtiecket.EDI += Math.Round(from.Subtract(topmo).TotalDays, 2);
        //                //            topmo = DateTime.MinValue;
        //                //            flag1 = true;
        //                //        }
        //                //        else if (to != from && totemp == DateTime.MinValue && to != DateTime.MinValue)
        //                //        {
        //                //            newtiecket.EDI += Math.Round(from.Subtract(to).TotalDays, 2);
        //                //            to = DateTime.MinValue;
        //                //            flag2 = true;
        //                //            flag3 = true;
        //                //        }
        //                //    }
        //                //    if (str1.IndexOf("72") > 0)
        //                //    {
        //                //        from = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //        string newsub = str1.Substring(str1.IndexOf("72"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            if (from != DateTime.MinValue && topmo != DateTime.MinValue)
        //                //                detail.CWApmo += Math.Round(from.Subtract(topmo).TotalDays, 2);
        //                //        }
        //                //        else
        //                //        {
        //                //            if (from != DateTime.MinValue && totemp != DateTime.MinValue)
        //                //                detail.CWA += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //        }
        //                //        if (to == from && totemp != DateTime.MinValue)
        //                //        {
        //                //            newtiecket.CWA += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //            totemp = DateTime.MinValue;
        //                //            flag = true;
        //                //        }
        //                //        else if (topmo != from && topmo != DateTime.MinValue)
        //                //        {
        //                //            newtiecket.CWA += Math.Round(from.Subtract(topmo).TotalDays, 2);
        //                //            topmo = DateTime.MinValue;
        //                //            flag1 = true;
        //                //        }
        //                //        else if (to != from && totemp == DateTime.MinValue && to != DateTime.MinValue)
        //                //        {
        //                //            newtiecket.CWA += Math.Round(from.Subtract(to).TotalDays, 2);
        //                //            to = DateTime.MinValue;
        //                //            flag2 = true;
        //                //            flag3 = true;
        //                //        }
        //                //    }
        //                //    if (str1.IndexOf("73") > 0)
        //                //    {
        //                //        from = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //        string newsub = str1.Substring(str1.IndexOf("73"));
        //                //        if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        {
        //                //            if (from != DateTime.MinValue && topmo != DateTime.MinValue)
        //                //                detail.Html5pmo += Math.Round(from.Subtract(topmo).TotalDays, 2);
        //                //        }
        //                //        else
        //                //        {
        //                //            if (from != DateTime.MinValue && totemp != DateTime.MinValue)
        //                //                detail.Html5 += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //        }
        //                //        if (to == from && totemp != DateTime.MinValue)
        //                //        {
        //                //            newtiecket.Html5 += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //            totemp = DateTime.MinValue;
        //                //            flag = true;
        //                //        }
        //                //        else if (topmo != from && topmo != DateTime.MinValue)
        //                //        {
        //                //            newtiecket.Html5 += Math.Round(from.Subtract(topmo).TotalDays, 2);
        //                //            topmo = DateTime.MinValue;
        //                //            flag1 = true;
        //                //        }
        //                //        else if (to != from && totemp == DateTime.MinValue && to != DateTime.MinValue)
        //                //        {
        //                //            newtiecket.Html5 += Math.Round(from.Subtract(to).TotalDays, 2);
        //                //            to = DateTime.MinValue;
        //                //            flag2 = true;
        //                //            flag3 = true;
        //                //        }
        //                //    }
        //                //    if (str1.IndexOf("80") > 0)
        //                //    {

        //                //        from = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //        if (from != DateTime.MinValue && to != DateTime.MinValue)
        //                //        {
        //                //            if (from == to && totemp != DateTime.MinValue)
        //                //            {
        //                //                newtiecket.Review += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //                totemp = DateTime.MinValue;
        //                //                flag = true;
        //                //            }
        //                //            else if (from != to && to == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.Review += Math.Round(from.Subtract(DateTime.Parse(((Ticket)ti).CreateDate.ToString())).TotalDays, 2);
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //            else if (from != to && to != DateTime.MinValue && totemp == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.Review += Math.Round(from.Subtract(to).TotalDays, 2);
        //                //                to = DateTime.MinValue;
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //        }
        //                //    }
        //                //    if (str1.IndexOf("90") > 0)
        //                //    {
        //                //        from = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //        //string newsub = str1.Substring(str1.IndexOf("73"));
        //                //        //if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        //{
        //                //        //    if (from != DateTime.MinValue && topmo != DateTime.MinValue)
        //                //        //        detail.Html5pmo += from.Subtract(topmo).Days;
        //                //        //}
        //                //        //else
        //                //        //{
        //                //        //    if (from != DateTime.MinValue && totemp != DateTime.MinValue)
        //                //        //        detail += from.Subtract(totemp).Days;
        //                //        //}
        //                //        if (to == from && totemp != DateTime.MinValue)
        //                //        {
        //                //            newtiecket.DefectsWaitingSch += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //            totemp = DateTime.MinValue;
        //                //            flag = true;
        //                //        }
        //                //        else if (topmo != from && topmo != DateTime.MinValue)
        //                //        {
        //                //            newtiecket.DefectsWaitingSch += Math.Round(from.Subtract(topmo).TotalDays, 2);
        //                //            topmo = DateTime.MinValue;
        //                //            flag1 = true;
        //                //        }
        //                //        else if (to != from && totemp == DateTime.MinValue && to != DateTime.MinValue)
        //                //        {
        //                //            newtiecket.DefectsWaitingSch += Math.Round(from.Subtract(to).TotalDays, 2);
        //                //            to = DateTime.MinValue;
        //                //            flag2 = true;
        //                //            flag3 = true;
        //                //        }
        //                //    }
        //                //    if (str1.IndexOf("91") > 0)
        //                //    {
        //                //        from = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //        //string newsub = str1.Substring(str1.IndexOf("73"));
        //                //        //if (newsub.IndexOf("(Project Manager)") > 0)
        //                //        //{
        //                //        //    if (from != DateTime.MinValue && topmo != DateTime.MinValue)
        //                //        //        detail.Html5pmo += from.Subtract(topmo).Days;
        //                //        //}
        //                //        //else
        //                //        //{
        //                //        //    if (from != DateTime.MinValue && totemp != DateTime.MinValue)
        //                //        //        detail.Html5 += from.Subtract(totemp).Days;
        //                //        //}
        //                //        if (to == from && totemp != DateTime.MinValue)
        //                //        {
        //                //            newtiecket.Defectsindev += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //            totemp = DateTime.MinValue;
        //                //            flag = true;
        //                //        }
        //                //        else if (topmo != from && topmo != DateTime.MinValue)
        //                //        {
        //                //            newtiecket.Defectsindev += Math.Round(from.Subtract(topmo).TotalDays, 2);
        //                //            topmo = DateTime.MinValue;
        //                //            flag1 = true;
        //                //        }
        //                //        else if (to != from && totemp == DateTime.MinValue && to != DateTime.MinValue)
        //                //        {
        //                //            newtiecket.Defectsindev += Math.Round(from.Subtract(to).TotalDays, 2);
        //                //            to = DateTime.MinValue;
        //                //            flag2 = true;
        //                //            flag3 = true;
        //                //        }

        //                //    }
        //                //    if (str1.IndexOf("95") > 0)
        //                //    {
        //                //        from = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //        if (from != DateTime.MinValue && to != DateTime.MinValue)
        //                //        {
        //                //            if (from == to && totemp != DateTime.MinValue)
        //                //            {
        //                //                newtiecket.Destribution += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //                totemp = DateTime.MinValue;
        //                //                flag = true;
        //                //            }
        //                //            else if (from != to && to == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.Destribution += Math.Round(from.Subtract(DateTime.Parse(((Ticket)ti).CreateDate.ToString())).TotalDays, 2);
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //            else if (from != to && to != DateTime.MinValue && totemp == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.Destribution += Math.Round(from.Subtract(to).TotalDays, 2);
        //                //                to = DateTime.MinValue;
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //        }
        //                //    }
        //                //    if (str1.IndexOf("96") > 0)
        //                //    {
        //                //        from = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //        if (from != DateTime.MinValue && to != DateTime.MinValue)
        //                //        {
        //                //            if (from == to && totemp != DateTime.MinValue)
        //                //            {
        //                //                newtiecket.IT += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //                totemp = DateTime.MinValue;
        //                //                flag = true;
        //                //            }
        //                //            else if (from != to && to == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.IT += Math.Round(from.Subtract(DateTime.Parse(((Ticket)ti).CreateDate.ToString())).TotalDays, 2);
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //            else if (from != to && to != DateTime.MinValue && totemp == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.IT += Math.Round(from.Subtract(to).TotalDays, 2);
        //                //                to = DateTime.MinValue;
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //        }
        //                //    }
        //                //    if (str1.IndexOf("Product management") > 0)
        //                //    {
        //                //        from = DateTime.Parse(note2.LastActivityDate.ToString());
        //                //        if (from != DateTime.MinValue && to != DateTime.MinValue)
        //                //        {
        //                //            if (from == to && totemp != DateTime.MinValue)
        //                //            {
        //                //                newtiecket.PMO += Math.Round(from.Subtract(totemp).TotalDays, 2);
        //                //                totemp = DateTime.MinValue;
        //                //                flag = true;
        //                //            }
        //                //            else if (from != to && to == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.PMO += Math.Round(from.Subtract(DateTime.Parse(((Ticket)ti).CreateDate.ToString())).TotalDays, 2);
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //            else if (from != to && to != DateTime.MinValue && totemp == DateTime.MinValue)
        //                //            {
        //                //                newtiecket.PMO += Math.Round(from.Subtract(to).TotalDays, 2);
        //                //                to = DateTime.MinValue;
        //                //                flag = true;
        //                //                flag1 = true;
        //                //                flag2 = true;
        //                //                flag3 = true;
        //                //            }
        //                //        }
        //                //    }
        //                //}
        //                #endregion
        //                TicketNote note2 = autotasknotes[i];
        //               //check the index of the word forwared from and forwared to 
        //                int indexfrom = note2.Description.ToString().IndexOf("Forwarded From:");

        //                int indexto = note2.Description.ToString().IndexOf("Forwarded To:");
        //                if (indexto > 0)
        //                {
        //                    string Str = note2.Description.ToString().Substring(indexto);
        //                    //get the index of queue word and primary word to get specfically the name of queue that the ticket forward to                          
        //                    int indexofqueue = Str.IndexOf("Queue:");
        //                    int indexofprimaryresource = Str.IndexOf("Primary");
        //                    string subqueue = Str.Substring(indexofqueue + 7, (indexofprimaryresource - 10) - indexofqueue);
        //                    if (Queues_with_pmo_dic.Keys.Contains(subqueue))
        //                    {
        //                        string newsub = Str.Substring(Str.IndexOf(subqueue));
        //                        if (newsub.IndexOf("(Project Manager)") > 0)
        //                        {
        //                            /// [3] is the date tiem that ticket forwared to queue but assigned  on project manger resource 
        //                            Queues_with_pmo_dic[subqueue][3] = DateTime.Parse(note2.LastActivityDate.ToString());
        //                        }
        //                        else
        //                        {
        //                            if (Queues_with_pmo_dic[subqueue][0] != null)
        //                            {
        //                                /// [2] that temporary save the date time that ticket forwared to queue
        //                                Queues_with_pmo_dic[subqueue][2] = Queues_with_pmo_dic[subqueue][0];
        //                            }
        //                            ///[0] the date time that ticket forwared to the queue 
        //                            Queues_with_pmo_dic[subqueue][0] = DateTime.Parse(note2.LastActivityDate.ToString());
        //                        }
        //                    }
        //                    else if (Queues_without_pmo_dic.Keys.Contains(subqueue))
        //                    {
        //                        if (Queues_without_pmo_dic[subqueue][0] != null)
        //                        {
        //                            Queues_without_pmo_dic[subqueue][2] = Queues_without_pmo_dic[subqueue][0];
        //                        }
        //                        Queues_without_pmo_dic[subqueue][0] = DateTime.Parse(note2.LastActivityDate.ToString());
        //                        if (i == autotasknotes.Count - 1)
        //                        {
        //                            double x = Convert.ToDouble(Queues_without_pmo_dic[subqueue][3]);
        //                            x += Math.Round((DateTime.Parse(((Ticket)ti).CompletedDate.ToString())).Subtract((DateTime)Queues_without_pmo_dic[subqueue][0]).TotalDays, 2);
        //                            Queues_without_pmo_dic[subqueue][3] = x;
        //                        }
        //                    }
        //                }
        //                if (indexfrom > 0)
        //                {
        //                    // substring 100 character form the discription to prevent 2 queue te repeated in this sub string 
        //                    string Str = note2.Description.ToString().Substring(indexfrom, 100);
        //                    //get the index of queue word and primary wod to get specfically the name of queue that the ticket forward from 
        //                    int indexofqueue = Str.IndexOf("Queue:");
        //                    int indexofprimaryresource = Str.IndexOf("Primary");
        //                    string subqueue = Str.Substring(indexofqueue + 7, (indexofprimaryresource - 10) - indexofqueue);
        //                    if (Queues_with_pmo_dic.Keys.Contains(subqueue))
        //                    {
        //                        Queues_with_pmo_dic[subqueue][1] = DateTime.Parse(note2.LastActivityDate.ToString());
        //                        string newsub = Str.Substring(Str.IndexOf(subqueue));
        //                        if (newsub.IndexOf("(Project Manager)") > 0)
        //                        {
        //                            if ((DateTime)Queues_with_pmo_dic[subqueue][1] != DateTime.MinValue && Queues_with_pmo_dic[subqueue][3] != null)
        //                            { 
        //                                // two [subqueue][3] is the date that assigned to project manager (pmo )
        //                                //two [subqueue][5] is the total days ticket spend n project management 
        //                                double temp = Convert.ToDouble(Queues_with_pmo_dic[subqueue][5]);
        //                                temp += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_with_pmo_dic[subqueue][3]).Date).TotalDays,0);
        //                                Queues_with_pmo_dic[subqueue][5] = temp;
        //                            }
        //                            else if (Queues_with_pmo_dic[subqueue][1] != null && Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) == Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) && i == 0)
        //                            {
        //                                double temp = Convert.ToDouble(Queues_with_pmo_dic[subqueue][5]);
        //                                temp += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(DateTime.Parse(((Ticket)ti).CreateDate.ToString()).Date).TotalDays,0);
        //                                Queues_with_pmo_dic[subqueue][5] = temp;
        //                            }

        //                        }
        //                        if ((Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) == Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) || Queues_with_pmo_dic[subqueue][0] == null) && Queues_with_pmo_dic[subqueue][3] != null)
        //                        {
        //                            //two [subqueue][4] is the total days spend in this queue 
        //                            double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
        //                            x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_with_pmo_dic[subqueue][3]).Date).TotalDays,0);
        //                            Queues_with_pmo_dic[subqueue][4] = x;
        //                            Queues_with_pmo_dic[subqueue][3] = null;
        //                        }
        //                        else if ((DateTime)Queues_with_pmo_dic[subqueue][1] != DateTime.MinValue && Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) != Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) && Queues_with_pmo_dic[subqueue][0] != null)
        //                        {
        //                            double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
        //                            x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_with_pmo_dic[subqueue][0]).Date).TotalDays,0);
        //                            Queues_with_pmo_dic[subqueue][4] = x;
        //                            Queues_with_pmo_dic[subqueue][0] = null;
        //                        }
        //                        else if ((DateTime)Queues_with_pmo_dic[subqueue][1] != DateTime.MinValue && Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) == Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) && Queues_with_pmo_dic[subqueue][2] != null)
        //                        {
        //                            double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
        //                            x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_with_pmo_dic[subqueue][2]).Date).TotalDays,0);
        //                            Queues_with_pmo_dic[subqueue][4] = x;
        //                            Queues_with_pmo_dic[subqueue][2] = null;
        //                        }
        //                        else if (Queues_with_pmo_dic[subqueue][1] != null && Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) == Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) && i == 0)
        //                        {
        //                            double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
        //                            x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(DateTime.Parse(((Ticket)ti).CreateDate.ToString()).Date).TotalDays,0);
        //                            Queues_with_pmo_dic[subqueue][4] = x;
        //                        }
        //                    }
        //                    else if (Queues_without_pmo_dic.Keys.Contains(subqueue))
        //                    {
        //                        Queues_without_pmo_dic[subqueue][1] = DateTime.Parse(note2.LastActivityDate.ToString());
        //                        if ((DateTime)Queues_without_pmo_dic[subqueue][1] != DateTime.MinValue && Convert.ToDateTime(Queues_without_pmo_dic[subqueue][1]) == Convert.ToDateTime(Queues_without_pmo_dic[subqueue][0]) && Queues_without_pmo_dic[subqueue][0] != null && i == 0)
        //                        {
        //                            //one [subqueue][3] is the total days spend in this queue 
        //                            double x = Convert.ToDouble(Queues_without_pmo_dic[subqueue][3]);
        //                            x += Math.Round(((DateTime)Queues_without_pmo_dic[subqueue][1]).Date.Subtract(DateTime.Parse(((Ticket)ti).CreateDate.ToString()).Date).TotalDays,0);
        //                            Queues_without_pmo_dic[subqueue][3] = x;
        //                            Queues_without_pmo_dic[subqueue][0] = null;
        //                        }
        //                        else if ((DateTime)Queues_without_pmo_dic[subqueue][1] != DateTime.MinValue && Convert.ToDateTime(Queues_without_pmo_dic[subqueue][1]) != Convert.ToDateTime(Queues_without_pmo_dic[subqueue][0]) && Queues_without_pmo_dic[subqueue][0] != null)
        //                        {
        //                            //one [subqueue][3] is the total days spend in this queue 
        //                            double x = Convert.ToDouble(Queues_without_pmo_dic[subqueue][3]);
        //                            x += Math.Round(((DateTime)Queues_without_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_without_pmo_dic[subqueue][0]).Date).TotalDays,0);
        //                            Queues_without_pmo_dic[subqueue][3] = x;
        //                            Queues_without_pmo_dic[subqueue][0] = null;
        //                        }
        //                        else if ((DateTime)Queues_without_pmo_dic[subqueue][1] != DateTime.MinValue && Convert.ToDateTime(Queues_without_pmo_dic[subqueue][1]) == Convert.ToDateTime(Queues_without_pmo_dic[subqueue][0]) && Queues_without_pmo_dic[subqueue][2] != null)
        //                        {
        //                            double x = Convert.ToDouble(Queues_without_pmo_dic[subqueue][3]);
        //                            x += Math.Round(((DateTime)Queues_without_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_without_pmo_dic[subqueue][2]).Date).TotalDays,0);
        //                            Queues_without_pmo_dic[subqueue][3] = x;
        //                            Queues_without_pmo_dic[subqueue][2] = null;
        //                        }
        //                        else if (Queues_without_pmo_dic[subqueue][1] != null && Queues_without_pmo_dic[subqueue][0] == null && i == 0)
        //                        {
        //                            double x = Convert.ToDouble(Queues_without_pmo_dic[subqueue][3]);
        //                            x += Math.Round(((DateTime)Queues_without_pmo_dic[subqueue][1]).Date.Subtract(DateTime.Parse(((Ticket)ti).CreateDate.ToString()).Date).TotalDays,0);
        //                            Queues_without_pmo_dic[subqueue][3] = x;
        //                        }
                                
        //                    } 
        //                }
        //            }
        //        }
        //    }
        //    // assign each queue total time to ticket queue property 
        //    #region commented code
        //    //newtiecket.Sales = Convert.ToDouble(Queues_without_pmo_dic["00 Sales"][3]);
        //    //newtiecket.Helpdesk = Convert.ToDouble(Queues_without_pmo_dic["20 Help desk"][3]);
        //    //newtiecket.CustomerCare = Convert.ToDouble(Queues_without_pmo_dic["10 Customer Care"][3]);
        //    //newtiecket.ConsultDispatch = Convert.ToDouble(Queues_without_pmo_dic["23 Consulting to Dispatch"][3]);
        //    //newtiecket.Webbased = Convert.ToDouble(Queues_without_pmo_dic["21 Web Based Support"][3]);
        //    //newtiecket.P_C_Waiting = Convert.ToDouble(Queues_without_pmo_dic["24 Pc waiting for sign of"][3]);
        //    //newtiecket.CustomerAcceptance = Convert.ToDouble(Queues_without_pmo_dic["50 Customer Acceptance"][3]);
        //    //newtiecket.PMO = Convert.ToDouble(Queues_without_pmo_dic["Product management "][3]);
        //    //newtiecket.Validation = Convert.ToDouble(Queues_without_pmo_dic["69 Validation"][3]);
        //    //newtiecket.Review = Convert.ToDouble(Queues_without_pmo_dic["80 Review"][3]);
        //    //newtiecket.DefectsWaitingSch = Convert.ToDouble(Queues_without_pmo_dic["90 Defects Waiting Sch."][3]);
        //    //newtiecket.Defectsindev = Convert.ToDouble(Queues_without_pmo_dic["91 Defects (In Dev.)"][3]);
        //    //newtiecket.Destribution = Convert.ToDouble(Queues_without_pmo_dic["95 Distribution"][3]);
        //    //newtiecket.IT = Convert.ToDouble(Queues_without_pmo_dic["96 IT"][3]);
        //    //newtiecket.QERP = Convert.ToDouble(Queues_with_pmo_dic["70 ERP"][4]);
        //    //new_ticket_detail.QERPpmo = Convert.ToDouble(Queues_with_pmo_dic["70 ERP"][5]);
        //    //new_ticket_detail.QERP = newtiecket.QERP - new_ticket_detail.QERPpmo;
        //    //newtiecket.EDI = Convert.ToDouble(Queues_with_pmo_dic["71 EDI"][4]);
        //    //new_ticket_detail.EDIpmo = Convert.ToDouble(Queues_with_pmo_dic["71 EDI"][5]);
        //    //new_ticket_detail.EDI = newtiecket.EDI - new_ticket_detail.EDIpmo;
        //    //newtiecket.CWA = Convert.ToDouble(Queues_with_pmo_dic["72 CWA"][4]);
        //    //new_ticket_detail.CWApmo = Convert.ToDouble(Queues_with_pmo_dic["72 CWA"][5]);
        //    //new_ticket_detail.CWA = newtiecket.CWA - new_ticket_detail.CWApmo;
        //    //newtiecket.Html5 = Convert.ToDouble(Queues_with_pmo_dic["73 HTML5"][4]);
        //    //new_ticket_detail.Html5pmo = Convert.ToDouble(Queues_with_pmo_dic["73 HTML5"][5]);
        //    //new_ticket_detail.Html5 = newtiecket.Html5 - new_ticket_detail.Html5pmo;
        //    #endregion
        //    newtiecket.TiecketNum = ((Ticket)ti).TicketNumber.ToString();
        //    newtiecket.Completedate = DateTime.Parse(((Ticket)ti).CompletedDate.ToString());
        //    newtiecket.Ticketage = Math.Round((DateTime.Parse(((Ticket)ti).CompletedDate.ToString())).Date.Subtract(DateTime.Parse(((Ticket)ti).CreateDate.ToString()).Date).TotalDays,0);
        //    newtiecket.Title = ((Ticket)ti).Title.ToString();
        //    //ATA check on user defined field number 1 in the array which is base line due date and number 2 which is instlation date 
        //      if (((Ticket)ti).UserDefinedFields.Count() > 0 && ((Ticket)ti).UserDefinedFields[0].Value != null && ((Ticket)ti).UserDefinedFields[2].Value != null)
        //      {
        //        newtiecket.DeliveryOnTime = Math.Round(DateTime.Parse(((Ticket)ti).UserDefinedFields[2].Value.ToString()).Subtract(DateTime.Parse(((Ticket)ti).UserDefinedFields[0].Value.ToString())).TotalDays,0);
        //      }
        //      newtiecket.UDF = (Udfenum)int.Parse(((Ticket)ti).UserDefinedFields[8].Value.ToString());
        //      if (((Ticket)ti).AccountID.ToString() != null)
        //      {
        //          StringBuilder selectaccount = new StringBuilder();
        //          selectaccount.Append("<queryxml><entity>Account</entity>").Append(System.Environment.NewLine);
        //          selectaccount.Append("<query><condition><field>id<expression op=\"Equals\">" + ((Ticket)ti).AccountID.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
        //          selectaccount.Append("</queryxml>").Append(System.Environment.NewLine);
        //          var account = clientAuto.query(at_integrations, selectaccount.ToString());
        //          if (account.ReturnCode == 1)
        //          {
        //              foreach (Aria5SystemAdmin.Module.SubAutoTask1.Account ac in account.EntityResults)
        //              {
        //                  newtiecket.Account = ac.AccountName.ToString();
        //                  if (ac.UserDefinedFields[0].Value != null)
        //                  {
        //                      Aria5SystemAdmin.Module.BusinessObjects.Account system_account = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Id] = '" + ac.UserDefinedFields[0].Value.ToString() + "'"));
        //                      if (system_account != null)
        //                      {

        //                          newtiecket.Account_profile = system_account;
        //                          newtiecket.Save();
        //                          newtiecket.Session.CommitTransaction();
        //                          new_ticket_detail.Ticket = newtiecket;
        //                          new_ticket_detail.Save();
        //                          new_ticket_detail.Session.CommitTransaction();

        //                      }
        //                      else
        //                      {

        //                          // newtiecket.Session.LockingOption = LockingOption.None;
        //                          Aria5SystemAdmin.Module.BusinessObjects.Account account_to_creat = Create_system_admin_account(ac.AccountName.ToString(),ac.UserDefinedFields[0].Value.ToString(),Guid.Empty,newobject);
        //                          Aria5SystemAdmin.Module.BusinessObjects.Account account_Exist = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Oid] = '" + account_to_creat.Oid + "'"));
        //                          if (account_Exist != null)
        //                          {

        //                              newtiecket.Account_profile = account_Exist;
        //                          }
        //                          newtiecket.Save();
        //                          newtiecket.Session.CommitTransaction();
        //                          new_ticket_detail.Ticket = newtiecket;
        //                          new_ticket_detail.Save();
        //                          new_ticket_detail.Session.CommitTransaction();

        //                      }
        //                  }
        //                  else
        //                  {
                             
        //                      Aria5SystemAdmin.Module.BusinessObjects.Account system_account = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Name] = '" + ac.AccountName.ToString() + "'"));
        //                      if (system_account != null)
        //                      {

        //                          newtiecket.Account_profile = system_account;
        //                          newtiecket.Save();
        //                          newtiecket.Session.CommitTransaction();
        //                          new_ticket_detail.Ticket = newtiecket;
        //                          new_ticket_detail.Save();
        //                          new_ticket_detail.Session.CommitTransaction();

        //                      }
        //                      else
        //                      {                                                                   
        //                          StringBuilder selectparentaccount = new StringBuilder();
        //                          selectparentaccount.Append("<queryxml><entity>Account</entity>").Append(System.Environment.NewLine);
        //                          selectparentaccount.Append("<query><condition><field>id<expression op=\"Equals\">" + ac.ParentAccountID.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
        //                          selectparentaccount.Append("</queryxml>").Append(System.Environment.NewLine);
        //                          var parentaccount = clientAuto.query(at_integrations, selectparentaccount.ToString());
        //                          if (parentaccount.ReturnCode == 1)
        //                          {
        //                              foreach (Aria5SystemAdmin.Module.SubAutoTask1.Account parentac in parentaccount.EntityResults)
        //                              {
        //                                  if (parentac.UserDefinedFields[0].Value.ToString() != null)
        //                                  {
        //                                      Aria5SystemAdmin.Module.BusinessObjects.Account systemparentaccount = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Id] = '" + parentac.UserDefinedFields[0].Value.ToString() + "'"));
        //                                      if (systemparentaccount == null)
        //                                      {
        //                                          Aria5SystemAdmin.Module.BusinessObjects.Account parent_account_to_creat = Create_system_admin_account(parentac.AccountName.ToString(), parentac.UserDefinedFields[0].Value.ToString(), Guid.Empty, newobject);
        //                                          Aria5SystemAdmin.Module.BusinessObjects.Account account_to_creat = Create_system_admin_account(ac.AccountName.ToString(), ac.AccountNumber.ToString(), parent_account_to_creat.Oid, newobject);
        //                                          Aria5SystemAdmin.Module.BusinessObjects.Account account_Exist = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Oid] = '" + account_to_creat.Oid + "'"));
        //                                          if (account_Exist != null)
        //                                          {

        //                                              newtiecket.Account_profile = account_Exist;
        //                                          }
        //                                          newtiecket.Save();
        //                                          newtiecket.Session.CommitTransaction();
        //                                          new_ticket_detail.Ticket = newtiecket;
        //                                          new_ticket_detail.Save();
        //                                          new_ticket_detail.Session.CommitTransaction();
        //                                      }
        //                                      else
        //                                      {
        //                                          Aria5SystemAdmin.Module.BusinessObjects.Account account_to_creat = Create_system_admin_account(ac.AccountName.ToString(), ac.AccountNumber.ToString(), systemparentaccount.Oid, newobject);
        //                                          Aria5SystemAdmin.Module.BusinessObjects.Account account_Exist = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Oid] = '" + account_to_creat.Oid + "'"));
        //                                          if (account_Exist != null)
        //                                          {

        //                                              newtiecket.Account_profile = account_Exist;
        //                                          }
        //                                          newtiecket.Save();
        //                                          newtiecket.Session.CommitTransaction();
        //                                          new_ticket_detail.Ticket = newtiecket;
        //                                          new_ticket_detail.Save();
        //                                          new_ticket_detail.Session.CommitTransaction();
        //                                      }
        //                                  }
        //                              }
        //                          }

        //                      }
        //                  }
        //              }
                     
        //          }
        //      }
        //      foreach (string key in Queues_without_pmo_dic.Keys)
        //      {
        //          if (Convert.ToInt32(Queues_without_pmo_dic[key][3]) > 0)
        //          {
        //              QueueName queuname = newobject.FindObject<QueueName>(CriteriaOperator.Parse("[Name] = '" + key + "'"));
        //              QueueValues queuvalue = newobject.CreateObject<QueueValues>();
        //              queuvalue.Type = queuname;
        //              queuvalue.TicketCompleteDate = newtiecket.Completedate;
        //              queuvalue.Value = Convert.ToDouble(Queues_without_pmo_dic[key][3]);
        //              queuvalue.Ticket = newtiecket;
        //              queuvalue.Save();
        //              queuvalue.Session.CommitTransaction();
        //          }
        //      }
        //      foreach (string key in Queues_with_pmo_dic.Keys)
        //      {
        //          if (Convert.ToInt32(Queues_with_pmo_dic[key][4]) > 0)
        //          {
        //              QueueName queuname = newobject.FindObject<QueueName>(CriteriaOperator.Parse("[Name] = '" + key + "'"));
        //              QueueValues queuvalue = newobject.CreateObject<QueueValues>();
        //              queuvalue.Type = queuname;
        //              queuvalue.TicketCompleteDate = newtiecket.Completedate;
        //              queuvalue.Value = Convert.ToDouble(Queues_with_pmo_dic[key][4]);
        //              queuvalue.Ticket = newtiecket;
        //              queuvalue.Save();
        //              queuvalue.Session.CommitTransaction();
        //          }
        //      }
        //      newobject.CommitChanges();
        //        return newtiecket;
        //}
        public TicketAHT CalculateAHT(IObjectSpace newobject, Ticket AutotaskTicket, Dictionary<string, object[]> Queues_with_pmo_dic, IList<QueueName> allqueues)//, Dictionary<string, object[]> Queues_with_pmo_dic)
        {
            // make sure that all dictionary values empty 
            foreach (var two_record in Queues_with_pmo_dic)
            {
                Array.Clear(two_record.Value, 0, two_record.Value.Length);
            }
            TicketAHT newtiecket = newobject.CreateObject<TicketAHT>();
            newtiecket.Session.LockingOption = LockingOption.None;
            TicketDetail new_ticket_detail = newobject.CreateObject<TicketDetail>();

            newtiecket.TiecketNum = AutotaskTicket.TicketNumber.ToString();
            newtiecket.Completedate = DateTime.Parse(AutotaskTicket.CompletedDate.ToString());
            newtiecket.Ticketage = Math.Round((DateTime.Parse(AutotaskTicket.CompletedDate.ToString())).Date.Subtract(DateTime.Parse(AutotaskTicket.CreateDate.ToString()).Date).TotalDays, 0);
            newtiecket.Title = (AutotaskTicket.Title.ToString());
            //select from auto task ticket not only which it's discription contain the word forward from to make ensure that all ticket retrived to will be used in calculation  
            StringBuilder selectnote = new StringBuilder();
            selectnote.Append("<queryxml><entity>TicketNote</entity>").Append(System.Environment.NewLine);
            selectnote.Append("<query><condition><field>TicketID<expression op=\"Equals\">" + ((Ticket)AutotaskTicket).id.ToString() + "</expression></field>").Append(System.Environment.NewLine);
            selectnote.Append("<field>Description<expression op=\"Contains\">" + "Forwarded From:" + "</expression></field></condition></query>").Append(System.Environment.NewLine);
            selectnote.Append("</queryxml>").Append(System.Environment.NewLine);
            var notes = clientAuto.query(at_integrations, selectnote.ToString());
            if (notes.ReturnCode == 1)
            {
                if (notes.EntityResults.Count() > 0)
                {
                    // reorder all ticket notes ascending according to ticket note date 
                    TicketNote[] array123 = Array.ConvertAll(notes.EntityResults, item => (TicketNote)item);
                    List<TicketNote> autotasknotes = array123.OrderBy(si => si.LastActivityDate).ToList();
                    for (int i = 0; i < autotasknotes.Count; i++)//TicketNote n in autotasknotes)
                    {
                        TicketNote note2 = autotasknotes[i];
                        //check the index of the word forwared from and forwared to 
                        int indexfrom = note2.Description.ToString().IndexOf("Forwarded From:");
                        int indexto = note2.Description.ToString().IndexOf("Forwarded To:");
                        if (indexto > 0)
                        {
                            string Str = note2.Description.ToString().Substring(indexto);
                            //get the index of queue word and primary word to get specfically the name of queue that the ticket forward to                          
                            int indexofqueue = Str.IndexOf("Queue:");
                            int indexofprimaryresource = Str.IndexOf("Primary");
                            string subqueue = Str.Substring(indexofqueue + 7, (indexofprimaryresource - 10) - indexofqueue);
                            if (Queues_with_pmo_dic.Keys.Contains(subqueue))
                            {
                                string newsub = Str.Substring(Str.IndexOf("Primary"),50);
                                if (newsub.IndexOf("(Project Manager)") > 0)
                                {
                                    /// [3] is the date tiem that ticket forwared to queue but assigned  on project manger resource 
                                    Queues_with_pmo_dic[subqueue][3] = DateTime.Parse(note2.LastActivityDate.ToString());
                                }
                                else
                                {
                                    if (Queues_with_pmo_dic[subqueue][0] != null)
                                    {
                                        /// [2] that temporary save the date time that ticket forwared to queue
                                        Queues_with_pmo_dic[subqueue][2] = Queues_with_pmo_dic[subqueue][0];
                                    }
                                    ///[0] the date time that ticket forwared to the queue 
                                    Queues_with_pmo_dic[subqueue][0] = DateTime.Parse(note2.LastActivityDate.ToString());
                                }
                                if (i == autotasknotes.Count - 1)
                                {
                                    double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
                                    x += Math.Round((DateTime.Parse(((Ticket)AutotaskTicket).CompletedDate.ToString()).Date).Subtract(((DateTime)Queues_with_pmo_dic[subqueue][0]).Date).TotalDays,0);
                                    Queues_with_pmo_dic[subqueue][4] = x;
                                }
                            }
                        }
                        if (indexfrom > 0)
                        {
                            // substring 100 character form the discription to prevent 2 queue te repeated in this sub string 
                            string Str = note2.Description.ToString().Substring(indexfrom, 100);
                            //get the index of queue word and primary wod to get specfically the name of queue that the ticket forward from 
                            int indexofqueue = Str.IndexOf("Queue:");
                            int indexofprimaryresource = Str.IndexOf("Primary");
                            string subqueue = Str.Substring(indexofqueue + 7, (indexofprimaryresource - 10) - indexofqueue);
                            if (Queues_with_pmo_dic.Keys.Contains(subqueue))
                            {
                                Queues_with_pmo_dic[subqueue][1] = DateTime.Parse(note2.LastActivityDate.ToString());
                                string newsub = Str.Substring(Str.IndexOf(subqueue));
                                if (newsub.IndexOf("(Project Manager)") > 0)
                                {
                                    if ((DateTime)Queues_with_pmo_dic[subqueue][1] != DateTime.MinValue && Queues_with_pmo_dic[subqueue][3] != null)
                                    {
                                        // two [subqueue][3] is the date that assigned to project manager (pmo )
                                        //two [subqueue][5] is the total days ticket spend n project management 
                                        double temp = Convert.ToDouble(Queues_with_pmo_dic[subqueue][5]);
                                        temp += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_with_pmo_dic[subqueue][3]).Date).TotalDays, 0);
                                        Queues_with_pmo_dic[subqueue][5] = temp;
                                    }
                                    else if (Queues_with_pmo_dic[subqueue][1] != null && Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) == Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) && i == 0)
                                    {
                                        double temp = Convert.ToDouble(Queues_with_pmo_dic[subqueue][5]);
                                        temp += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(DateTime.Parse(((Ticket)AutotaskTicket).CreateDate.ToString()).Date).TotalDays, 0);
                                        Queues_with_pmo_dic[subqueue][5] = temp;
                                    }

                                }
                                if ((Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) == Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) || Queues_with_pmo_dic[subqueue][0] == null) && Queues_with_pmo_dic[subqueue][3] != null)
                                {
                                    //two [subqueue][4] is the total days spend in this queue 
                                    double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
                                    x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_with_pmo_dic[subqueue][3]).Date).TotalDays, 0);
                                    Queues_with_pmo_dic[subqueue][4] = x;
                                    Queues_with_pmo_dic[subqueue][3] = null;
                                }
                                else if ((DateTime)Queues_with_pmo_dic[subqueue][1] != DateTime.MinValue && Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) != Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) && Queues_with_pmo_dic[subqueue][0] != null)
                                {
                                    double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
                                    x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_with_pmo_dic[subqueue][0]).Date).TotalDays, 0);
                                    Queues_with_pmo_dic[subqueue][4] = x;
                                    Queues_with_pmo_dic[subqueue][0] = null;
                                }
                                else if ((DateTime)Queues_with_pmo_dic[subqueue][1] != DateTime.MinValue && Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) == Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) && Queues_with_pmo_dic[subqueue][2] != null)
                                {
                                    double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
                                    x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_with_pmo_dic[subqueue][2]).Date).TotalDays, 0);
                                    Queues_with_pmo_dic[subqueue][4] = x;
                                    Queues_with_pmo_dic[subqueue][2] = null;
                                }
                                else if (Queues_with_pmo_dic[subqueue][1] != null && Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) == Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) && i == 0)
                                {
                                    double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
                                    x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(DateTime.Parse(((Ticket)AutotaskTicket).CreateDate.ToString()).Date).TotalDays, 0);
                                    Queues_with_pmo_dic[subqueue][4] = x;
                                }
                                else if (Queues_with_pmo_dic[subqueue][1] != null && Queues_with_pmo_dic[subqueue][0] == null && i == 0)
                                {
                                    double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
                                    x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(DateTime.Parse(((Ticket)AutotaskTicket).CreateDate.ToString()).Date).TotalDays, 0);
                                    Queues_with_pmo_dic[subqueue][4] = x;
                                }
                            }
                        }
                    }
                }
            }
            //ATA check on user defined field number 1 in the array which is base line due date and number 2 which is instlation date 
            if (((Ticket)AutotaskTicket).UserDefinedFields.Count() > 0 && AutotaskTicket.UserDefinedFields[0].Value != null && AutotaskTicket.UserDefinedFields[2].Value != null)
            {
                newtiecket.DeliveryOnTime = Math.Round(DateTime.Parse(AutotaskTicket.UserDefinedFields[2].Value.ToString()).Subtract(DateTime.Parse(AutotaskTicket.UserDefinedFields[0].Value.ToString())).TotalDays, 0);
            }
            newtiecket.UDF = (Udfenum)int.Parse(AutotaskTicket.UserDefinedFields[8].Value.ToString());
            if (AutotaskTicket.AccountID.ToString() != null)
            {
                StringBuilder selectaccount = new StringBuilder();
                selectaccount.Append("<queryxml><entity>Account</entity>").Append(System.Environment.NewLine);
                selectaccount.Append("<query><condition><field>id<expression op=\"Equals\">" + AutotaskTicket.AccountID.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                selectaccount.Append("</queryxml>").Append(System.Environment.NewLine);
                var account = clientAuto.query(at_integrations, selectaccount.ToString());
                if (account.ReturnCode == 1)
                {
                    foreach (Aria5SystemAdmin.Module.SubAutoTask1.Account ac in account.EntityResults)
                    {
                        newtiecket.Account = ac.AccountName.ToString();
                        if (ac.UserDefinedFields[0].Value != null)
                        {
                            Aria5SystemAdmin.Module.BusinessObjects.Account system_account = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Id] = '" + ac.UserDefinedFields[0].Value.ToString() + "'"));
                            if (system_account != null)
                            {
                                newtiecket.Account_profile = system_account;
                            }
                            else
                            {
                                // newtiecket.Session.LockingOption = LockingOption.None;
                                Aria5SystemAdmin.Module.BusinessObjects.Account account_to_creat = Create_system_admin_account(ac.AccountName.ToString(), ac.UserDefinedFields[0].Value.ToString(), Guid.Empty, newobject);
                                Aria5SystemAdmin.Module.BusinessObjects.Account account_Exist = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Oid] = '" + account_to_creat.Oid + "'"));
                                if (account_Exist != null)
                                {
                                    newtiecket.Account_profile = account_Exist;
                                }
                            }
                        }
                        else
                        {
                            Aria5SystemAdmin.Module.BusinessObjects.Account system_account = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Name] = '" + ac.AccountName.ToString() + "'"));
                            if (system_account != null)
                            {

                                newtiecket.Account_profile = system_account;
                            }
                            else
                            {
                                StringBuilder selectparentaccount = new StringBuilder();
                                selectparentaccount.Append("<queryxml><entity>Account</entity>").Append(System.Environment.NewLine);
                                selectparentaccount.Append("<query><condition><field>id<expression op=\"Equals\">" + ac.ParentAccountID.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                                selectparentaccount.Append("</queryxml>").Append(System.Environment.NewLine);
                                var parentaccount = clientAuto.query(at_integrations, selectparentaccount.ToString());
                                if (parentaccount.ReturnCode == 1)
                                {
                                    foreach (Aria5SystemAdmin.Module.SubAutoTask1.Account parentac in parentaccount.EntityResults)
                                    {
                                        if (parentac.UserDefinedFields[0].Value.ToString() != null)
                                        {
                                            Aria5SystemAdmin.Module.BusinessObjects.Account systemparentaccount = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Id] = '" + parentac.UserDefinedFields[0].Value.ToString() + "'"));
                                            if (systemparentaccount == null)
                                            {
                                                Aria5SystemAdmin.Module.BusinessObjects.Account parent_account_to_creat = Create_system_admin_account(parentac.AccountName.ToString(), parentac.UserDefinedFields[0].Value.ToString(), Guid.Empty, newobject);
                                                Aria5SystemAdmin.Module.BusinessObjects.Account account_to_creat = Create_system_admin_account(ac.AccountName.ToString(), ac.AccountNumber.ToString(), parent_account_to_creat.Oid, newobject);
                                                Aria5SystemAdmin.Module.BusinessObjects.Account account_Exist = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Oid] = '" + account_to_creat.Oid + "'"));
                                                if (account_Exist != null)
                                                {
                                                    newtiecket.Account_profile = account_Exist;
                                                }
                                            }
                                            else
                                            {
                                                Aria5SystemAdmin.Module.BusinessObjects.Account account_to_creat = Create_system_admin_account(ac.AccountName.ToString(), ac.AccountNumber.ToString(), systemparentaccount.Oid, newobject);
                                                Aria5SystemAdmin.Module.BusinessObjects.Account account_Exist = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Oid] = '" + account_to_creat.Oid + "'"));
                                                if (account_Exist != null)
                                                {

                                                    newtiecket.Account_profile = account_Exist;
                                                }
                                            }
                                        }
                                    }
                                }

                            }
                        }
                    }
                }
            }
          //System.Collections.IList queuevalues = newobject.CreateCollection(typeof(QueueValues));
            foreach (string key in Queues_with_pmo_dic.Keys)
            {
                QueueValues queuvalue = newobject.CreateObject<QueueValues>();
                if (Queues_with_pmo_dic[key][4] != null)
                {
                    // QueueName queuname = newobject.FindObject<QueueName>(CriteriaOperator.Parse("[Name] = '" + key + "'"));
                    QueueName queuname = allqueues.FirstOrDefault(x => x.Name == key);
                    queuvalue.Type = queuname;
                    queuvalue.TicketCompleteDate = newtiecket.Completedate;
                    queuvalue.Value = Convert.ToDouble(Queues_with_pmo_dic[key][4]);
                  // queuvalue.Ticket = newtiecket;
                    newtiecket.QueuesValues.Add(queuvalue);
                    //newtiecket.QueuesValues.Add(queuvalue);
                    //queuvalue.Save();
                    //queuvalue.Session.CommitTransaction();
                }
               
            }
            IList<Aria5SystemAdmin.Module.BusinessObjects.Department> departments = newobject.GetObjects<Aria5SystemAdmin.Module.BusinessObjects.Department>();
            foreach (Aria5SystemAdmin.Module.BusinessObjects.Department Departmen in departments)
            {
                if (newtiecket.QueuesValues.Where(x => x.Type.Department == Departmen).Count()> 0)
                {
                    int queuevalues = 0;
                    foreach (QueueValues queueval in newtiecket.QueuesValues.Where(x => x.Type.Department == Departmen))
                    {
                        queuevalues += (int)queueval.Value;
                    }
                    Aria5SystemAdmin.Module.BusinessObjects.DepartmentAHT DepAHT = newobject.CreateObject<Aria5SystemAdmin.Module.BusinessObjects.DepartmentAHT>();
                    DepAHT.Department = Departmen;
                    DepAHT.Value = queuevalues;
                    newtiecket.DepartmentsAHT.Add(DepAHT);
                }
                
            }
            //new_ticket_detail.QERPpmo = Convert.ToDouble(Queues_with_pmo_dic["70 ERP"][5]);
            //new_ticket_detail.QERP = Convert.ToDouble(Queues_with_pmo_dic["70 ERP"][4]) - new_ticket_detail.QERPpmo;
            //new_ticket_detail.EDIpmo = Convert.ToDouble(Queues_with_pmo_dic["71 EDI"][5]);
            //new_ticket_detail.EDI = Convert.ToDouble(Queues_with_pmo_dic["71 EDI"][4]) - new_ticket_detail.EDIpmo;
            //new_ticket_detail.CWApmo = Convert.ToDouble(Queues_with_pmo_dic["72 CWA"][5]);
            //new_ticket_detail.CWA = Convert.ToDouble(Queues_with_pmo_dic["72 CWA"][4]) - new_ticket_detail.CWApmo;
            //new_ticket_detail.Html5pmo = Convert.ToDouble(Queues_with_pmo_dic["73 HTML5"][5]);
            //new_ticket_detail.Html5 = Convert.ToDouble(Queues_with_pmo_dic["73 HTML5"][4]) - new_ticket_detail.Html5pmo;
            //newtiecket.TicketDetail.Add(new_ticket_detail);
            newtiecket.Save();
            newobject.CommitChanges();
            return newtiecket;
        }
        public Aria5SystemAdmin.Module.BusinessObjects.Account Create_system_admin_account(string accountname, string accountID, Guid parentaccount, IObjectSpace newobject)
        {
            Aria5SystemAdmin.Module.BusinessObjects.Account account_to_creat = newobject.CreateObject<Aria5SystemAdmin.Module.BusinessObjects.Account>();
            account_to_creat.Name = accountname;
            account_to_creat.Id = accountID;
            account_to_creat.ParentContact = parentaccount;
            account_to_creat.Save();
            account_to_creat.Session.CommitTransaction();
            return account_to_creat;
        }
        public void CreateAHTlist(string[] array,AverageHandleTime AHTList, TicketAHT newticket, IObjectSpace object1)
        {
            
            if (array.Contains(newticket.UDF.ToString()))
            {
                TicketAHT ticket_to_creat = AHTList.Session.FindObject<TicketAHT>(CriteriaOperator.Parse("[Oid] = '" + newticket.Oid + "'"));
                if (AHTList.Tickets.Count > 0)
                {
                    if (!AHTList.Tickets.Contains(ticket_to_creat))
                    {
                        AHTList.Tickets.Add(ticket_to_creat);
                        AHTList.Save();
                        AHTList.Session.CommitTransaction();
                    }
                }
                else
                {
                    AHTList.Tickets.Add(ticket_to_creat);
                    AHTList.Save();
                    AHTList.Session.CommitTransaction();
                }
            }
        }
        //ATA 17/5/2016 calculate AHT for eachticket task  [End ]
        /// <summary>
        /// calculate DOT for projects or tickets or department or resource depend on DOT type that user select
        /// in Delivery on time screen 
        /// </summary>
        /// <param name="from"></param>
        /// <param name="to"></param>
        /// <param name="newobject"></param>
        /// <param name="DOT"></param>
        /// <param name="ismaster"></param>
        public void get_Dot_by_date_range(DateTime from, DateTime to, IObjectSpace newobject, DeliveryOnTime DOT,bool ismaster)
        {
            //Check which DOT type User want to calculate 
            //if (DOT.DotType == DeliveryOnTime.DOTType.Tickets)
            //{
            //    #region ticketsdot
            //    StringBuilder Selecttiecket = new StringBuilder();
            //    Selecttiecket.Append("<queryxml><entity>Ticket</entity>").Append(System.Environment.NewLine);
            //    Selecttiecket.Append("<query><condition><field>CompletedDate<expression op=\"greaterthan\">" + from + "</expression></field></condition>").Append(System.Environment.NewLine);
            //    Selecttiecket.Append("<condition><field>CompletedDate<expression op=\"LessThan\">" + to + "</expression></field></condition></query>").Append(System.Environment.NewLine);
            //    Selecttiecket.Append("</queryxml>").Append(System.Environment.NewLine);
            //    var tieckets = clientAuto.query(at_integrations, Selecttiecket.ToString());
            //    if (tieckets.ReturnCode == 1)
            //    {

            //        if (tieckets.EntityResults.Count() > 0)
            //        {
            //            foreach (var ticket in tieckets.EntityResults)
            //            {
            //                //check if this ticket have user defined field or not if it have we can calculate it's AHT if not we needn't to calculate it 
            //                if (((Ticket)ticket).UserDefinedFields.Count() > 0 && ((Ticket)ticket).UserDefinedFields[8].Value != null)
            //                {
            //                    TicketAHT existticket = newobject.FindObject<TicketAHT>(CriteriaOperator.Parse("[TiecketNum] = '" + ((Ticket)ticket).TicketNumber + "'"));
            //                    if (existticket != null)
            //                    {
            //                        CreateDotlist(DOT, existticket, newobject);
            //                    }
            //                    else
            //                    {
            //                        Dictionary<string, object[]> Queues_without_pmo_dic = new Dictionary<string, object[]>();
            //                        Dictionary<string, object[]> Queues_with_pmo_dic = new Dictionary<string, object[]>();
            //                        Dictionary<string, object[]> Allqueuesdic = new Dictionary<string, object[]>();
            //                        IList<QueueName> allqueues = newobject.GetObjects<QueueName>();
            //                        foreach (var item in allqueues)
            //                        {
            //                            Allqueuesdic.Add(item.Name, new object[6]);
            //                        }
            //                        #region dectionary Queues_without_pmo_dic
            //                        ///<summary>
            //                        /// this array of objects  element of index [0] is the datetime that ticket forwared to the queue element of index [1]
            //                        /// is the datetiem that ticket forwared from the queue and the element of index [2] is a temproray datetime to save the date 
            //                        /// if ticket moved from the queue to the same queue element of index [3] is the total of days that ticket spend in the queue 
            //                        ///</summary>

            //                        //fill the dectionary 
            //                        Queues_without_pmo_dic.Add("00 Sales", new object[4]);
            //                        Queues_without_pmo_dic.Add("10 Customer Care", new object[4]);
            //                        Queues_without_pmo_dic.Add("20 Help desk", new object[4]);
            //                        Queues_without_pmo_dic.Add("21 Web Based Support", new object[4]);
            //                        Queues_without_pmo_dic.Add("23 Consulting to Dispatch", new object[4]);
            //                        Queues_without_pmo_dic.Add("24 Pc waiting for sign of", new object[4]);
            //                        Queues_without_pmo_dic.Add("50 Customer Acceptance", new object[4]);
            //                        Queues_without_pmo_dic.Add("69 Validation", new object[4]);
            //                        Queues_without_pmo_dic.Add("98 Pending Enhancement", new object[4]);
            //                        Queues_without_pmo_dic.Add("99 Hold Entries", new object[4]);
            //                        Queues_without_pmo_dic.Add("Product management ", new object[4]);
            //                        Queues_without_pmo_dic.Add("Billing Disputes", new object[4]);
            //                        Queues_without_pmo_dic.Add("Postal Sale", new object[4]);
            //                        Queues_without_pmo_dic.Add("80 Review", new object[4]);
            //                        Queues_without_pmo_dic.Add("90 Defects Waiting Sch.", new object[4]);
            //                        Queues_without_pmo_dic.Add("91 Defects (In Dev.)", new object[4]);
            //                        Queues_without_pmo_dic.Add("95 Distribution", new object[4]);
            //                        Queues_without_pmo_dic.Add("96 IT", new object[4]);
            //                        #endregion
            //                        #region dictionary Queues_with_pmo_dic
            //                        ///<summary>
            //                        /// this array of objects  element of index [0] is the datetime that ticket forwared to the queue element of index [1]
            //                        /// is the datetiem that ticket forwared from the queue and the element of index [2] is a temproray datetime to save the date 
            //                        /// if ticket moved from the queue to the same queue element of index [3] is the date time that the ticket is forwared to the queue 
            //                        /// but the primary resource is project manager 
            //                        /// the element of index [4] is the total of days that ticket spend in the queue 
            //                        /// the element of index [5] is the total of days ticket spend in the queue with priject manager resource
            //                        /// </summary>

            //                        // fill the dictionary
            //                        Queues_with_pmo_dic.Add("70 ERP", new object[6]);
            //                        Queues_with_pmo_dic.Add("71 EDI", new object[6]);
            //                        Queues_with_pmo_dic.Add("72 CWA", new object[6]);
            //                        Queues_with_pmo_dic.Add("73 HTML5", new object[6]);
            //                        // end of fill the dictionary 
            //                        #endregion
            //                        TicketAHT AHT_for_ticket = CalculateAHT(newobject, ((Ticket)ticket), Allqueuesdic,allqueues);//Queues_without_pmo_dic, Queues_with_pmo_dic);
            //                        if (AHT_for_ticket != null)
            //                        {
            //                            TicketAHT ticket_to_create2 = newobject.FindObject<TicketAHT>(CriteriaOperator.Parse("[Oid] = '" + AHT_for_ticket.Oid + "'"));
            //                            CreateDotlist(DOT, ticket_to_create2, newobject);
            //                        }
            //                    }
            //                }
            //            }
            //        }
            //    }
            //    #endregion
            //}
            if (DOT.DotType == DeliveryOnTime.DOTType.Projects)
            {
                calculateprojectdeliveryontime(newobject,DOT.DateFrom,DOT.DateTO,DOT,ismaster);
            }
            else if (DOT.DotType == DeliveryOnTime.DOTType.Resources)
            {
                    CalculateResourcedeliveryontime(newobject, DOT.DateFrom, DOT.DateTO, DOT, DOT.Resource, ismaster);
            }
            else if (DOT.DotType == DeliveryOnTime.DOTType.Departments)
            {
                if (DOT.Department == null)
                {
                    IList<Aria5SystemAdmin.Module.BusinessObjects.Department> Departments = newobject.GetObjects<Aria5SystemAdmin.Module.BusinessObjects.Department>();
                    DeliveryOnTime ALLDOT = newobject.FindObject<DeliveryOnTime>(CriteriaOperator.Parse("[Oid] = '" + DOT.Oid + "'"));
                    CalculateResourcedeliveryontime(newobject, DOT.DateFrom, DOT.DateTO, ALLDOT,null, ismaster);                   
                    foreach (Aria5SystemAdmin.Module.BusinessObjects.Department department in Departments)
                    {
                        foreach (Resources Resource in department.Resourceses)
                        {
                            CalculateResourcedeliveryontime(newobject, DOT.DateFrom, DOT.DateTO, ALLDOT, Resource, ismaster);
                        }
                        CalculateDepartmentdeliveryontime(newobject, DOT.DateFrom, DOT.DateTO, department, ALLDOT, ismaster);
                    }
                }
                else
                {
                    foreach (Resources Resource in DOT.Department.Resourceses)
                    {
                        CalculateResourcedeliveryontime(newobject, DOT.DateFrom, DOT.DateTO, DOT, Resource, ismaster);
                    }
                    CalculateDepartmentdeliveryontime(newobject, DOT.DateFrom, DOT.DateTO, DOT.Department, DOT, ismaster);
                }
                DOT.Reload();

            }
           
        }
        /// <summary>
        /// calculate DOT for all projects depend on date range specified 
        /// </summary>
        /// <param name="objectspace"></param>
        /// <param name="datefrom"></param>
        /// <param name="dateto"></param>
        /// <param name="DOT"></param>
        /// <param name="ismaster"></param>
        public void calculateprojectdeliveryontime(IObjectSpace objectspace, DateTime datefrom, DateTime dateto, DeliveryOnTime DOT, bool ismaster)
        {
            Projects_Delivery_on_Time oneproject = DOT.ProjectsDOT.FirstOrDefault(x => x.CreateDate == datefrom);
            if (oneproject == null)
            {
                StringBuilder SelectProject = new StringBuilder();
                SelectProject.Append("<queryxml><entity>Project</entity>").Append(System.Environment.NewLine);
                SelectProject.Append("<query><condition><field>CompletedDateTime<expression op=\"IsNull\"></expression></field></condition>").Append(System.Environment.NewLine);
                SelectProject.Append("<condition operator=\"OR\"><field>CompletedDateTime<expression op=\"GreaterThanorEquals\">" + datefrom + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                SelectProject.Append("</queryxml>").Append(System.Environment.NewLine);
                var completion = clientAuto.query(at_integrations, SelectProject.ToString());
                if (completion.ReturnCode == 1)
                {
                    foreach (Project Project in completion.EntityResults)
                    {
                        IList<TasksDOT> alltasks = objectspace.GetObjects<TasksDOT>(CriteriaOperator.Parse("[ProjectNumber]= '" + Project.id + "' and [TaskCompleteDate] >= '" + datefrom + "' and [TaskCompleteDate] <= '" + dateto + "'"));
                        int otdtaskscount = objectspace.GetObjectsCount(typeof(TasksDOT), CriteriaOperator.Parse("[ProjectNumber]= '" + Project.id + "' and [TaskCompleteDate] >= '" + datefrom + "' and [TaskCompleteDate] <= '" + dateto + "'and [DeliveryOnTime] <= 0"));
                        if (alltasks.Count > 0)
                        {
                            double dot = genericcalculatdotfromtasksdottable(alltasks, DOT.Penality);
                            Projects_Delivery_on_Time newone = objectspace.CreateObject<Projects_Delivery_on_Time>();
                            newone.ProjectName = Project.ProjectName.ToString();
                            newone.Id = Project.id;
                            newone.Total = (long)dot;
                            newone.Deliveryontime = (long)Math.Round(dot / alltasks.Count, 0);
                            newone.Numberoftasks = alltasks.Count;
                            newone.CreateDate = datefrom;
                            newone.NumberofOTDTasks = otdtaskscount;
                            decimal otd = Convert.ToDecimal(Convert.ToDouble(otdtaskscount) / alltasks.Count);
                            newone.OTD = (double)Math.Round(otd * 100, 0);
                            if (ismaster)
                            {
                                newone.IsMaster = ismaster;
                            }
                            newone.Save();
                            newone.Session.CommitTransaction();
                            if (DOT != null)
                            {
                                DeliveryOnTime configration = objectspace.FindObject<DeliveryOnTime>(CriteriaOperator.Parse("[oid] = '" + DOT.Oid + "'"));
                                if (configration != null)
                                {
                                    configration.ProjectsDOT.Add(newone);
                                    configration.Save();
                                    configration.Session.CommitTransaction();
                                }
                            }
                        }

                    }
                }
                      #region xyz
                //        StringBuilder newsb = new StringBuilder();
                //        newsb.Append("<queryxml><entity>Task</entity>").Append(System.Environment.NewLine);
                //        newsb.Append("<query><condition><field>ProjectID<expression op=\"Equals\">" + Project.id + "</expression></field></condition>").Append(System.Environment.NewLine);
                //        newsb.Append("<condition><field>CompletedDateTime<expression op=\"GreaterThanorEquals\">" + datefrom + "</expression></field></condition>").Append(System.Environment.NewLine);
                //        newsb.Append("<condition><field>CompletedDateTime<expression op=\"LessThanOrEquals\">" + dateto + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                //        newsb.Append("</queryxml>").Append(System.Environment.NewLine);
                //        var Autotask_query = clientAuto.query(at_integrations, newsb.ToString());
                //        if (Autotask_query.ReturnCode == 1)
                //        {
                //            double dot = 0;
                //            bool createdot = false;
                //            if (Autotask_query.EntityResults.Length > 0)
                //            {
                //                createdot = true;
                //                IEnumerable<SubAutoTask1.Task> tasks = Autotask_query.EntityResults.ToList().Cast<SubAutoTask1.Task>();
                //                dot = genericcalculatedot(tasks, DOT.Penality);
                //                if (createdot)
                //                {
                //                    Projects_Delivery_on_Time newone = objectspace.CreateObject<Projects_Delivery_on_Time>();


                //                    newone.ProjectName = Project.ProjectName.ToString();
                //                    newone.Id = Project.id;
                //                    newone.Total = (long)dot;
                //                    newone.Deliveryontime = (long) Math.Round(dot / Autotask_query.EntityResults.Length,0);
                //                    newone.Numberoftasks = Autotask_query.EntityResults.Length;
                //                    newone.CreateDate = datefrom;
                //                    if (ismaster)
                //                    {
                //                        newone.IsMaster = ismaster;
                //                    }
                //                    newone.Save();
                //                    newone.Session.CommitTransaction();
                //                    if (DOT != null)
                //                    {
                //                        DeliveryOnTime configration = objectspace.FindObject<DeliveryOnTime>(CriteriaOperator.Parse("[oid] = '" + DOT.Oid + "'"));
                //                        if (configration != null)
                //                        {
                //                            configration.ProjectsDOT.Add(newone);
                //                            configration.Save();
                //                            configration.Session.CommitTransaction();
                //                        }
                //                    }


                //                    createdot = false;

                //                }
                //            }

                //        }
                //    }
                //}
                      #endregion
            }
        }
        /// <summary>
        /// calculate DOt for one resource for all tasks completed durrin date range specified 
        /// </summary>
        /// <param name="objectspace"></param>
        /// <param name="datefrom"></param>
        /// <param name="dateto"></param>
        /// <param name="DOT"></param>
        /// <param name="resource"></param>
        /// <param name="ismaster"></param>
        public void CalculateResourcedeliveryontime(IObjectSpace objectspace ,DateTime datefrom,DateTime dateto,DeliveryOnTime DOT,Resources resource, bool ismaster)
        {
                Resource_DOT Oneresource = DOT.ResourcesDOT.FirstOrDefault(x => x.CreateDate == datefrom && x.ResourceName == resource);
                if (Oneresource != null)
                {
                    return;
                }
                else
                {
                    IList<TasksDOT> alltasks = new List<TasksDOT>();
                    int otdtaskscount = 0;
                    if (resource == null)
                    {
                        alltasks = objectspace.GetObjects<TasksDOT>(CriteriaOperator.Parse("[ResourceId]= '" + null + "' and [TaskCompleteDate] >= '" + datefrom + "' and [TaskCompleteDate] <= '" + dateto + "'"));
                       otdtaskscount = objectspace.GetObjectsCount(typeof(TasksDOT), CriteriaOperator.Parse("[ResourceId]= '" + null + "' and [TaskCompleteDate] >= '" + datefrom + "' and [TaskCompleteDate] <= '" + dateto + "'and [DeliveryOnTime] <= 0"));
                    }
                    else
                    {
                        if (resource.AutoTaskID != null)
                        {
                            alltasks = objectspace.GetObjects<TasksDOT>(CriteriaOperator.Parse("[ResourceId]= '" + resource.AutoTaskID + "' and [TaskCompleteDate] >= '" + datefrom + "' and [TaskCompleteDate] <= '" + dateto + "'"));
                           otdtaskscount = objectspace.GetObjectsCount(typeof(TasksDOT), CriteriaOperator.Parse("[ResourceId]= '" + resource.AutoTaskID + "' and [TaskCompleteDate] >= '" + datefrom + "' and [TaskCompleteDate] <= '" + dateto + "'and [DeliveryOnTime] <= 0"));

                        }
                    }
                    if (alltasks.Count > 0)
                    {
                        Resource_DOT newrecord = objectspace.CreateObject<Resource_DOT>();

                        newrecord.Session.BeginNestedUnitOfWork();
                        if (DOT == null)
                        {
                            newrecord.DOT = Math.Round(genericcalculatdotfromtasksdottable(alltasks, 5) / alltasks.Count, 0);

                        }
                        else
                        {
                            newrecord.DOT = Math.Round(genericcalculatdotfromtasksdottable(alltasks, DOT.Penality) / alltasks.Count, 0);
                            newrecord.Total = genericcalculatdotfromtasksdottable(alltasks, DOT.Penality);
                        }
                        if (resource != null)
                        {
                            Resources Resource = objectspace.FindObject<Resources>(CriteriaOperator.Parse("[oid] = '" + resource.Oid + "'"));
                            newrecord.ResourceName = Resource;
                        }
                        newrecord.NumberOfTasks = alltasks.Count;
                        newrecord.NumberofOTDTasks = otdtaskscount;
                        decimal otd = Convert.ToDecimal(Convert.ToDouble(otdtaskscount) / alltasks.Count);
                        newrecord.OTD = (double)Math.Round( otd* 100,0);
                        newrecord.CreateDate = datefrom;
                        if (ismaster)
                        {
                            newrecord.IsMaster = ismaster;
                        }
                        newrecord.Save();
                        newrecord.Session.CommitTransaction();
                        if (DOT != null)
                        {
                            DeliveryOnTime configration = objectspace.FindObject<DeliveryOnTime>(CriteriaOperator.Parse("[oid] = '" + DOT.Oid + "'"));
                            if (configration != null)
                            {
                                configration.ResourcesDOT.Add(newrecord);
                                configration.Save();
                                configration.Session.CommitTransaction();
                            }
                        }
                    }
                    #region commented
                    //StringBuilder newsb = new StringBuilder();
                    //newsb.Append("<queryxml><entity>Task</entity>").Append(System.Environment.NewLine);
                    //newsb.Append("<query><condition><field>AssignedResourceID<expression op=\"Equals\">" + resource.AutoTaskID + "</expression></field></condition>").Append(System.Environment.NewLine);
                    //newsb.Append("<condition><field>CompletedDateTime<expression op=\"GreaterThanorEquals\">" + datefrom + "</expression></field></condition>").Append(System.Environment.NewLine);
                    //newsb.Append("<condition><field>CompletedDateTime<expression op=\"LessThanOrEquals\">" + dateto + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                    //newsb.Append("</queryxml>").Append(System.Environment.NewLine);
                    //var Autotask_query = clientAuto.query(at_integrations, newsb.ToString());
                    //if (Autotask_query.ReturnCode == 1)
                    //{
                    //    if (Autotask_query.EntityResults.Length > 0)
                    //    {
                    //        IEnumerable<SubAutoTask1.Task> tasks = Autotask_query.EntityResults.ToList().Cast<SubAutoTask1.Task>();
                    //        Resource_DOT newrecord = objectspace.CreateObject<Resource_DOT>();
                    //        Resources Resource = objectspace.FindObject<Resources>(CriteriaOperator.Parse("[oid] = '" + resource.Oid + "'"));

                    //        newrecord.Session.BeginNestedUnitOfWork();
                    //        if (DOT == null)
                    //        {
                    //            newrecord.DOT = Math.Round(genericcalculatedot(tasks, 5) / Autotask_query.EntityResults.Length, 0);

                    //        }
                    //        else
                    //        {
                    //            newrecord.DOT = Math.Round(genericcalculatedot(tasks, DOT.Penality) / Autotask_query.EntityResults.Length, 0);
                    //            newrecord.Total = genericcalculatedot(tasks, DOT.Penality);
                    //        }
                    //        newrecord.ResourceName = Resource;
                    //        newrecord.NumberOfTasks = Autotask_query.EntityResults.Length;
                    //        newrecord.CreateDate = datefrom;
                    //        if (ismaster)
                    //        {
                    //            newrecord.IsMaster = ismaster;
                    //        }
                    //        newrecord.Save();
                    //        newrecord.Session.CommitTransaction();
                    //        if (DOT != null)
                    //        {
                    //            DeliveryOnTime configration = objectspace.FindObject<DeliveryOnTime>(CriteriaOperator.Parse("[oid] = '" + DOT.Oid + "'"));
                    //            if (configration != null)
                    //            {
                    //                configration.ResourcesDOT.Add(newrecord);
                    //                configration.Save();
                    //                configration.Session.CommitTransaction();
                    //            }
                    //        }

                    //    }
                    //}
                    #endregion
                }
            
        }
       /// <summary>
        /// calculate DOt for one Department for all tasks completed based on resources DOT 
       /// </summary>
       /// <param name="objectspace"></param>
       /// <param name="datefrom"></param>
       /// <param name="dateto"></param>
       /// <param name="department"></param>
       /// <param name="DOT"></param>
       /// <param name="ismaster"></param>
        public void CalculateDepartmentdeliveryontime(IObjectSpace objectspace, DateTime datefrom, DateTime dateto, Aria5SystemAdmin.Module.BusinessObjects.Department department, DeliveryOnTime DOT, bool ismaster)
        {
              Department_DOT onedepartment = DOT.DepartmentsDOT.FirstOrDefault(x => x.CreateDate == datefrom && x.Department == department);
            if (onedepartment == null)
            {
                double deliveryontime = 0;
                int numberoftasks = 0;
                int otdtaskscount = 0;
                Aria5SystemAdmin.Module.BusinessObjects.Department Department = objectspace.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Department>(CriteriaOperator.Parse("[oid] = '" + department.Oid + "'"));

                if (department != null)
                    {
                        if (department.Resourceses.Count > 0)
                        {
                            foreach (Resources record in department.Resourceses)
                            {
                                if (record.ResourceDOT.Count > 0)
                                {
                                    foreach (Resource_DOT Item in record.ResourceDOT)
                                    {
                                        if (Item.DeliveryOnTime == DOT)
                                        {
                                            deliveryontime += (int)Item.Total;
                                            numberoftasks += Item.NumberOfTasks;
                                            otdtaskscount += Item.NumberofOTDTasks;
                                        }
                                    }

                                }
                            } 
                            Department_DOT newrecord = objectspace.CreateObject<Department_DOT>();
                            if (numberoftasks > 0)
                            {
                                
                                newrecord.DOT = (int)Math.Round(deliveryontime / numberoftasks, 0);
                                newrecord.Total = (int)deliveryontime;
                                newrecord.Department = Department;
                                newrecord.NumberOfTasks = numberoftasks;
                                newrecord.NumberofOTDTasks = otdtaskscount;
                                decimal otd = Convert.ToDecimal(Convert.ToDouble(otdtaskscount) / numberoftasks);
                                newrecord.OTD = (double)Math.Round(otd * 100, 0);
                                newrecord.CreateDate = datefrom;
                                if (ismaster)
                                {
                                    newrecord.IsMaster = ismaster;
                                }
                                newrecord.Save();
                                newrecord.Session.CommitTransaction();
                                if (DOT != null)
                                {
                                    DeliveryOnTime configration = objectspace.FindObject<DeliveryOnTime>(CriteriaOperator.Parse("[oid] = '" + DOT.Oid + "'"));
                                    if (configration != null)
                                    {
                                        configration.DepartmentsDOT.Add(newrecord);
                                        configration.Save();
                                        configration.Session.CommitTransaction();
                                    }
                                }
                            }
                           
                        }
                       
                    }

                #region systemcalc
                //else
                //{
                //    if (DOT.Department != null)
                //    {
                //        if (Department.Resourceses.Count > 0)
                //        {
                //            foreach (Resources record in DOT.Department.Resourceses)
                //            {
                //                if (record.ResourceDOT.Count > 0)
                //                {
                //                    foreach (Resource_DOT Item in record.ResourceDOT)
                //                    {
                //                        if (Item.CreateDate >= datefrom && Item.CreateDate <= dateto && Item.IsMaster == true)
                //                        {
                //                            deliveryontime += (int)Item.DOT;
                //                            numberoftasks += Item.NumberOfTasks;
                //                        }
                //                    }
                //                }
                //                else
                //                {
                //                    CalculateResourcedeliveryontime(objectspace, datefrom, dateto, DOT, record, ismaster);
                //                    if (record.ResourceDOT.Count > 0)
                //                    {
                //                        foreach (Resource_DOT Item in record.ResourceDOT)
                //                        {
                //                            if (Item.CreateDate >= datefrom && Item.CreateDate <= dateto && Item.IsMaster == true)
                //                            {
                //                                deliveryontime += (int)Item.DOT;
                //                                numberoftasks += Item.NumberOfTasks;
                //                            }
                //                        }
                //                    }
                //                }

                //            }
                //        }
                //        Department_DOT newrecord = objectspace.CreateObject<Department_DOT>();
                //        newrecord.DOT = deliveryontime;
                //        newrecord.Department = DOT.Department;
                //        newrecord.NumberOfTasks = numberoftasks;
                //        newrecord.CreateDate = datefrom;
                //        if (ismaster)
                //        {
                //            newrecord.IsMaster = ismaster;
                //        }
                //        newrecord.Save();
                //        newrecord.Session.CommitTransaction();
                //        if (DOT != null)
                //        {
                //            DeliveryOnTime configration = objectspace.FindObject<DeliveryOnTime>(CriteriaOperator.Parse("[oid] = '" + DOT.Oid + "'"));
                //            if (configration != null)
                //            {
                //                configration.DepartmentsDOT.Add(newrecord);
                //                configration.Save();
                //                configration.Session.CommitTransaction();
                //            }
                //        }
                //    }

                //}
                #endregion
            }
        }
       /// <summary>
       /// calculate DOT from auto task query direct (on fly )
       /// </summary>
       /// <param name="taskslist"></param>
       /// <param name="penality"></param>
       /// <returns></returns>
        public double genericcalculatedot(IEnumerable<SubAutoTask1.Task> taskslist , int penality)
        {
            double dot = 0;
            foreach (SubAutoTask1.Task task in taskslist)
            {
                if (task.UserDefinedFields.Length > 0)
                {
                    if (task.UserDefinedFields[4].Value == null)
                    {
                        dot += penality;
                    }
                    else
                    {
                        dot += Math.Round((DateTime.Parse(task.CompletedDateTime.ToString())).Subtract((DateTime.Parse(task.UserDefinedFields[4].Value.ToString()))).TotalDays,0);
                    }
                }
            }
            return dot;
        }
       /// <summary>
       /// calculate DOT from TASKS DOT table as a row data 
       /// </summary>
       /// <param name="tasklist"></param>
       /// <param name="penality"></param>
       /// <returns></returns>
        public double genericcalculatdotfromtasksdottable(IList<TasksDOT> tasklist, int penality)
        {
            double dot = 0;
            foreach (TasksDOT task in tasklist)
            {
                    if (task.BaselineDueDate == DateTime.MinValue)
                    {
                        dot += penality;
                    }
                    else
                    {
                        dot += task.DeliveryOnTime;
                    }
            }
            return dot;
        }
       /// <summary>
       /// Retrive all tasks completed from auto tasks during spcefic date range
       /// </summary>
       /// <param name="datefrom"></param>
       /// <param name="dateto"></param>
       /// <param name="Objectspace"></param>
        public void getDOTdata(DateTime datefrom, DateTime dateto , IObjectSpace Objectspace)
        {
            int x = 0;
            StringBuilder newsb = new StringBuilder();
            newsb.Append("<queryxml><entity>Task</entity>").Append(System.Environment.NewLine);
            newsb.Append("<query><condition><field>CompletedDateTime<expression op=\"GreaterThanorEquals\">" + datefrom + "</expression></field></condition>").Append(System.Environment.NewLine);
            newsb.Append("<condition><field>CompletedDateTime<expression op=\"LessThanOrEquals\">" + dateto + "</expression></field></condition></query>").Append(System.Environment.NewLine);
            newsb.Append("</queryxml>").Append(System.Environment.NewLine);
           var Autotask_query = clientAuto.query(at_integrations, newsb.ToString());
           if (Autotask_query.ReturnCode == 1)
           {
               if (Autotask_query.EntityResults.Length > 0)
               {
                   IList<TasksDOT> alltasks = Objectspace.GetObjects<TasksDOT>(CriteriaOperator.Parse("[TaskCompleteDate] >= '" + datefrom + "' and [TaskCompleteDate] <= '" + dateto + "'"));
                   foreach (SubAutoTask1.Task onetask in Autotask_query.EntityResults)
                   {
                       TasksDOT taskone = alltasks.FirstOrDefault(y => y.TaskId == onetask.id.ToString());
                       if (taskone != null)
                       {
                           if (onetask.UserDefinedFields.Length > 0 && onetask.UserDefinedFields[4].Value != null)
                           {
                               if (taskone.BaselineDueDate == DateTime.Parse(onetask.UserDefinedFields[4].Value))
                               {
                                   continue;
                               }
                               else
                               {
                                   taskone.BaselineDueDate = DateTime.Parse(onetask.UserDefinedFields[4].Value);
                                   taskone.DeliveryOnTime = (long)Math.Round(DateTime.Parse(onetask.CompletedDateTime.ToString()).Subtract(DateTime.Parse(onetask.UserDefinedFields[4].Value.ToString())).TotalDays, 0);
                                   taskone.ResourceId = int.Parse(onetask.AssignedResourceID.ToString());
                                   if (onetask.AssignedResourceID != null)
                                   {
                                       taskone.ResourceId = int.Parse(onetask.AssignedResourceID.ToString());
                                       StringBuilder existresourcequery = new StringBuilder();
                                       existresourcequery.Append("<queryxml><entity>Resource</entity>").Append(System.Environment.NewLine);
                                       existresourcequery.Append("<query><condition><field>id<expression op=\"Equals\">" + onetask.AssignedResourceID.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                                       existresourcequery.Append("</queryxml>").Append(System.Environment.NewLine);
                                       var Resourceresponse = clientAuto.query(at_integrations, existresourcequery.ToString());
                                       if (Resourceresponse.ReturnCode == 1)
                                       {
                                           if (Resourceresponse.EntityResults.Length > 0)
                                           {
                                               foreach (Aria5SystemAdmin.Module.SubAutoTask1.Resource oneresource in Resourceresponse.EntityResults)
                                               {

                                                   taskone.ResourceName = oneresource.FirstName.ToString() + " " + oneresource.LastName.ToString();
                                               }
                                           }
                                       }
                                   }
                                   taskone.Save();
                                   taskone.Session.CommitTransaction();
                                   continue;
                               }
                           }
                       }
                       TasksDOT completedtask = Objectspace.CreateObject<TasksDOT>();
                       completedtask.TaskTitle = onetask.Title.ToString();
                       completedtask.TaskCompleteDate = DateTime.Parse(onetask.CompletedDateTime.ToString());
                       completedtask.TaskId = onetask.id.ToString();
                       if (onetask.UserDefinedFields.Length > 0 && onetask.UserDefinedFields[4].Value != null)
                       {
                           completedtask.BaselineDueDate = DateTime.Parse(onetask.UserDefinedFields[4].Value.ToString());
                           completedtask.DeliveryOnTime = (long)Math.Round(DateTime.Parse(onetask.CompletedDateTime.ToString()).Subtract(DateTime.Parse(onetask.UserDefinedFields[4].Value.ToString())).TotalDays, 0);
                       }
                       StringBuilder Projectquery = new StringBuilder();
                       Projectquery.Append("<queryxml><entity>Project</entity>").Append(System.Environment.NewLine);
                       Projectquery.Append("<query><condition><field>id<expression op=\"Equals\">" + onetask.ProjectID.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                       Projectquery.Append("</queryxml>").Append(System.Environment.NewLine);
                       var Projectresponse = clientAuto.query(at_integrations, Projectquery.ToString());
                       if (Projectresponse.ReturnCode == 1)
                       {
                           if (Projectresponse.EntityResults.Length > 0)
                           {
                               foreach (Project oneproject in Projectresponse.EntityResults)
                               {
                                   completedtask.ProjectName = oneproject.ProjectName.ToString();
                                   completedtask.ProjectNumber = int.Parse(oneproject.id.ToString());
                                   if (oneproject.CompletedDateTime != null)
                                   {
                                       completedtask.ProjectCompleteDate = DateTime.Parse(oneproject.CompletedDateTime.ToString());
                                   }
                               }
                           }
                       }
                       if (onetask.AssignedResourceID != null)
                       {
                           StringBuilder resourcequery = new StringBuilder();
                           resourcequery.Append("<queryxml><entity>Resource</entity>").Append(System.Environment.NewLine);
                           resourcequery.Append("<query><condition><field>id<expression op=\"Equals\">" + onetask.AssignedResourceID.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                           resourcequery.Append("</queryxml>").Append(System.Environment.NewLine);
                           var Resourceresponse = clientAuto.query(at_integrations, resourcequery.ToString());
                           if (Resourceresponse.ReturnCode == 1)
                           {
                               if (Resourceresponse.EntityResults.Length > 0)
                               {
                                   foreach (SubAutoTask1.Resource oneresource in Resourceresponse.EntityResults)
                                   {
                                       completedtask.ResourceId = int.Parse(oneresource.id.ToString());
                                       completedtask.ResourceName = oneresource.FirstName.ToString() + " " + oneresource.LastName.ToString();
                                       Resources taskresource = Objectspace.FindObject<Resources>(CriteriaOperator.Parse("[AutoTaskID] = '" + oneresource.id.ToString() + "'"));
                                       if (taskresource != null)
                                       {
                                           completedtask.DepName = taskresource.Department.Id;
                                       }
                                       //StringBuilder Resourcerolequery = new StringBuilder();
                                       //Resourcerolequery.Append("<queryxml><entity>ResourceRole</entity>").Append(System.Environment.NewLine);
                                       //Resourcerolequery.Append("<query><condition><field>id<expression op=\"Equals\">" + onetask.AssignedResourceRoleID.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                                       //Resourcerolequery.Append("</queryxml>").Append(System.Environment.NewLine);
                                       //var Resourceroleresponse = clientAuto.query(at_integrations, Resourcerolequery.ToString());
                                       //if (Resourceroleresponse.ReturnCode == 1)
                                       //{
                                       //    if (Resourceroleresponse.EntityResults.Length > 0)
                                       //    {
                                       //        foreach (SubAutoTask1.ResourceRole onerole in Resourceroleresponse.EntityResults)
                                       //        {
                                       //            StringBuilder Departmentquery = new StringBuilder();
                                       //            Departmentquery.Append("<queryxml><entity>Department</entity>").Append(System.Environment.NewLine);
                                       //            Departmentquery.Append("<query><condition><field>id<expression op=\"Equals\">" + onerole.DepartmentID.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                                       //            Departmentquery.Append("</queryxml>").Append(System.Environment.NewLine);
                                       //            var Departmentresponse = clientAuto.query(at_integrations, Departmentquery.ToString());
                                       //            if (Departmentresponse.ReturnCode == 1)
                                       //            {
                                       //                foreach (SubAutoTask1.Department onedepartment in Departmentresponse.EntityResults)
                                       //                {
                                       //                    completedtask.DepName = onedepartment.Name.ToString();
                                       //                }
                                       //            }
                                       //        }
                                       //    }
                                       //}
                                   }
                               }

                           }
                       }
                       completedtask.Save();
                       x++;
                       completedtask.Session.CommitTransaction();
                   }
                   
               }
           }
        }
        /// <summary>
        /// add tickets DOt under Delivery on time tickets list view 
        /// </summary>
        /// <param name="dOT"></param>
        /// <param name="newticket"></param>
        /// <param name="object1"></param>
        //public void CreateDotlist(DeliveryOnTime dOT, TicketAHT newticket, IObjectSpace object1)
        //{
        //    string[] array = new string[5];
        //    if (dOT.ERP == true)
        //    {
        //        array[0] = "ERP";
        //    }
        //    if (dOT.CRM == true)
        //    {
        //        array[1] = "CWA";
        //    }
        //    if (dOT.EDI == true)
        //    {
        //        array[2] = "EDI";
        //    }
        //    if (dOT.SD == true)
        //    {
        //        array[3] = "SD";
        //    }
        //    if (dOT.IT == true)
        //    {
        //        array[4] = "IT";
        //    }
        //    if (array.Contains(newticket.UDF.ToString()))
        //    {
        //        TicketAHT ticket_to_creat = dOT.Session.FindObject<TicketAHT>(CriteriaOperator.Parse("[Oid] = '" + newticket.Oid + "'"));
        //        if (dOT.TicketsforDot.Count > 0)
        //        {
        //            if (!dOT.TicketsforDot.Contains(ticket_to_creat))
        //            {
        //                dOT.TicketsforDot.Add(ticket_to_creat);
        //                dOT.Save();
        //                dOT.Session.CommitTransaction();
        //            }
        //        }
        //        else
        //        {
        //            dOT.TicketsforDot.Add(ticket_to_creat);
        //            dOT.Save();
        //            dOT.Session.CommitTransaction();
        //        }
             

        //    }
        //}
        /// <summary>
        /// Calculate AHT for each queue name in the system for specific period 
        /// </summary>
        /// <param name="AHTList"></param>
        /// <param name="objectspace"></param>
        public void calculateQueueAHT(AverageHandleTime AHT,IObjectSpace objectspace)
        {
            IList<QueueName> allqueues = objectspace.GetObjects<QueueName>();
            AverageHandleTime Averagehandlingtime = objectspace.FindObject<AverageHandleTime>(CriteriaOperator.Parse("[Oid] = '" + AHT.Oid + "'"));
            Dictionary<QueueName, int[]> queuedic = new Dictionary<QueueName,int[]>();
            Averagehandlingtime.Session.LockingOption = LockingOption.None;
            foreach (QueueName queue in allqueues)
            {
                queuedic.Add(queue, new int[2]);
            }

            foreach (TicketAHT ticket in Averagehandlingtime.Tickets)
            {
                foreach (QueueValues queuevalu in ticket.QueuesValues)
                {
                    queuedic[queuevalu.Type][0] += (int)queuevalu.Value;
                    queuedic[queuevalu.Type][1]++;
                }
            }
            foreach (QueueName key in queuedic.Keys)
            {
                QueueValues queuvalue = objectspace.CreateObject<QueueValues>();
                if (queuedic[key][0] > 0)
                {
                    queuvalue.Type = key;
                    queuvalue.Value = Convert.ToDouble(queuedic[key][0]);
                    queuvalue.NumberOfTickets = queuedic[key][1];
                    queuvalue.Average = Math.Round(queuvalue.Value / queuvalue.NumberOfTickets,0);
                    queuvalue.AverageHandleTime = Averagehandlingtime;
                    queuvalue.Save();
                    queuvalue.Session.CommitTransaction();
                }

            }
            objectspace.CommitChanges();
        }
        /// <summary>
        /// Calculate AHT for each department 
        /// </summary>
        /// <param name="datefrom"></param>
        /// <param name="dateto"></param>
        /// <param name="objectspace"></param>
        public void calculateDepartmentAHT(AverageHandleTime Averagehandlingtime, IObjectSpace objectspace)
        {
            IList<QueueName> allqueues = objectspace.GetObjects<QueueName>();
           // IList<TicketAHT> ticketslist = objectspace.GetObjects<TicketAHT>(CriteriaOperator.Parse("[comletedate] >= '" + datefrom + "'and [comletedate] <= '" + dateto + "'"));
            IList<Aria5SystemAdmin.Module.BusinessObjects.Department> departments = objectspace.GetObjects<Aria5SystemAdmin.Module.BusinessObjects.Department>();
            AverageHandleTime configration = objectspace.FindObject<AverageHandleTime>(CriteriaOperator.Parse("[Oid] = '" + Averagehandlingtime.Oid + "'"));
           // Dictionary<QueueName, int> queuedic = new Dictionary<QueueName, int>();
            foreach (Aria5SystemAdmin.Module.BusinessObjects.Department Dep in departments)
            {
                int queuevalues = 0;
                int queuetickets = 0;
                foreach (QueueValues value in configration.QueuesValues.Where(x => x.Type.Department == Dep))
                {
                    queuevalues += (int)value.Value;
                    queuetickets += value.NumberOfTickets;
                }
                if (queuetickets > 0)
                {
                    Aria5SystemAdmin.Module.BusinessObjects.DepartmentAHT depAHT = objectspace.CreateObject<Aria5SystemAdmin.Module.BusinessObjects.DepartmentAHT>();
                    depAHT.Department = Dep;
                    depAHT.Value = (int)Math.Round((double)queuevalues / queuetickets, 0);
                    depAHT.AverageHandleTime = configration;
                    depAHT.Save();
                    depAHT.Session.CommitTransaction();
                }
            }

        }
        #endregion
        #region TaskNotes
        public void gettasknote(TrackingTask task ,IObjectSpace ObjectSpace)
        {
            StringBuilder getNote = new StringBuilder();
            getNote.Append("<queryxml><entity>TaskNote</entity>").Append(System.Environment.NewLine);
            //ATA add oneresource instead of this
            getNote.Append("<query><condition><field>TaskID<expression op=\"Equals\">" + task.AutotaskID + "</expression></field></condition></query>").Append(System.Environment.NewLine);
            getNote.Append("</queryxml>").Append(System.Environment.NewLine);

            var Notes = clientAuto.query(at_integrations, getNote.ToString());
            if (Notes.ReturnCode == 1)
            {
                if (Notes.EntityResults.Length > 0)
                {
                    foreach (Aria5SystemAdmin.Module.SubAutoTask1.TaskNote onenote in Notes.EntityResults)
                    {
                       TrackingTaskNote excistnote = ObjectSpace.FindObject<TrackingTaskNote>(CriteriaOperator.Parse("[AutoTaskID] = '"+onenote.id+"'"));
                        if(excistnote == null)
                        {
                         TrackingTaskNote SystemNote = ObjectSpace.CreateObject<TrackingTaskNote>();
                        SystemNote.Title = onenote.Title.ToString();
                        SystemNote.AutoTaskID = onenote.id.ToString();
                        SystemNote.Describtion = onenote.Description.ToString();
                        Resources Notecreatorresource = ObjectSpace.FindObject<Resources>(CriteriaOperator.Parse("[AutoTaskID] ='"+onenote.CreatorResourceID+"' "));
                        if (Notecreatorresource != null)
                        {
                            SystemNote.Resource = Notecreatorresource;
                        }
                        SystemNote.CreateDate = DateTime.Parse(onenote.LastActivityDate.ToString());
                        SystemNote.Task = task;
                        SystemNote.Save();
                        SystemNote.Session.CommitTransaction();
                        }
                       
                    }
                }
            }
        }
        #endregion
    }
}
