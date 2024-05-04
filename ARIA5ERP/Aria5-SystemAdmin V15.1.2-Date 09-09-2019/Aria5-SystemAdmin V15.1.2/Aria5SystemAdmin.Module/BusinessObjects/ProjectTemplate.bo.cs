using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Text;
using Aria5SystemAdmin.Module.SubAutoTask1;
using System.Data.SqlClient;
using System.ServiceModel;
using DevExpress.ExpressApp.Security;
using Aria5SystemAdmin.Module.Managers;
using System.Net;
using DevExpress.ExpressApp.ConditionalAppearance;
using DevExpress.ExpressApp.Editors;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    [Appearance("NotKeyDisablingIteration", AppearanceItemType = "ViewItem", Context = "DetailView", TargetItems = "IterationNumber", Criteria = "Type !='Key'", Enabled = false)]
    [Appearance("NotKeyDisablingStartFrom", AppearanceItemType = "ViewItem", Context = "DetailView", TargetItems = "StartFrom", Criteria = "Type !='Key'", Enabled = false)]
    [Appearance("NotKeyDisablingUseCase", AppearanceItemType = "ViewItem", Context = "DetailView", TargetItems = "UseCasePoints", Criteria = "Type !='Key'", Enabled = false)]

    //// Doaa 05/05/2019 start
    [Appearance("PhaseM0", AppearanceItemType = "ViewItem", TargetItems = "PhaseM0", Criteria = "Type!='Key'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
    [Appearance("PhaseM1", AppearanceItemType = "ViewItem", TargetItems = "PhaseM1", Criteria = "Type!='Key'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
    [Appearance("PhaseM2", AppearanceItemType = "ViewItem", TargetItems = "PhaseM2", Criteria = "Type!='Key'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
    [Appearance("PhaseM3", AppearanceItemType = "ViewItem", TargetItems = "PhaseM3", Criteria = "Type!='Key'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]

    [Appearance("PhaseM0_statues", AppearanceItemType = "ViewItem", TargetItems = "PhaseM0_statues", Criteria = "Type!='Key'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
    [Appearance("PhaseM1_statues", AppearanceItemType = "ViewItem", TargetItems = "PhaseM1_statues", Criteria = "Type!='Key'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
    [Appearance("PhaseM2_statues", AppearanceItemType = "ViewItem", TargetItems = "PhaseM2_statues", Criteria = "Type!='Key'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
    [Appearance("PhaseM3_statues", AppearanceItemType = "ViewItem", TargetItems = "PhaseM3_statues", Criteria = "Type!='Key'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]

    [Appearance("PhaseM0_SPI", AppearanceItemType = "ViewItem", TargetItems = "PhaseM0_SPI", Criteria = "Type!='Key'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
    [Appearance("PhaseM1_SPI", AppearanceItemType = "ViewItem", TargetItems = "PhaseM1_SPI", Criteria = "Type!='Key'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
    [Appearance("PhaseM2_SPI", AppearanceItemType = "ViewItem", TargetItems = "PhaseM2_SPI", Criteria = "Type!='Key'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
    [Appearance("PhaseM3_SPI", AppearanceItemType = "ViewItem", TargetItems = "PhaseM3_SPI", Criteria = "Type!='Key'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]

    //// End

    [RelatedEntity("Aria5-SystemAdmin-Project")]
    public partial class ProjectTemplate
    {

        protected override void OnSaving() 
        {
           
            //ATA Apply notification module on the project component baseline 17/7/2016[Start]
            if (this.Session.IsNewObject(this))
            {
                
                if (this.Type == ProjectType.Key )//&& Name != (this.Application.Name + "-Iteration #" + this.EndDate.AddDays(-7).Year + "-" + IterationNumber + " - " + Name))
                {
                    this.Name = this.Application.Name + "-Iteration #" + this.EndDate.AddDays(-7).Year + "-" + IterationNumber + " - " + Name;
                }
                else if (this.Type == ProjectType.Standard)//&& Name != (this.Account.Name + "-" + this.Application.Name + " Maintenance #" + IterationNumber))
                {

                    this.Name = this.Account.Name + "-" + this.Application.Name + " Maintenance #" + this.StartDate.Year + " - " + Name;
                }
                else
                {
                    this.Name = this.Account + "-" + this.Application + this.Name;
                }
                if (this.Type == ProjectType.Key)
                {
                    alarmTime = CalcEndate.CalcEnddate(StartDate, 2).Subtract(new TimeSpan(10, 0, 0, 0));
                    _notificationmessage = string.Format("We need to check the blew missing items for this project '{0}' Requirement Use cases project Entities Use Case Points Test Cases before this date {1} ", this.Name, CalcEndate.CalcEnddate(StartDate, 2).Subtract(new TimeSpan(10, 0, 0, 0)));
                    //ATA add the template for the project 
                    if ((int)StartFrom > -1 )
                    {
                        QAUseCasePoints ucp = this.Session.FindObject<QAUseCasePoints>(CriteriaOperator.Parse("[Title] = 'Key Template'"));
                        for (int i = (int)StartFrom; i < 4; i++)
                        {
                            QAWBS wbsphase;
                            int wbs = -1;
                            if (ucp != null)
                            {
                                wbs = ucp.WBS.FindIndex(x => x.Month == i);
                            }
                            if (wbs > -1)
                            {
                                 wbsphase = new QAWBS(this.Session, ucp.WBS[wbs]);
                            }
                            else
                            {
                                 wbsphase = new QAWBS(this.Session);
                                 wbsphase.Month = i;
                            }
                           
                            this.ProjectWBS.Add(wbsphase);
                        }
                    }
                }
                else
                {
                    if(this.Type==ProjectType.Standard || this.Type == ProjectType.Custom)
                    {
                        QAWBS wbsphase;
                        wbsphase = new QAWBS(this.Session);
                        wbsphase.Month = (int)(StartFrom);
                        this.ProjectWBS.Add(wbsphase);

                    }
                }
            }
            else
            {
                NotificationManager NotificationManager = new NotificationManager();
                NotificationManager.ProjectTemplateNotification(this);
            }
            
            if(this.Type != ProjectType.Key && this.IterationNumber == null)
            {
                this.IterationNumber = 1;
            }
            //ATA Apply notification module on the project component baseline 17/7/2016[End]
            // base.OnSaving();
            base.Save();
           // Create_and_UpdateProject();
        }
       
        protected override void OnDeleting()
        {  
            if (Session.CollectReferencingObjects(this).Count > 0)
            {
                throw new Exception("Can not Delete Project! ... because there are items related to this project ");// DevExpress.XtraEditors.XtraMessageBox.Show("Can not Delete Project!");
            }
            else
            {
                base.OnDeleting();
            }
        }

        //Doaa 05/01/2019 start
        public override void AfterConstruction()
        {
            // base.AfterConstruction();
            if (this.Status == null)
                this.Status = status.New;

            if (this.IterationNumber == null)
            {
                this.IterationNumber = 1;
            }
        }
        //Doaa 05/01/2019 end
        // [DevExpress.Persistent.Base.Action]
        public void Create_and_UpdateProject()
        {
            if (this.AutoTaskID == null || this.AutoTaskID == 0)
            {
                bool returnVal = CreateAutotaskProject(this.Oid.ToString(), this.Name, this.StartDate, this.EndDate);
            }
            else
            {
                bool returnVal = UpdateAutoTaskProject(this.Oid.ToString(), this.AutoTaskID.ToString());
            }
        }


        private ATWSSoapClient clientAuto;
        private AutotaskIntegrations at_integrations;
        SqlConnection connection;
        SqlCommand command;
        SqlDataReader reader;
        private string auth_user_id = "IT@ariasystems.biz"; // user@domain.com
        private string auth_user_password = "Aria@2016";
        private ATWSZoneInfo zoneInfo = null;

        private BasicHttpBinding myBinding;
        public bool CreateAutotaskProject(string oId, string projectName, DateTime startDate, DateTime endDate)
        {
            connection = new SqlConnection();
            //  connection.ConnectionString = @"Data Source=NSDE_Khaled;Initial Catalog=Aria5SystemAdmin;User ID=sa;Password=aria_123";
            connection.ConnectionString = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin_Test;User ID=sa;Password=aria_123";
            // connection.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123";

            connection.Open();
            command = new SqlCommand();
            command.Connection = connection;
            myBinding = new BasicHttpBinding();
            myBinding.Security.Mode = BasicHttpSecurityMode.Transport;
            myBinding.Security.Transport.ClientCredentialType = HttpClientCredentialType.Basic;
            myBinding.MaxReceivedMessageSize = 2147483647;
            System.Net.ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12;
            //  clientAuto = new ATWSSoapClient();
            SubAutoTask1.ATWSSoapClient clientAuto = new SubAutoTask1.ATWSSoapClient();
            zoneInfo = clientAuto.getZoneInfo(auth_user_id);
            //EndpointAddress ea = new EndpointAddress(new Uri("https://webservices16.autotask.net/atservices/1.5/atws.wsdl"));
            EndpointAddress ea = new EndpointAddress(zoneInfo.URL);
            clientAuto = new ATWSSoapClient(myBinding, ea);
            clientAuto.ClientCredentials.UserName.UserName = auth_user_id;
            clientAuto.ClientCredentials.UserName.Password = auth_user_password;


            AutotaskIntegrations at_integrations = new AutotaskIntegrations();
            StringBuilder sb = new StringBuilder();
            sb.Append("<queryxml><entity>ResourceRole</entity>").Append(System.Environment.NewLine);
            sb.Append("<query><field>ResourceID<expression op=\"Equals\">29834747</expression></field></query>").Append(System.Environment.NewLine);
            sb.Append("</queryxml>").Append(System.Environment.NewLine);
            var r2p = clientAuto.query(at_integrations, sb.ToString());
            if (r2p.ReturnCode == 1)
            { }



            // readerAct = new SqlDataReader();
            bool retValue = false;
            Project newProject = new Project();
            newProject.AccountID = 18894566;//Aria In house
            newProject.StartDateTime = startDate;
            newProject.EndDateTime = endDate;
            newProject.ProjectName = projectName;
            newProject.ProjectLeadResourceID = this.ProjectOwner.AutoTaskID;//PMO
                                                        //newProject.ProjectLeadResourceID = 19418857;//Mariam
            newProject.Status = 1;
            newProject.Type = 4;
            ATWSResponse ResponseAuto2;
            SubAutoTask1.Entity[] entArr2 = new SubAutoTask1.Entity[1];
            entArr2[0] = newProject;
            ResponseAuto2 = clientAuto.create(at_integrations, entArr2);
            if (ResponseAuto2.ReturnCode == 1) //Project created
            {
                command.CommandText = "Update [ProjectTemplate] set AutoTaskID = " + ResponseAuto2.EntityReturnInfoResults[0].EntityId.ToString() + " Where  [Oid] = '" + oId.ToString() + "'";
                try
                {
                    reader.Close();
                }
                catch { }
                command.ExecuteNonQuery();
                this.AutoTaskID = int.Parse(ResponseAuto2.EntityReturnInfoResults[0].EntityId.ToString());
                //ResponseAuto2.EntityReturnInfoResults[0].EntityId.ToString() project id in Autotask
                command.CommandText = "Select Oid,month from QAWBS where qawbs.UseCasePoints in (Select QAUseCasePoints.Oid from QAUseCasePoints where Project = '" + oId + "' )";
                try
                {
                    reader.Close();
                }
                catch { }
                reader = command.ExecuteReader();
                Dictionary<string, string> phases = new Dictionary<string, string>();
                while (reader.Read())
                {
                    phases.Add(reader.GetValue(1).ToString(), reader.GetValue(0).ToString());

                }
                reader.Close();
                foreach (var item in phases)
                {
                    string phaseName = "Phase M" + item.Key;
                    Phase newPhase = new Phase();
                    newPhase.Title = phaseName;
                    newPhase.ProjectID = ResponseAuto2.EntityReturnInfoResults[0].EntityId;
                    newPhase.CreateDate = DateTime.Now.Date;
                    newPhase.Description = phaseName;
                    newPhase.StartDate = DateTime.Now.Date;
                    newPhase.DueDate = DateTime.Now.Date;

                    ATWSResponse ResponseAuto;
                    SubAutoTask1.Entity[] entArr = new SubAutoTask1.Entity[1];
                    entArr[0] = (SubAutoTask1.Entity)newPhase;
                    ResponseAuto = clientAuto.create(at_integrations, entArr);
                    if (ResponseAuto.ReturnCode == 1)
                    {
                        command.CommandText = "Update QAWBS set AutoTaskID = " + ResponseAuto.EntityReturnInfoResults[0].EntityId.ToString() + " Where Oid ='" + item.Value + "'";
                        try
                        {
                            reader.Close();
                        }
                        catch { }
                        command.ExecuteNonQuery();

                        SqlCommand command2 = new SqlCommand();
                        command2.CommandText = "Select [Activity],[Department],[Resource],[AvgEstVal] from [QAActivity] Where [QAWBS]='" + item.Value + "'";
                        command2.Connection = connection;

                        SqlDataReader readerAct = command2.ExecuteReader();
                        List<Task> tasks = new List<Task>();

                        while (readerAct.Read())
                        {
                            Task tsk = new Task();
                            tsk.Title = readerAct.GetValue(0).ToString();
                            tsk.AllocationCodeID = 30043275;
                            tsk.PhaseID = ResponseAuto.EntityReturnInfoResults[0].EntityId.ToString();
                            tsk.ProjectID = ResponseAuto2.EntityReturnInfoResults[0].EntityId.ToString();
                            tsk.Status = 1;
                            tsk.EstimatedHours = readerAct.GetValue(3);
                            tsk.TaskType = 1;
                            tsk.DepartmentID = 30042970;
                            //tsk.AssignedResourceRoleID = 30207555; //18337452;
                            //tsk.AssignedResourceID = 19418857;//readerAct.GetValue(2).ToString();
                            tsk.AssignedResourceID = 29834747;
                            tsk.AssignedResourceRoleID = 18337452;
                            tasks.Add(tsk);
                        }
                        try
                        {
                            readerAct.Close();
                        }

                        catch { }

                        SubAutoTask1.Entity[] entArr1 = tasks.ToArray();
                        ATWSResponse ResponseAuto1 = clientAuto.create(at_integrations, entArr1);
                        if (ResponseAuto1.ReturnCode == 1)
                        {
                            foreach (var t in ResponseAuto1.EntityResults)
                            {
                                Task ts = (Task)t;
                                command.CommandText = "Update [QAActivity] set AutoTaskID = " + ts.id.ToString() + " Where   [Activity] = '" + ts.Title + "' and [QAWBS]='" + item.Value + "'";
                                try
                                {
                                    reader.Close();
                                }
                                catch { }
                                command.ExecuteNonQuery();
                            }
                        }

                        //ResponseAuto.EntityReturnInfoResults[0].EntityId Phase ID

                    }

                }

                retValue = true;
            }
            else
            {
                retValue = false;
            }
            return retValue;
        }
        public bool UpdateAutoTaskProject(string oId, string ATProjectId)
        {
            connection = new SqlConnection();
            connection.ConnectionString = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin_Test;User ID=sa;Password=aria_123";
            // connection.ConnectionString = @"Data Source=NSDE_KHALED;Initial Catalog=Aria5SystemAdmin;User ID=sa;Password=aria_123";

            // connection.ConnectionString = this.Session.ConnectionString;

            connection.Open();
            command = new SqlCommand();
            command.Connection = connection;
            myBinding = new BasicHttpBinding();
            myBinding.Security.Mode = BasicHttpSecurityMode.Transport;
            myBinding.Security.Transport.ClientCredentialType = HttpClientCredentialType.Basic;
            myBinding.MaxReceivedMessageSize = 2147483647;
            System.Net.ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12;
            //  clientAuto = new ATWSSoapClient();
            SubAutoTask1.ATWSSoapClient clientAuto = new SubAutoTask1.ATWSSoapClient();
            zoneInfo = clientAuto.getZoneInfo(auth_user_id);
            //EndpointAddress ea = new EndpointAddress(new Uri("https://webservices16.autotask.net/atservices/1.5/atws.wsdl"));
            EndpointAddress ea = new EndpointAddress(zoneInfo.URL);
            clientAuto = new ATWSSoapClient(myBinding, ea);
            clientAuto.ClientCredentials.UserName.UserName = auth_user_id;
            clientAuto.ClientCredentials.UserName.Password = auth_user_password;


            AutotaskIntegrations at_integrations = new AutotaskIntegrations();
            bool returnVal = true;
            //ATA [start]
            StringBuilder SelectProject = new StringBuilder();
            SelectProject.Append("<queryxml><entity>Project</entity>").Append(System.Environment.NewLine);
            SelectProject.Append("<query><field>id<expression op=\"Equals\">" + ATProjectId + "</expression></field></query>").Append(System.Environment.NewLine);
            SelectProject.Append("</queryxml>").Append(System.Environment.NewLine);
            var completion = clientAuto.query(at_integrations, SelectProject.ToString());
            if (completion.ReturnCode == 1)
            {
                foreach (var Project1 in completion.EntityResults)
                {
                    this.Completness = int.Parse(((Project)Project1).CompletedPercentage.ToString());
                }
            }
            Dictionary<string, Int64> Autotaskphases = new Dictionary<string, Int64>();
            StringBuilder sb = new StringBuilder();
            sb.Append("<queryxml><entity>Phase</entity>").Append(System.Environment.NewLine);
            sb.Append("<query><field>ProjectID<expression op=\"Equals\">" + ATProjectId + "</expression></field></query>").Append(System.Environment.NewLine);
            sb.Append("</queryxml>").Append(System.Environment.NewLine);
            var Autotask_response = clientAuto.query(at_integrations, sb.ToString());
            if (Autotask_response.ReturnCode == 1)
            {
                foreach (var PhaseResponse in Autotask_response.EntityResults)
                {
                    Autotaskphases.Add(((Phase)PhaseResponse).Title.ToString(), ((Phase)PhaseResponse).id);
                }
            }
            //ATA [END]

            command.CommandText = "Select AutoTaskID,Oid,month from QAWBS where qawbs.UseCasePoints in (Select QAUseCasePoints.Oid from QAUseCasePoints where Project = '" + oId + "' )";
            try
            {
                reader.Close();
            }
            catch { }
            reader = command.ExecuteReader();
            Dictionary<string, string> phasesIDS = new Dictionary<string, string>();
            Dictionary<string, string> Phasestitles = new Dictionary<string, string>();
            while (reader.Read())
            {
                phasesIDS.Add(reader.GetValue(1).ToString(), reader.GetValue(0).ToString());
                //ATA[start]
                Phasestitles.Add(reader.GetValue(1).ToString(), "Phase M" + reader.GetValue(2).ToString());
                //ATA[end]
            }
            reader.Close();
            //ATA[start]
            if (Autotaskphases.Count > 0)
            {
                foreach (var ATphase in Autotaskphases)
                {
                    bool flag = false;
                    foreach (var SAphase in phasesIDS)
                    {
                        if (ATphase.Value.ToString() == SAphase.Value)
                        {
                            flag = true;
                            break;
                        }
                    }
                    if (!flag)
                    {
                        foreach (var tittle in Phasestitles)
                        {
                            if (ATphase.Key == tittle.Value)
                            {
                                command.CommandText = "Update [QAWBS] set AutoTaskID = " + ATphase.Value.ToString() + " Where [Oid]='" + tittle.Key + "'";
                                command.ExecuteNonQuery();
                                continue;
                            }
                        }
                    }
                }
            }
            else
            {
                command.CommandText = "Select Oid,month from QAWBS where qawbs.UseCasePoints in (Select QAUseCasePoints.Oid from QAUseCasePoints where Project = '" + oId + "' )";
                try
                {
                    reader.Close();
                }
                catch { }
                reader = command.ExecuteReader();
                Dictionary<string, string> phases1 = new Dictionary<string, string>();
                while (reader.Read())
                {
                    phases1.Add(reader.GetValue(1).ToString(), reader.GetValue(0).ToString());

                }
                reader.Close();
                foreach (var item in phases1)
                {
                    string phaseName = "Phase M" + item.Key;
                    Phase newPhase = new Phase();
                    newPhase.Title = phaseName;
                    newPhase.ProjectID = ATProjectId;
                    newPhase.CreateDate = DateTime.Now.Date;
                    newPhase.Description = phaseName;
                    newPhase.StartDate = DateTime.Now.Date;
                    newPhase.DueDate = DateTime.Now.Date;
                    ATWSResponse ResponseAuto;
                    SubAutoTask1.Entity[] entArr = new SubAutoTask1.Entity[1];
                    entArr[0] = (SubAutoTask1.Entity)newPhase;
                    ResponseAuto = clientAuto.create(at_integrations, entArr);
                    if (ResponseAuto.ReturnCode == 1)
                    {
                        command.CommandText = "Update QAWBS set AutoTaskID = " + ResponseAuto.EntityReturnInfoResults[0].EntityId.ToString() + " Where Oid ='" + item.Value + "'";
                        try
                        {
                            reader.Close();
                        }
                        catch { }
                        command.ExecuteNonQuery();

                        SqlCommand command2 = new SqlCommand();
                        command2.CommandText = "Select [Activity],[Department],[Resource],[AvgEstVal] from [QAActivity] Where [QAWBS]='" + item.Value + "'";
                        command2.Connection = connection;

                        SqlDataReader readerAct = command2.ExecuteReader();
                        List<Task> tasks = new List<Task>();

                        while (readerAct.Read())
                        {
                            Task tsk = new Task();
                            tsk.Title = readerAct.GetValue(0).ToString();
                            tsk.AllocationCodeID = 30043275;
                            tsk.PhaseID = ResponseAuto.EntityReturnInfoResults[0].EntityId.ToString();
                            tsk.ProjectID = ATProjectId;
                            tsk.Status = 1;
                            tsk.EstimatedHours = readerAct.GetValue(3);
                            //tsk.Task = 1;
                            tsk.DepartmentID = 30042970;
                            //tsk.AssignedResourceRoleID = 30207555; //18337452;
                            //tsk.AssignedResourceID = 19418857;//readerAct.GetValue(2).ToString();
                            tsk.AssignedResourceID = 29834747;
                            tsk.AssignedResourceRoleID = 18337452;
                            tasks.Add(tsk);
                        }
                        try
                        {
                            readerAct.Close();
                        }

                        catch { }

                        SubAutoTask1.Entity[] entArr1 = tasks.ToArray();
                        ATWSResponse ResponseAuto1 = clientAuto.create(at_integrations, entArr1);
                        if (ResponseAuto1.ReturnCode == 1)
                        {
                            foreach (var t in ResponseAuto1.EntityResults)
                            {
                                Task ts = (Task)t;
                                command.CommandText = "Update [QAActivity] set AutoTaskID = " + ts.id.ToString() + " Where   [Activity] = '" + ts.Title + "' and [QAWBS]='" + item.Value + "'";
                                try
                                {
                                    reader.Close();
                                }
                                catch { }
                                command.ExecuteNonQuery();
                            }
                        }

                        //ResponseAuto.EntityReturnInfoResults[0].EntityId Phase ID

                    }

                }
            }
            command.CommandText = "Select AutoTaskID,Oid,month from QAWBS where qawbs.UseCasePoints in (Select QAUseCasePoints.Oid from QAUseCasePoints where Project = '" + oId + "' )";
            try
            {
                reader.Close();
            }
            catch { }
            reader = command.ExecuteReader();
            Dictionary<string, string> System_phases = new Dictionary<string, string>();
            while (reader.Read())
            {
                System_phases.Add(reader.GetValue(1).ToString(), reader.GetValue(0).ToString());
            }
            //ATA[END]
            foreach (var item in System_phases)
            {
                //ATA
                //var tottal = 0;
                //ATA 
                Dictionary<string, Int64> Autotask_tasks = new Dictionary<string, Int64>();
                Dictionary<Int64, string> Autotask_tasks_statues = new Dictionary<Int64, string>();
                #region "Delete Task Commented Code"
                //ATA [start] deleted task trace 
                //  SqlCommand selectdeleted = new SqlCommand();
                //  selectdeleted.CommandText = "Select [AutoTaskID],[GCRecord]from [TrackingTask] where [WBSMonth] = '" + item.Key + "' and ( [GCRecord] is not NULL or [TrackingEntry] is null) and [AutoTaskID] is not NULL";
                //   selectdeleted.Connection = connection;
                //try
                //{
                //    reader.Close();
                //}
                //catch { }
                //reader = selectdeleted.ExecuteReader();
                //Dictionary<string, string> Deleted_task = new Dictionary<string, string>();
                //while (reader.Read())
                //{
                //    Deleted_task.Add(reader.GetValue(0).ToString(), reader.GetValue(1).ToString());
                //}
                //ATA [END]
                //qawbs.UseCasePoints in (Select QAUseCasePoints.Oid from QAUseCasePoints where Project = '" + oId + "' )";

                #endregion

                StringBuilder newsb = new StringBuilder();
                newsb.Append("<queryxml><entity>Task</entity>").Append(System.Environment.NewLine);
                newsb.Append("<query><condition><field>PhaseID<expression op=\"Equals\">" + item.Value + "</expression></field></condition>").Append(System.Environment.NewLine);
                newsb.Append("<condition><field>ProjectID<expression op=\"Equals\">" + ATProjectId + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                newsb.Append("</queryxml>").Append(System.Environment.NewLine);
                var Autotask_query = clientAuto.query(at_integrations, newsb.ToString());
                if (Autotask_query.ReturnCode == 1)
                {
                    foreach (var Autotask_task in Autotask_query.EntityResults)
                    {
                        Autotask_tasks.Add(((Task)Autotask_task).Title.ToString(), ((Task)Autotask_task).id);
                        Autotask_tasks_statues.Add(((Task)Autotask_task).id, ((Task)Autotask_task).Status.ToString());

                        //tottal = tottal + (int.Parse(((Task)Autotask_task).EstimatedHours.ToString()) - int.Parse(((Task)Autotask_task).RemainingHours.ToString()))
                    }
                }

                SqlCommand command2 = new SqlCommand();
                command2.CommandText = "Select [Activity],[Department],[Resource],[AvgEstVal],[AutoTaskID],[GCRecord] from [QAActivity] Where [QAWBS]='" + item.Key + "'";
                command2.Connection = connection;
                try
                {
                    reader.Close();
                }
                catch { }
                SqlDataReader readerAct = command2.ExecuteReader();
                List<Task> tasksToUpdate = new List<Task>();
                List<Task> tasksToAdd = new List<Task>();
                List<Task> tasksToDelete = new List<Task>();
                List<Task> trackingtaskToAdd = new List<Task>();
                List<Task> tracingtaskToUpdate = new List<Task>();

                //ATA [Start]
                foreach (QAProjectEntity projectentity in this.ProjectEntities)
                {
                    if (projectentity.TrackingNumber != null && projectentity.TrackingNumber.TrackingTasks != null)
                    {
                        foreach (TrackingTask TASK in projectentity.TrackingNumber.TrackingTasks)
                        {

                            if (TASK.WBSMonth.Oid.ToString() == item.Key)//&& (TASK.AutotaskID == null || TASK.AutotaskID == 0))// && //Autotask ID not null or 0)
                            {
                                foreach (var ATtask in Autotask_tasks)
                                {
                                    if (TASK.Tittle == ATtask.Key && TASK.AutotaskID == 0)
                                    {
                                        TASK.AutotaskID = ATtask.Value;
                                        break;
                                    }
                                }
                                if (TASK.AutotaskID > 0)
                                {
                                    Task ExistTask = new Task();
                                    ExistTask.Title = TASK.Tittle;
                                    ExistTask.id = TASK.AutotaskID;
                                    ExistTask.AllocationCodeID = 30043275;
                                    ExistTask.PhaseID = item.Value;
                                    ExistTask.ProjectID = ATProjectId.ToString();
                                    foreach (var Task_statues in Autotask_tasks_statues)
                                    {
                                        if (TASK.AutotaskID == Task_statues.Key)
                                        {
                                            ExistTask.Status = Task_statues.Value;
                                            break;
                                        }
                                    }
                                    //tsk2.Status = 1;
                                    ExistTask.StartDateTime = TASK.StartDate;
                                    ExistTask.EndDateTime = TASK.EndDate;
                                    ExistTask.EstimatedHours = TASK.Duration;
                                    ExistTask.TaskType = 1;
                                    ExistTask.DepartmentID = TASK.Resources.AutoTaskDepartementID;
                                    ExistTask.AssignedResourceID = TASK.Resources.AutoTaskID;
                                    ExistTask.AssignedResourceRoleID = TASK.Resources.AutoTaskRoleID;
                                    tracingtaskToUpdate.Add(ExistTask);
                                    //}
                                }
                                else
                                {// New Task Has No Autotask ID So it will be added to Autotask
                                    Task NewTask = new Task();
                                    //tsk2.Title = "[Tracking" + projectentity.TrackingNumber.ID + "+" + projectentity.Name + "]_" + TASK.Task.Name.ToString();
                                    NewTask.Title = TASK.Tittle;
                                    //tsk3.id = 0;
                                    NewTask.AllocationCodeID = 30043275;
                                    NewTask.PhaseID = item.Value;
                                    NewTask.ProjectID = ATProjectId.ToString();
                                    // tsk.Status = TASK.Status.ToString();
                                    NewTask.Status = 1;
                                    NewTask.StartDateTime = TASK.StartDate;
                                    NewTask.EndDateTime = TASK.EndDate;
                                    NewTask.EstimatedHours = TASK.Duration;
                                    NewTask.TaskType = 1;
                                    NewTask.DepartmentID = TASK.Resources.AutoTaskDepartementID;
                                    NewTask.AssignedResourceID = TASK.Resources.AutoTaskID;
                                    NewTask.AssignedResourceRoleID = TASK.Resources.AutoTaskRoleID;
                                    trackingtaskToAdd.Add(NewTask);
                                }
                            }
                        }
                    }
                }
                //ATA[End]
                while (readerAct.Read())
                {
                    Task PhaseActivity = new Task();
                    if (!string.IsNullOrEmpty(readerAct.GetValue(4).ToString()) && int.Parse(readerAct.GetValue(4).ToString()) != 0)
                    {
                        if (readerAct.GetValue(5).ToString() != null && readerAct.GetValue(5).ToString() != "")
                        {
                            PhaseActivity.id = long.Parse(readerAct.GetValue(4).ToString());
                            PhaseActivity.Title = readerAct.GetValue(0).ToString();
                            PhaseActivity.AllocationCodeID = 30043275;
                            PhaseActivity.PhaseID = item.Value;
                            PhaseActivity.ProjectID = ATProjectId.ToString();
                            PhaseActivity.Status = 1;
                            PhaseActivity.EstimatedHours = readerAct.GetValue(3);
                            PhaseActivity.TaskType = 1;
                            PhaseActivity.DepartmentID = 30042970;
                            //tsk.AssignedResourceRoleID = 30207555; //18337452;
                            //tsk.AssignedResourceID = 29834747;//readerAct.GetValue(2).ToString();
                            PhaseActivity.AssignedResourceID = 29834747;
                            PhaseActivity.AssignedResourceRoleID = 18337452;
                            tasksToDelete.Add(PhaseActivity);
                        }
                        else
                        {
                            PhaseActivity.id = long.Parse(readerAct.GetValue(4).ToString());
                            PhaseActivity.Title = readerAct.GetValue(0).ToString();
                            PhaseActivity.AllocationCodeID = 30043275;
                            PhaseActivity.PhaseID = item.Value;
                            PhaseActivity.ProjectID = ATProjectId.ToString();
                            PhaseActivity.Status = 1;
                            PhaseActivity.EstimatedHours = readerAct.GetValue(3);
                            PhaseActivity.TaskType = 1;
                            PhaseActivity.DepartmentID = 30042970;
                            //tsk.AssignedResourceRoleID = 30207555; //18337452;
                            //tsk.AssignedResourceID = 29834747;//readerAct.GetValue(2).ToString();
                            PhaseActivity.AssignedResourceID = 29834747;
                            PhaseActivity.AssignedResourceRoleID = 18337452;
                            tasksToUpdate.Add(PhaseActivity);
                        }
                    }
                    else
                    {

                        PhaseActivity.Title = readerAct.GetValue(0).ToString();
                        PhaseActivity.AllocationCodeID = 30043275;
                        PhaseActivity.PhaseID = item.Value;
                        PhaseActivity.ProjectID = ATProjectId.ToString();
                        PhaseActivity.Status = 1;
                        PhaseActivity.EstimatedHours = readerAct.GetValue(3);
                        PhaseActivity.TaskType = 1;
                        PhaseActivity.DepartmentID = 30042970;
                        PhaseActivity.AssignedResourceID = 29834747;
                        PhaseActivity.AssignedResourceRoleID = 18337452;
                        tasksToAdd.Add(PhaseActivity);

                    }
                }
                try
                {
                    readerAct.Close();
                }

                catch { }
                if (tasksToUpdate.Count > 0)
                {
                    SubAutoTask1.Entity[] entArr1 = tasksToUpdate.ToArray();
                    ATWSResponse ResponseAuto1 = clientAuto.update(at_integrations, entArr1);
                }
                //ATA[start]
                if (tracingtaskToUpdate.Count > 0)
                {
                    SubAutoTask1.Entity[] entArr1 = tracingtaskToUpdate.ToArray();
                    ATWSResponse ResponseAuto1 = clientAuto.update(at_integrations, entArr1);
                    if (ResponseAuto1.ReturnCode == 1)
                    {
                        //
                    }
                }
                //ATA[End]
                if (tasksToDelete.Count > 0)
                {
                    SubAutoTask1.Entity[] entArr1 = tasksToDelete.ToArray();
                    ATWSResponse ResponseAuto1 = clientAuto.delete(at_integrations, entArr1);
                }
                if (tasksToAdd.Count > 0)
                {
                    SubAutoTask1.Entity[] entArr1 = tasksToAdd.ToArray();
                    ATWSResponse ResponseAuto1 = clientAuto.create(at_integrations, entArr1);

                    if (ResponseAuto1.ReturnCode == 1)
                    {
                        //New
                        foreach (var t in ResponseAuto1.EntityResults)
                        {
                            Task ts = (Task)t;
                            command.CommandText = "Update [QAActivity] set AutoTaskID = " + ts.id.ToString() + " Where   [Activity] = '" + ts.Title + "' and [QAWBS]='" + item.Key + "'";
                            try
                            {
                                reader.Close();
                            }
                            catch { }
                            command.ExecuteNonQuery();
                        }
                    }
                    //New
                    else
                    {
                        returnVal = false;
                    }
                }
                //ATA[start]
                if (trackingtaskToAdd.Count > 0)
                {

                    SubAutoTask1.Entity[] entArr1 = trackingtaskToAdd.ToArray();
                    ATWSResponse ResponseAuto1 = clientAuto.create(at_integrations, entArr1);

                    if (ResponseAuto1.ReturnCode == 1)
                    {
                        //New
                        foreach (var t in ResponseAuto1.EntityResults)
                        {
                            Task ts = (Task)t;
                            command.CommandText = "Update [TrackingTask] set AutoTaskID = " + ts.id.ToString() + " Where [Tittle] = '" + ts.Title + "' and [WBSMonth]='" + item.Key + "'";
                            //ts.Title.ToString().Contains("[Tracking" + "[TrackingEntry].[ID]" + "+" + "]_" + "[Task].[Name]") + "' and [QAWBS]='" + item.Key + "'";
                            try
                            {
                                reader.Close();
                            }
                            catch { }
                            command.ExecuteNonQuery();
                        }
                    }
                    //New
                    else
                    {
                        returnVal = false;
                    }
                }
                //ATA [END]



            }
            return returnVal;
        }
    }
}
