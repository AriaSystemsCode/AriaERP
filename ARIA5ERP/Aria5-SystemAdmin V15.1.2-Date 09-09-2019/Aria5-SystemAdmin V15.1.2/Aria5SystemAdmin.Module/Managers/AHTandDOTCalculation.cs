using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DevExpress.ExpressApp;
using Aria5SystemAdmin.Module.SubAutoTask1;
using System.ServiceModel;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Data.Filtering;
using DevExpress.Xpo;
using DevExpress.Xpo.Metadata;
using System.Data.SqlClient;
using System.Data.Common;
using System.Data;
using System.IO;
using DevExpress.Spreadsheet;
using DevExpress.Web.ASPxSpreadsheet;
using System.Drawing;
using System.Text.RegularExpressions;
namespace Aria5SystemAdmin.Module.Managers
{
    // ATA this class have all functions needed in kpi calculations
  public class AHTandDOTCalculation
    {
        # region Declarations
       // private static string auth_user_id = "PPMO@ariasystems.biz"; // user@domain.com
        //private static string auth_user_password = "password7asbyraby";
        private static string auth_user_id = "it@ariasystems.biz"; // user@domain.com
        private static string auth_user_password = "Aria@2016";
        private static ATWSZoneInfo zoneInfo = null;
        private static BasicHttpBinding myBinding;
        public static AutotaskIntegrations at_integrations = new AutotaskIntegrations();
        public static Aria5SystemAdmin.Module.SubAutoTask1.ATWSSoapClient clientAuto = new Aria5SystemAdmin.Module.SubAutoTask1.ATWSSoapClient();
        #endregion
        public AHTandDOTCalculation()
        {
            //MMT
            System.Net.ServicePointManager.SecurityProtocol = System.Net.SecurityProtocolType.Tls12;
            //mmt
            zoneInfo = clientAuto.getZoneInfo(auth_user_id);
            clientAuto = new ATWSSoapClient();
            myBinding = new BasicHttpBinding();
            myBinding.Security.Mode = BasicHttpSecurityMode.Transport;
            myBinding.Security.Transport.ClientCredentialType = HttpClientCredentialType.Basic;
            myBinding.MaxReceivedMessageSize = 2147483647;
            EndpointAddress ea = new EndpointAddress(zoneInfo.URL);
            clientAuto = new ATWSSoapClient(myBinding, ea);
            clientAuto.ClientCredentials.UserName.UserName = auth_user_id;
            clientAuto.ClientCredentials.UserName.Password = auth_user_password;
        }
       // const string connectionstr = @"Data Source=NSDE_KHALED;Initial Catalog=Aria5SystemAdmin22;User ID=sa;Password=aria_123";
        const string connectionstr = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin_Test;User ID=sa;Password=aria_123";
      //const string connectionstr = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123";
        //const string connectionstr = @"Data Source=NSDE_KHALED;Initial Catalog=Aria5SystemAdmin_testing;User ID=sa;Password=aria_123";
 
      //ATA [Start]
      /// <summary>
      /// Collect OTD for tasks from auto task and save it in system admin 
      /// </summary>
      /// <param name="datefrom"></param>
      /// <param name="dateto"></param>
      /// <param name="Objectspace"></param>
        public  void getDOTdata(DateTime datefrom, DateTime dateto, Session Objectspace)//, IObjectSpace Objectspace) 
         {
            StringBuilder newsb = new StringBuilder();
            newsb.Append("<queryxml><entity>Task</entity>").Append(System.Environment.NewLine);
            newsb.Append("<query><condition><field>CompletedDateTime<expression op=\"GreaterThanorEquals\">" + datefrom + "</expression></field></condition>").Append(System.Environment.NewLine);
            newsb.Append("<condition operator=\"AND\"><field>CompletedDateTime<expression op=\"LessThanOrEquals\">" + dateto + "</expression></field></condition></query>").Append(System.Environment.NewLine);
            newsb.Append("</queryxml>").Append(System.Environment.NewLine);
            var Autotask_query = clientAuto.query(at_integrations, newsb.ToString());
            if (Autotask_query.ReturnCode == 1)
            {
                if (Autotask_query.EntityResults.Length > 0)
                 {
                    //IList<TasksDOT> alltasks = Objectspace.GetObjects( new ObjectsQuery())
                    //IList<TasksDOT> alltasks = Objectspace.GetObjects<TasksDOT>(CriteriaOperator.Parse("[TaskCompleteDate] >= '" + datefrom + "' and [TaskCompleteDate] <= '" + dateto + "'"));
                    foreach (Aria5SystemAdmin.Module.SubAutoTask1.Task onetask in Autotask_query.EntityResults)
                    {
                        
                        //ATA check task doublication 
                        TasksDOT taskone = Objectspace.FindObject<TasksDOT>(CriteriaOperator.Parse("[TaskId] = '" + onetask.id.ToString() + "'"));
                        if (taskone != null)
                        {
                            if (onetask.EndDateTime != null && onetask.AssignedResourceID != null && onetask.CompletedDateTime != null)
                            {
                                if (taskone.BaselineDueDate != DateTime.Parse(onetask.EndDateTime.ToString()) || taskone.ResourceId != int.Parse(onetask.AssignedResourceID.ToString()) || taskone.TaskCompleteDate != DateTime.Parse(onetask.CompletedDateTime.ToString()))
                                {
                                    //ATA 27/10/2016 change baseline due date to be task end date [start]
                                
                                    // taskone.BaselineDueDate = DateTime.Parse(onetask.UserDefinedFields[4].Value);
                                    //taskone.DeliveryOnTime = (long)Math.Round(DateTime.Parse(onetask.CompletedDateTime.ToString()).Subtract(DateTime.Parse(onetask.UserDefinedFields[4].Value.ToString())).TotalDays, 0);
                                    
                                    taskone.BaselineDueDate = DateTime.Parse(onetask.EndDateTime.ToString());
                                    taskone.TaskCompleteDate = DateTime.Parse(onetask.CompletedDateTime.ToString());
                                    if (DateTime.Parse(onetask.CompletedDateTime.ToString()).Subtract(DateTime.Parse(onetask.EndDateTime.ToString())).TotalDays < 1 && DateTime.Parse(onetask.CompletedDateTime.ToString()).Subtract(DateTime.Parse(onetask.EndDateTime.ToString())).TotalDays > 0)
                                    {
                                        taskone.DeliveryOnTime = 0;
                                    }
                                    else
                                    {
                                        taskone.DeliveryOnTime = (long)Math.Round(DateTime.Parse(onetask.CompletedDateTime.ToString()).Subtract(DateTime.Parse(onetask.EndDateTime.ToString())).TotalDays, 0);
                                    }
                                    //ATA 27/10/2016 change baseline due date to be task end date [end]
                                    if (onetask.AssignedResourceID != null && taskone.ResourceId != int.Parse(onetask.AssignedResourceID.ToString()))
                                    { 
                                        //ATA modify the task if there are any changes on baseline or resource fields [start]
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
                                    //ATA modify the task if there are any changes on baseline or resource fields  [End]
                                }
                               
                            }
                            continue;
                        }
                        // ATA create new task if it is not exist 
                        TasksDOT newtasks = new TasksDOT(Objectspace);
                        newtasks.TaskTitle = onetask.Title.ToString();
                        newtasks.TaskCompleteDate = DateTime.Parse(onetask.CompletedDateTime.ToString());
                        newtasks.TaskId = onetask.id.ToString();
                        if (onetask.EndDateTime != null)
                        {
                            newtasks.BaselineDueDate = DateTime.Parse(onetask.EndDateTime.ToString());
                            if (DateTime.Parse(onetask.CompletedDateTime.ToString()).Subtract(DateTime.Parse(onetask.EndDateTime.ToString())).TotalDays < 1 && DateTime.Parse(onetask.CompletedDateTime.ToString()).Subtract(DateTime.Parse(onetask.EndDateTime.ToString())).TotalDays > 0)
                            {
                                newtasks.DeliveryOnTime = 0;
                            }
                            else
                            {
                                newtasks.DeliveryOnTime = (long)Math.Round(DateTime.Parse(onetask.CompletedDateTime.ToString()).Subtract(DateTime.Parse(onetask.EndDateTime.ToString())).TotalDays, 0);
                            }
                            //newtasks.DeliveryOnTime = (long)Math.Round(DateTime.Parse(onetask.CompletedDateTime.ToString()).Subtract(DateTime.Parse(onetask.EndDateTime.ToString())).TotalDays, 0);
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
                                    newtasks.ProjectName = oneproject.ProjectName.ToString();
                                    newtasks.ProjectNumber = int.Parse(oneproject.id.ToString());
                                    if (oneproject.CompletedDateTime != null)
                                    {
                                        newtasks.ProjectCompleteDate = DateTime.Parse(oneproject.CompletedDateTime.ToString());
                                    }
                                }
                            }
                        }        
                        if (onetask.AssignedResourceID != null)
                        {
                            //ATA  get task resource from auto task depend on resource id field in taks 
                            StringBuilder resourcequery = new StringBuilder();
                            resourcequery.Append("<queryxml><entity>Resource</entity>").Append(System.Environment.NewLine);
                            resourcequery.Append("<query><condition><field>id<expression op=\"Equals\">" + onetask.AssignedResourceID.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                            resourcequery.Append("</queryxml>").Append(System.Environment.NewLine);
                            var Resourceresponse = clientAuto.query(at_integrations, resourcequery.ToString());
                            if (Resourceresponse.ReturnCode == 1)
                            {
                                if (Resourceresponse.EntityResults.Length > 0)
                                {
                                    foreach (Aria5SystemAdmin.Module.SubAutoTask1.Resource oneresource in Resourceresponse.EntityResults)
                                    {
                                        newtasks.ResourceId = int.Parse(oneresource.id.ToString());
                                        newtasks.ResourceName = oneresource.FirstName.ToString() + " " + oneresource.LastName.ToString();
                                        Resources taskresource = Objectspace.FindObject<Resources>(CriteriaOperator.Parse("[AutoTaskID] = '" + oneresource.id.ToString() + "'"));
                                        //ATA select department from system admin data because we can't retrive the department from auto tasks (cause it debend on role and each user have multiple roles)
                                        if (taskresource != null && taskresource.Department != null)
                                        {
                                            newtasks.DepName = taskresource.Department.Id;
                                        }
#region department from auto task 
                                        //StringBuilder Resourcerolequery = new StringBuilder();
                                        //Resourcerolequery.Append("<queryxml><entity>ResourceRole</entity>").Append(System.Environment.NewLine);
                                        //Resourcerolequery.Append("<query><condition><field>id<expression op=\"Equals\">" + onetask.AssignedResourceRoleID.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                                        //Resourcerolequery.Append("</queryxml>").Append(System.Environment.NewLine);
                                        //var Resourceroleresponse = clientAuto.query(at_integrations, Resourcerolequery.ToString());
                                        //if (Resourceroleresponse.ReturnCode == 1)
                                        //{
                                        //    if (Resourceroleresponse.EntityResults.Length > 0)
                                        //    {
                                        //        foreach (Aria5SystemAdmin.Module.SubAutoTask1.ResourceRole onerole in Resourceroleresponse.EntityResults)
                                        //        {
                                        //            StringBuilder Departmentquery = new StringBuilder();
                                        //            Departmentquery.Append("<queryxml><entity>Department</entity>").Append(System.Environment.NewLine);
                                        //            Departmentquery.Append("<query><condition><field>id<expression op=\"Equals\">" + onerole.DepartmentID.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                                        //            Departmentquery.Append("</queryxml>").Append(System.Environment.NewLine);
                                        //            var Departmentresponse = clientAuto.query(at_integrations, Departmentquery.ToString());
                                        //            if (Departmentresponse.ReturnCode == 1)
                                        //            {
                                        //                foreach (Aria5SystemAdmin.Module.SubAutoTask1.Department onedepartment in Departmentresponse.EntityResults)
                                        //                {
                                        //                    newtasks.DepName = onedepartment.Name.ToString();
                                        //                }
                                        //            }
                                        //        }
                                        //    }
                                        //}
#endregion 
                                    }
                                }

                            }
                        }
                        newtasks.Save();
                    }
                }
            }
        }
      //ATA   [End]


        public  void generateDOT(DateTime datefrom, DateTime dateto, Session objectspace,DeliveryOnTime existconfig)
        {
            //ATA send email to verfiy taht this function start work [start]
            if (existconfig == null)
            {
                Email VerfiyEmail = new Email();
                VerfiyEmail.FromEmail = "qua@ariasystems.biz";
                VerfiyEmail.EmailPassword = "quality_123";
                VerfiyEmail.ToEmail = "ahmed.r@ariany.com";
                VerfiyEmail.EmailTitle = "OTD&AHTCalculationReportVerfication";
                VerfiyEmail.EmailBody = "Generate OTD Function start work";
                VerfiyEmail.SendEmail();
            }
            
            //ATA send email to verfiy taht this function start work [start]
            //Guid configration = createDOTconfig(datefrom,dateto, DateTime.Now.ToString("MMMM")+DateTime.Now.ToString("YYYY") + " DOT", new Session());
            getDOTdata(dateto.Subtract(new TimeSpan(2, 0, 0, 0)), dateto, objectspace);
            DeliveryOnTime configration = null;
            if (existconfig == null)
            {
                configration = new DeliveryOnTime(objectspace);
                configration.DateFrom = datefrom;
                configration.DateTO = dateto;
                configration.Title = "Aria OTD KPI " + datefrom.ToString("MMMM") + " " + datefrom.Year;
               // configration.Title = "Aria OTD KPI " + datefrom.ToString("MMMM") + " " + datefrom.Year + "Testing";
                configration.Save();
            }else
            {
                configration = existconfig;
            }
           
            //create Project DOT 
            calculateprojectdeliveryontime(configration, true);
            //Start loop on all departments and resources to calculate DOT 
            System.Collections.ICollection alldepartments = configration.Session.GetObjects(configration.Session.Dictionary.GetClassInfo(typeof(Aria5SystemAdmin.Module.BusinessObjects.Department)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
            IList<Aria5SystemAdmin.Module.BusinessObjects.Department> Departments = alldepartments.Cast<Aria5SystemAdmin.Module.BusinessObjects.Department>().ToList();
            CalculateResourcedeliveryontime(configration, null, true);
            foreach (Aria5SystemAdmin.Module.BusinessObjects.Department department in Departments)
            {
                foreach (Resources Resource in department.Resourceses)
                {
                    CalculateResourcedeliveryontime(configration,Resource, true);
                }
                CalculateDepartmentdeliveryontime(department, configration, true);
            }
            //ATA  calculate product otd for each product 5/29/2017 [start] 
            System.Collections.ICollection allproducts = configration.Session.GetObjects(configration.Session.Dictionary.GetClassInfo(typeof(Aria5SystemAdmin.Module.BusinessObjects.Product)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
            IList<Aria5SystemAdmin.Module.BusinessObjects.Product> Products = allproducts.Cast<Aria5SystemAdmin.Module.BusinessObjects.Product>().ToList();
            foreach (Aria5SystemAdmin.Module.BusinessObjects.Product product in Products)
            {
                CalculateProductontimedelivery(product, configration, true);
            }
            // createresourceDOT(configration);
            if (existconfig == null)
            {
                Stream attachemntasasstream = new MemoryStream(DOTattachement(configration));
                System.Net.Mail.Attachment attach = new System.Net.Mail.Attachment(attachemntasasstream, configration.Title.ToString() + ".xlsx");
                Email newone = new Email();
                newone.Attachement = attach;
                newone.FromEmail = "qua@ariasystems.biz";
                newone.EmailPassword = "quality_123";
              //  newone.ToEmail = "ahmed.t@ariany.com";
                newone.EmailBody = "Dear All,<br /><br /> Please find the attached Excel file for the Projects OTD of " + datefrom.ToString("MMMM") + " KPI " + datefrom.Year;
                newone.ToEmail = "Ahmed.r@ariasystems.biz,ahmed.r@ariany.com,sara.a@ariany.com";//,Wael.A@ariasystems.biz,amr.h@ariasystems.biz,Khaled.m@ariasystems.biz,Ahmed.f@ariasystems.biz";//,ahmed.r@ariasystems.biz";
                newone.EmailTitle = "Projects OTD KPI " + datefrom.ToString("MMMM") + " " + datefrom.Year;
                newone.EmailSubject = "Projects OTD KPI " + datefrom.ToString("MMMM") + " " + datefrom.Year;
                // newone.EmailCC = "omar.r@ariasystems.biz";//"omar.r@ariasystems.biz"
                newone.SendEmail();
            }
            #region sql code
            //SqlConnection CS = new SqlConnection(connectionstr);
            //CS.Open();
            //SqlDataAdapter dp = new SqlDataAdapter("select * from TasksDOT Where [TaskCompleteDate] >= '" + datefrom + "'and [TaskCompleteDate] <= '"+dateto+"'", CS);
            //DataSet ds = new DataSet();
            //DataTable dt = new DataTable("TasksDOT");
            //dp.Fill(dt);
            //DataTable dt2 = new DataTable("Resources");
            //dp.SelectCommand = new SqlCommand("select * from Resources");
            //dp.Fill(dt2);
            //ds.Tables.Add(dt);
            //CS.Close();

            //foreach (DataRow row in dt2.Rows)
            //{
            //    DataRow[] rowdata = dt.Select("ResourceId = '" + row["AutoTaskID"] + "'");
            //    DataRow[] OtDrows = dt.Select("ResourceId = '" + row["AutoTaskID"] + "'and DeliveryOnTime <= 0");
            //    foreach (DataRow task in rowdata)
            //    {
            //        Total += int.Parse(task["DeliveryOnTime"].ToString());
            //    }
            //    createresourceDOT(row["oid"].ToString(), rowdata.Count(), Total, OtDrows.Count(), configration.ToString(), new Session());
            //}
            #endregion 
           
        }

        public void CalculateResourcedeliveryontime(DeliveryOnTime DOTconfigration, Resources resource, bool ismaster)
        {
            Resource_DOT Oneresource = DOTconfigration.ResourcesDOT.FirstOrDefault(x => x.CreateDate == DOTconfigration.DateFrom && x.ResourceName == resource);
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
                    System.Collections.ICollection listoftasks = DOTconfigration.Session.GetObjects(DOTconfigration.Session.Dictionary.GetClassInfo(typeof(TasksDOT)), CriteriaOperator.Parse("ResourceId = '" + null + "'and[TaskCompleteDate] >= '" + DOTconfigration.DateFrom + "'and [TaskCompleteDate] <= '" + DOTconfigration.DateTO + "' "), null, 1000, false, false);
                    alltasks = listoftasks.Cast<TasksDOT>().ToList();
                    otdtaskscount = DOTconfigration.Session.GetObjects(DOTconfigration.Session.Dictionary.GetClassInfo(typeof(TasksDOT)), CriteriaOperator.Parse("ResourceId = '" + null + "'and[TaskCompleteDate] >= '" + DOTconfigration.DateFrom + "'and [TaskCompleteDate] <= '" + DOTconfigration.DateTO + "'and DeliveryOnTime <= 0"), null, 1000, false, false).Count;
                }
                else
                {
                    if (resource.AutoTaskID != null)
                    {

                        System.Collections.ICollection listoftasks = DOTconfigration.Session.GetObjects(DOTconfigration.Session.Dictionary.GetClassInfo(typeof(TasksDOT)), CriteriaOperator.Parse("ResourceId = '" + resource.AutoTaskID + "'and[TaskCompleteDate] >= '" + DOTconfigration.DateFrom + "'and [TaskCompleteDate] <= '" + DOTconfigration.DateTO + "' "), null, 1000, false, false);
                        alltasks = listoftasks.Cast<TasksDOT>().ToList();
                        otdtaskscount = DOTconfigration.Session.GetObjects(DOTconfigration.Session.Dictionary.GetClassInfo(typeof(TasksDOT)), CriteriaOperator.Parse("ResourceId = '" + resource.AutoTaskID + "'and[TaskCompleteDate] >= '" + DOTconfigration.DateFrom + "'and [TaskCompleteDate] <= '" + DOTconfigration.DateTO + "'and DeliveryOnTime <= 0"), null, 1000, false, false).Count;

                    }
                }
                if (alltasks.Count > 0)
                {
                    Resource_DOT newrecord = new Resource_DOT(DOTconfigration.Session);

                    DOTconfigration.Session.BeginNestedUnitOfWork();

                    newrecord.DOT = Math.Round(genericcalculatdotfromtasksdottable(alltasks, DOTconfigration.Penality) / alltasks.Count, 0);
                    newrecord.Total = genericcalculatdotfromtasksdottable(alltasks, DOTconfigration.Penality);
                    newrecord.ResourceName = resource;
                    newrecord.NumberOfTasks = alltasks.Count;
                    newrecord.NumberofOTDTasks = otdtaskscount;
                    decimal otd = Convert.ToDecimal(Convert.ToDouble(otdtaskscount) / alltasks.Count);
                    newrecord.OTD = (double)Math.Round(otd * 100, 0);
                    newrecord.CreateDate = DOTconfigration.DateFrom;
                    if (ismaster)
                    {
                        newrecord.IsMaster = ismaster;
                    }
                    newrecord.DeliveryOnTime = DOTconfigration;
                    newrecord.Save();
                }

            }

        }
        public void CalculateDepartmentdeliveryontime(Aria5SystemAdmin.Module.BusinessObjects.Department department, DeliveryOnTime DOTConfigration, bool ismaster)
        {
            Department_DOT onedepartment = DOTConfigration.DepartmentsDOT.FirstOrDefault(x => x.CreateDate == DOTConfigration.DateFrom && x.Department == department);
            if (onedepartment == null)
            {
                double deliveryontime = 0;
                int numberoftasks = 0;
                int otdtaskscount = 0;
                Aria5SystemAdmin.Module.BusinessObjects.Department Department = DOTConfigration.Session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Department>(CriteriaOperator.Parse("[oid] = '" + department.Oid + "'"));

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
                                    if (Item.DeliveryOnTime == DOTConfigration)
                                    {
                                        deliveryontime += (int)Item.Total;
                                        numberoftasks += Item.NumberOfTasks;
                                        otdtaskscount += Item.NumberofOTDTasks;
                                    }
                                }

                            }
                        }
                        Department_DOT newrecord = new Department_DOT(DOTConfigration.Session);
                        if (numberoftasks > 0)
                        {

                            newrecord.DOT = (int)Math.Round(deliveryontime / numberoftasks, 0);
                            newrecord.Total = (int)deliveryontime;
                            newrecord.Department = Department;
                            newrecord.NumberOfTasks = numberoftasks;
                            newrecord.NumberofOTDTasks = otdtaskscount;
                            decimal otd = Convert.ToDecimal(Convert.ToDouble(otdtaskscount) / numberoftasks);
                            newrecord.OTD = (double)Math.Round(otd * 100, 0);
                            newrecord.CreateDate = DOTConfigration.DateFrom;
                            if (ismaster)
                            {
                                newrecord.IsMaster = ismaster;
                            }
                            newrecord.DeliveryOnTime = DOTConfigration;
                            newrecord.Save();
                            //newrecord.Session.CommitTransaction();
                            //if (DOTConfigration != null)
                            //{
                            //    DeliveryOnTime configration = objectspace.FindObject<DeliveryOnTime>(CriteriaOperator.Parse("[oid] = '" + DOTConfigration.Oid + "'"));
                            //    if (configration != null)
                            //    {
                            //        configration.DepartmentsDOT.Add(newrecord);
                            //        configration.Save();
                            //        configration.Session.CommitTransaction();
                            //    }
                            //}
                        }
                    }
                }
            }
        }
      //ATA add new function to calculate product otd 5/29/2017 [start] 
        public void CalculateProductontimedelivery(Aria5SystemAdmin.Module.BusinessObjects.Product product, DeliveryOnTime DOTConfigration, bool ismaster)
        {
            ProductOTD product_otd = DOTConfigration.ProductsOTD.FirstOrDefault(x => x.CreateDate == DOTConfigration.DateFrom && x.Product == product);
            if (product_otd == null)
            {
                double deliveryontime = 0;
                int numberoftasks = 0;
                int otdtaskscount = 0;
                Aria5SystemAdmin.Module.BusinessObjects.Product existProduct = DOTConfigration.Session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Product>(CriteriaOperator.Parse("[oid] = '" + product.Oid + "'"));

                if (existProduct != null)
                {
                    if (product.Resources.Count > 0)
                    {
                        foreach (Resources record in product.Resources)
                        {
                            if (record.ResourceDOT.Count > 0)
                            {
                                foreach (Resource_DOT Item in record.ResourceDOT)
                                {
                                    if (Item.DeliveryOnTime == DOTConfigration)
                                    {
                                        deliveryontime += (int)Item.Total;
                                        numberoftasks += Item.NumberOfTasks;
                                        otdtaskscount += Item.NumberofOTDTasks;
                                    }
                                }
                            }
                        }
                        ProductOTD newrecord = new ProductOTD(DOTConfigration.Session);
                        if (numberoftasks > 0)
                        {

                            newrecord.DOT = (int)Math.Round(deliveryontime / numberoftasks, 0);
                            newrecord.Total = (int)deliveryontime;
                            newrecord.Product = existProduct;
                            newrecord.NumberOfTasks = numberoftasks;
                            newrecord.NumberofOTDTasks = otdtaskscount;
                            decimal otd = Convert.ToDecimal(Convert.ToDouble(otdtaskscount) / numberoftasks);
                            newrecord.OTD = (double)Math.Round(otd * 100, 0);
                            newrecord.CreateDate = DOTConfigration.DateFrom;
                            if (ismaster)
                            {
                                newrecord.IsMaster = ismaster;
                            }
                            newrecord.OnTimeDeliveryReport = DOTConfigration;
                            newrecord.Save();
                        }
                    }
                }
            }
        }
     //ATA add new function to calculate product otd 5/29/2017 [End]
        public void calculateprojectdeliveryontime(DeliveryOnTime DOTconfigration, bool ismaster)
        {
            Projects_Delivery_on_Time oneproject = DOTconfigration.ProjectsDOT.FirstOrDefault(x => x.CreateDate == DOTconfigration.DateFrom);
            if (oneproject == null)
            {
                StringBuilder SelectProject = new StringBuilder();
                SelectProject.Append("<queryxml><entity>Project</entity>").Append(System.Environment.NewLine);
                SelectProject.Append("<query><condition><field>CompletedDateTime<expression op=\"IsNull\"></expression></field></condition>").Append(System.Environment.NewLine);
                SelectProject.Append("<condition operator=\"OR\"><field>CompletedDateTime<expression op=\"GreaterThanorEquals\">" + DOTconfigration.DateFrom + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                SelectProject.Append("</queryxml>").Append(System.Environment.NewLine);
                var completion = clientAuto.query(at_integrations, SelectProject.ToString());
                if (completion.ReturnCode == 1)
                {
                    foreach (Project Project in completion.EntityResults)
                    {
                        System.Collections.ICollection listoftasks = DOTconfigration.Session.GetObjects(DOTconfigration.Session.Dictionary.GetClassInfo(typeof(TasksDOT)), CriteriaOperator.Parse("[ProjectNumber]= '" + Project.id + "' and [TaskCompleteDate] >= '" + DOTconfigration.DateFrom + "' and [TaskCompleteDate] <= '" + DOTconfigration.DateTO + "'"), null, 1000, false, false);
                        IList<TasksDOT> alltasks = listoftasks.Cast<TasksDOT>().ToList();
                        int otdtaskscount = DOTconfigration.Session.GetObjects(DOTconfigration.Session.Dictionary.GetClassInfo(typeof(TasksDOT)), CriteriaOperator.Parse("[ProjectNumber]= '" + Project.id + "' and [TaskCompleteDate] >= '" + DOTconfigration.DateFrom + "' and [TaskCompleteDate] <= '" + DOTconfigration.DateTO + "'and [DeliveryOnTime] <= 0"), null, 1000, false, false).Count;
                        if (alltasks.Count > 0)
                        {
                            double dot = genericcalculatdotfromtasksdottable(alltasks, DOTconfigration.Penality);
                            Projects_Delivery_on_Time newone = new Projects_Delivery_on_Time(DOTconfigration.Session);
                            newone.ProjectName = Project.ProjectName.ToString();
                            newone.Id = Project.id;
                            newone.Total = (long)dot;
                            newone.Deliveryontime = (long)Math.Round(dot / alltasks.Count, 0);
                            newone.Numberoftasks = alltasks.Count;
                            newone.CreateDate = DOTconfigration.DateFrom;
                            newone.NumberofOTDTasks = otdtaskscount;
                            decimal otd = Convert.ToDecimal(Convert.ToDouble(otdtaskscount) / alltasks.Count);
                            newone.OTD = (double)Math.Round(otd * 100, 0);
                            if (ismaster)
                            {
                                newone.IsMaster = ismaster;
                            }
                            newone.Dot = DOTconfigration;
                            newone.Save();
                            //if (DOT != null)
                            //{
                            //    DeliveryOnTime configration = objectspace.FindObject<DeliveryOnTime>(CriteriaOperator.Parse("[oid] = '" + DOT.Oid + "'"));
                            //    if (configration != null)
                            //    {
                            //        configration.ProjectsDOT.Add(newone);
                            //        configration.Save();
                            //        configration.Session.CommitTransaction();
                            //    }
                            //}
                        }

                    }
                }
            }
        }

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
        public byte[] DOTattachement(DeliveryOnTime DOTconfigration)
        {
            ASPxSpreadsheet newspreadsheet = new ASPxSpreadsheet();
            IWorkbook xlWorkBook = newspreadsheet.Document;
            xlWorkBook.Worksheets["Sheet1"].Visible = false;
            System.Collections.ICollection listoftasks = DOTconfigration.Session.GetObjects(DOTconfigration.Session.Dictionary.GetClassInfo(typeof(TasksDOT)), CriteriaOperator.Parse("[TaskCompleteDate] >= '" + DOTconfigration.DateFrom + "'and [TaskCompleteDate] <= '" + DOTconfigration.DateTO + "' "), null, 2000, false, false);
            IList<TasksDOT> AllTasks =  listoftasks.Cast<TasksDOT>().ToList();
            xlWorkBook.Worksheets.Add("Departments OTD");
            xlWorkBook.Worksheets.Add("Resources OTD");
            xlWorkBook.Worksheets.Add("Projects OTD");
            xlWorkBook.Worksheets.Add("Analysis Sheet");
            xlWorkBook.Worksheets.Add("Projects Tasks Data");
            xlWorkBook.Worksheets.Add("ProductsOTD OTD");
            Worksheet xlalltaskssheet = xlWorkBook.Worksheets["Projects Tasks Data"];
            xlalltaskssheet.Range["A1:F1"].FillColor = Color.MediumPurple;
            xlalltaskssheet.Range["A1:F1"].Font.Color = Color.Black;
            xlalltaskssheet[0, 0].Value = "Project Name";
            xlalltaskssheet[0, 1].Value = "Task Title";
            xlalltaskssheet[0, 2].Value = "Resource";
            xlalltaskssheet[0, 3].Value = "Task Complete Date";
            xlalltaskssheet[0, 4].Value = "Task Base Line";
            xlalltaskssheet[0, 5].Value = "Sch.Var.";
            if (AllTasks.Count > 0)
            {
                int taskrow = 1;
                foreach (TasksDOT onetask in AllTasks)
                {
                    xlalltaskssheet[taskrow, 0].Value = onetask.ProjectName;
                    xlalltaskssheet[taskrow, 1].Value = onetask.TaskTitle;
                    xlalltaskssheet[taskrow, 2].Value = onetask.ResourceName;
                    xlalltaskssheet[taskrow, 3].Value = onetask.TaskCompleteDate;
                    xlalltaskssheet[taskrow, 4].Value = onetask.BaselineDueDate;
                    xlalltaskssheet[taskrow, 5].Value = onetask.DeliveryOnTime;
                    taskrow++;
                }

            }
            if (DOTconfigration.ProjectsDOT.Count > 0)
            {
                Worksheet xlanalysisworksheet = xlWorkBook.Worksheets["Analysis Sheet"];
                xlanalysisworksheet.Range["A1:E1"].FillColor = Color.MediumPurple;
                xlanalysisworksheet.Range["A1:E1"].Font.Color = Color.Black;
                xlanalysisworksheet[0, 0].Value = "Project Name";
                xlanalysisworksheet[0, 1].Value = "Delayed Task Title";
                xlanalysisworksheet[0, 2].Value = " Resource Name";
                xlanalysisworksheet[0, 3].Value = "# of all tasks";
                xlanalysisworksheet[0, 4].Value = "# of Days";
                int xlanalysisrow = 1;
                Worksheet xlWorkSheet = xlWorkBook.Worksheets["Projects OTD"];
                xlWorkSheet.Range["A1:E1"].FillColor = Color.MediumPurple;
                xlWorkSheet.Range["A1:E1"].Font.Color = Color.Black;
                xlWorkSheet.Cells[0, 0].Value = "Project Name";
                xlWorkSheet.Cells[0, 1].Value = "OTD Creat date";
                //xlWorkSheet.Cells[0, 2].Value = "Total Project Tasks (DOT) / Days";
                xlWorkSheet.Cells[0, 2].Value = "# of OTD Tasks";
                xlWorkSheet.Cells[0, 3].Value = "# of Tasks";
                //xlWorkSheet.Cells[0, 5].Value = "AVG Project Tasks (DOT) / Days";
                xlWorkSheet.Cells[0, 4].Value = "Tasks OTD %";

                for (int y = 0; y <= DOTconfigration.ProjectsDOT.Count; y++)
                {
                    int i = y + 1;
                    if (y == DOTconfigration.ProjectsDOT.Count)
                    {
                        xlWorkSheet.Cells[i, 0].Value = "Projects Totals";
                        //xlWorkSheet.Cells[i, 5].Formula = "=ROUND(SUM(" + "C2" + ": C" + (i) + ") /SUM(" + "E2" + ": E" + (i) + "),0) ";
                        xlWorkSheet.Cells[i, 4].Formula = "=ROUND(SUM(" + "C2" + ": C" + (i) + ") /SUM(" + "D2" + ": D" + (i) + ") *100,0)";
                        xlWorkSheet.Range["A" + (i + 1) + ":G" + (i + 1) + ""].FillColor = Color.Yellow;
                    }
                    else
                    {
                        xlWorkSheet.Cells[i, 0].Value = (DOTconfigration.ProjectsDOT[y]).ProjectName;
                        xlWorkSheet.Cells[i, 1].Value = (DOTconfigration.ProjectsDOT[y]).CreateDate;
                        //xlWorkSheet.Cells[i, 2].Value = (DOTconfigration.ProjectsDOT[y]).Total;
                        xlWorkSheet.Cells[i, 2].Value = (DOTconfigration.ProjectsDOT[y]).NumberofOTDTasks;
                        xlWorkSheet.Cells[i, 3].Value = (DOTconfigration.ProjectsDOT[y]).Numberoftasks;
                        //xlWorkSheet.Cells[i, 5].Value = (DOTconfigration.ProjectsDOT[y]).Deliveryontime;
                        xlWorkSheet.Cells[i, 4].Value = (DOTconfigration.ProjectsDOT[y]).OTD;
                        System.Collections.ICollection listofprojecttasks = DOTconfigration.Session.GetObjects(DOTconfigration.Session.Dictionary.GetClassInfo(typeof(TasksDOT)), CriteriaOperator.Parse(" ProjectNumber= '" + DOTconfigration.ProjectsDOT[y].Id + "'and[TaskCompleteDate] >= '" + DOTconfigration.DateFrom + "'and [TaskCompleteDate] <= '" + DOTconfigration.DateTO + "' "), null, 1000, false, false);
                        IList<TasksDOT> projecttaks = listofprojecttasks.Cast<TasksDOT>().ToList();
                        projecttaks = projecttaks.OrderBy(tk => tk.BaselineDueDate).ToList<TasksDOT>();
                        double delayeddays = 0;
                        if (projecttaks.Count > 0)
                        {
                            xlanalysisworksheet[xlanalysisrow, 0].Value = (DOTconfigration.ProjectsDOT[y]).ProjectName;
                            xlanalysisworksheet[xlanalysisrow, 3].Value = projecttaks.Count;
                            foreach (TasksDOT projecttask in projecttaks)
                            {
                                if (projecttask.DeliveryOnTime > delayeddays)
                                {
                                    xlanalysisworksheet[xlanalysisrow, 1].Value = projecttask.TaskTitle;
                                    xlanalysisworksheet[xlanalysisrow, 4].Value = projecttask.DeliveryOnTime;
                                    xlanalysisworksheet[xlanalysisrow, 2].Value = projecttask.ResourceName;
                                    xlanalysisrow++;
                                    delayeddays = projecttask.DeliveryOnTime;
                                }
                            }
                        }

                    }

                }
                //xlWorkSheet.Columns["A"].ColumnWidth = 20;
                //xlWorkSheet.FreezeColumns(0);
                //xlWorkSheet.FreezeRows(0);
                //xlWorkSheet.Columns["B"].ColumnWidth = 6;
                //xlWorkSheet.Rows["1"].RowHeight = 37;
            }
            if (DOTconfigration.ResourcesDOT.Count > 0)
            {
               
                Worksheet xlWorkSheet1 = xlWorkBook.Worksheets["Resources OTD"];
                xlWorkSheet1.Range["A1:E1"].FillColor = Color.MediumPurple;
                xlWorkSheet1.Range["A1:E1"].Font.Color = Color.Black;
                xlWorkSheet1.Cells.Font.Size = 14;
                xlWorkSheet1.Cells[0, 0].Value = "Resource Name";
                xlWorkSheet1.Cells[0, 1].Value = "OTD Creat date";
                //xlWorkSheet1.Cells[0, 2].Value = "Total Resource Tasks (DOT) / Days";
                xlWorkSheet1.Cells[0, 2].Value = "# of OTD Tasks";
                xlWorkSheet1.Cells[0, 3].Value = "# of Tasks";
                //xlWorkSheet1.Cells[0, 5].Value = "Resource AVG. (DOT)";
                xlWorkSheet1.Cells[0, 4].Value = "Tasks OTD %";
                for (int y = 0; y <= DOTconfigration.ResourcesDOT.Count; y++)
                {
                    int i = y + 1;
                    if (y == DOTconfigration.ResourcesDOT.Count)
                    {
                        //xlWorkSheet1.Cells[i, 5].Formula = "=ROUND(SUM(" + "C2" + ": C" + (i) + ") /SUM(" + "E2" + ": E" + (i) + "),0) ";
                        xlWorkSheet1.Cells[i, 4].Formula = "=ROUND(SUM(" + "C2" + ": C" + (i) + ") /SUM(" + "D2" + ": D" + (i) + ") *100,0)";
                        xlWorkSheet1.Range["A" + (i + 1) + ":G" + (i + 1) + ""].FillColor = Color.Yellow;

                    }
                    else
                    {
                        if ((DOTconfigration.ResourcesDOT[y]).ResourceName != null)
                        {
                            xlWorkSheet1.Cells[i, 0].Value = (DOTconfigration.ResourcesDOT[y]).ResourceName.Name;
                        }
                        xlWorkSheet1.Cells[i, 1].Value = (DOTconfigration.ResourcesDOT[y]).CreateDate;
                        //xlWorkSheet1.Cells[i, 2].Value = (DOTconfigration.ResourcesDOT[y]).Total;
                        xlWorkSheet1.Cells[i, 2].Value = (DOTconfigration.ResourcesDOT[y]).NumberofOTDTasks;
                        xlWorkSheet1.Cells[i, 3].Value = (DOTconfigration.ResourcesDOT[y]).NumberOfTasks;
                        //xlWorkSheet1.Cells[i, 5].Value = (DOTconfigration.ResourcesDOT[y]).DOT;
                        xlWorkSheet1.Cells[i, 4].Value = (DOTconfigration.ResourcesDOT[y]).OTD;

                    }

                }
            }
            if (DOTconfigration.DepartmentsDOT.Count > 0)
            {
               
                Worksheet xldepWorkSheet = xlWorkBook.Worksheets["Departments OTD"];
                xldepWorkSheet.Range["A1:E1"].FillColor = Color.MediumPurple;
                xldepWorkSheet.Range["A1:E1"].Font.Color = Color.Black;
                xldepWorkSheet.Cells.Font.Size = 14;
                xldepWorkSheet.Cells[0, 0].Value = "Department Name";
                xldepWorkSheet.Cells[0, 1].Value = "OTD Creat date";
                //xlWorkSheet2.Cells[0, 2].Value = "Total Department Tasks (DOT) / Days";
                xldepWorkSheet.Cells[0, 2].Value = "# of OTD Tasks";
                xldepWorkSheet.Cells[0, 3].Value = "# of Tasks";
                //xlWorkSheet2.Cells[0, 5].Value = "Department AVG. (DOT)";
                xldepWorkSheet.Cells[0, 4].Value = "Tasks OTD %";
                for (int y = 0; y < DOTconfigration.DepartmentsDOT.Count; y++)
                {
                    int i = y + 1;
                    xldepWorkSheet.Cells[i, 0].Value = (DOTconfigration.DepartmentsDOT[y]).Department.Id;
                    xldepWorkSheet.Cells[i, 1].Value = (DOTconfigration.DepartmentsDOT[y]).CreateDate;
                    //xlWorkSheet2.Cells[i, 2].Value = (DOTconfigration.DepartmentsDOT[y]).Total;
                    xldepWorkSheet.Cells[i, 2].Value = (DOTconfigration.DepartmentsDOT[y]).NumberofOTDTasks;
                    xldepWorkSheet.Cells[i, 3].Value = (DOTconfigration.DepartmentsDOT[y]).NumberOfTasks;
                    //xlWorkSheet2.Cells[i, 5].Value = (DOTconfigration.DepartmentsDOT[y]).DOT;
                    xldepWorkSheet.Cells[i, 4].Value = (DOTconfigration.DepartmentsDOT[y]).OTD;
                    switch ((DOTconfigration.DepartmentsDOT[y]).Department.Id.ToString())
                    {
                        case "Client Services":
                            xldepWorkSheet.Range["A" + (i + 1) + ":E" + (i + 1) + ""].FillColor = Color.Purple;
                            xldepWorkSheet.Range["A" + (i + 1) + ":E" + (i + 1) + ""].Font.Color = Color.White;
                            xldepWorkSheet.Range["A" + (i + 1) + ":E" + (i + 1) + ""].Font.FontStyle = SpreadsheetFontStyle.Bold;
                            break;
                        case "Product":
                            xldepWorkSheet.Range["A" + (i + 1) + ":E" + (i + 1) + ""].FillColor = Color.Orange;
                            xldepWorkSheet.Range["A" + (i + 1) + ":E" + (i + 1) + ""].Font.Color = Color.White;
                            xldepWorkSheet.Range["A" + (i + 1) + ":E" + (i + 1) + ""].Font.FontStyle = SpreadsheetFontStyle.Bold;
                            break;
                        case "Software Development":
                            xldepWorkSheet.Range["A" + (i + 1) + ":E" + (i + 1) + ""].FillColor = Color.Gray;
                            xldepWorkSheet.Range["A" + (i + 1) + ":E" + (i + 1) + ""].Font.Color = Color.White;
                            xldepWorkSheet.Range["A" + (i + 1) + ":E" + (i + 1) + ""].Font.FontStyle = SpreadsheetFontStyle.Bold;

                            break;
                        case "Information Technology":
                            xldepWorkSheet.Range["A" + (i + 1) + ":E" + (i + 1) + ""].FillColor = Color.Blue;
                            xldepWorkSheet.Range["A" + (i + 1) + ":E" + (i + 1) + ""].Font.Color = Color.White;
                            xldepWorkSheet.Range["A" + (i + 1) + ":E" + (i + 1) + ""].Font.FontStyle = SpreadsheetFontStyle.Bold;
                            break;
                        case "Quality":
                            xldepWorkSheet.Range["A" + (i + 1) + ":E" + (i + 1) + ""].FillColor = Color.Yellow;
                            xldepWorkSheet.Range["A" + (i + 1) + ":E" + (i + 1) + ""].Font.Color = Color.Black;
                            xldepWorkSheet.Range["A" + (i + 1) + ":E" + (i + 1) + ""].Font.FontStyle = SpreadsheetFontStyle.Bold;
                            break;
                        default:
                            xldepWorkSheet.Range["A" + (i + 1) + ":E" + (i + 1) + ""].FillColor = Color.Gray;
                            break;
                    }

                }
            }
            //ATA add product otd to the sheet tabs 5/29/2017 [start]
            if (DOTconfigration.ProductsOTD.Count > 0)
            {

                Worksheet xldepWorkSheet = xlWorkBook.Worksheets["ProductsOTD OTD"];
                xldepWorkSheet.Range["A1:E1"].FillColor = Color.MediumPurple;
                xldepWorkSheet.Range["A1:E1"].Font.Color = Color.Black;
                xldepWorkSheet.Cells.Font.Size = 14;
                xldepWorkSheet.Cells[0, 0].Value = "Products Name";
                xldepWorkSheet.Cells[0, 1].Value = "OTD Creat date";
                //xlWorkSheet2.Cells[0, 2].Value = "Total Department Tasks (DOT) / Days";
                xldepWorkSheet.Cells[0, 2].Value = "# of OTD Tasks";
                xldepWorkSheet.Cells[0, 3].Value = "# of Tasks";
                //xlWorkSheet2.Cells[0, 5].Value = "Department AVG. (DOT)";
                xldepWorkSheet.Cells[0, 4].Value = "Tasks OTD %";
                for (int y = 0; y < DOTconfigration.ProductsOTD.Count; y++)
                {
                    int i = y + 1;
                    xldepWorkSheet.Cells[i, 0].Value = (DOTconfigration.ProductsOTD[y]).Product.Name;
                    xldepWorkSheet.Cells[i, 1].Value = (DOTconfigration.ProductsOTD[y]).CreateDate;
                    //xlWorkSheet2.Cells[i, 2].Value = (DOTconfigration.DepartmentsDOT[y]).Total;
                    xldepWorkSheet.Cells[i, 2].Value = (DOTconfigration.ProductsOTD[y]).NumberofOTDTasks;
                    xldepWorkSheet.Cells[i, 3].Value = (DOTconfigration.ProductsOTD[y]).NumberOfTasks;
                    //xlWorkSheet2.Cells[i, 5].Value = (DOTconfigration.DepartmentsDOT[y]).DOT;
                    xldepWorkSheet.Cells[i, 4].Value = (DOTconfigration.ProductsOTD[y]).OTD;
                }
            }
            //ATA add product otd to the sheet tabs 5/29/2017 [End]
            #region DOT Report
            if (DOTconfigration.Tickets.Count > 0)
            {
                xlWorkBook.Worksheets.Add("Tickets DOT");
                Worksheet xlWorkSheet = xlWorkBook.Worksheets["Tickets DOT"];
                xlWorkSheet.Range["A1:F1"].FillColor = Color.LightBlue;
                xlWorkSheet.Range["A1:F1"].Font.Color = Color.Black;
                xlWorkSheet.Cells[0, 0].Value = "Ticket Name";
                xlWorkSheet.Cells[0, 1].Value = "User Defiend field";
                xlWorkSheet.Cells[0, 2].Value = "Title";
                xlWorkSheet.Cells[0, 3].Value = "Account";
                xlWorkSheet.Cells[0, 4].Value = "Completed Date";
                xlWorkSheet.Cells[0, 5].Value = "Deliver On Time";
                for (int y = 0; y <= DOTconfigration.Tickets.Count; y++)
                {
                    int i = y + 1;
                    /// I represent the row number started with y = 0 that mean row number 2 
                    int index = 4;
                    if (y == DOTconfigration.Tickets.Count)
                    {
                        index++;
                        /// while y = ticket count that mean all tickets already added to the queue so we need to fill the footer
                        xlWorkSheet.Cells[y + 1, 0].Value = "Total Averages";
                        string cell1 = "F";
                        string from1 = "F" + 2;
                        string to1 = "F" + (y + 1);
                        //xlWorkSheet.Cells[i, "F"] = "=AVERAGE(F1,F2)";
                        xlWorkSheet.Cells[y + 1, index].Formula = "=AVERAGE(" + from1 + ":" + to1 + ")";
                        xlWorkSheet.Range["A" + (y + 2) + ":F" + (y + 2)].Fill.BackgroundColor = Color.Yellow;
                        xlWorkSheet.Range["A" + 2 + ":A" + (y + 1)].Fill.BackgroundColor = Color.LightGray;
                        // xlWorkSheet.Range["A" + 2 + ":A" + (y + 1)].Interior.Color = System.Drawing.ColorTranslator.ToOle(System.Drawing.Color.LightGray);
                    }
                    else
                    {
                        xlWorkSheet.Cells[i, 0].Value = DOTconfigration.Tickets[y].TiecketNum;
                        xlWorkSheet.Cells[i, 1].Value = ((Udfenum)DOTconfigration.Tickets[y].UDF).ToString();
                        xlWorkSheet.Cells[i, 2].Value = DOTconfigration.Tickets[y].Title;
                        if (DOTconfigration.Tickets[y].Account_profile != null)
                        {
                            xlWorkSheet.Cells[i, 3].Value = (DOTconfigration).Tickets[y].Account_profile.Name;
                        }
                        else
                        {
                            xlWorkSheet.Cells[i, 3].Value = DOTconfigration.Tickets[y].Account;
                        }
                        xlWorkSheet.Cells[i, 4].Value = DOTconfigration.Tickets[y].Completedate;
                        xlWorkSheet.Cells[i, 4].NumberFormat = "m/d/yyyy";
                        xlWorkSheet.Cells[i, 5].Value = DOTconfigration.Tickets[y].DeliveryOnTime;
                    }

                }

            }
            #endregion
            
            byte[] buffer = xlWorkBook.SaveDocument(DocumentFormat.Xlsx);
            
        return buffer;
        }

      //ATA fill that aht sheet with the selected configration data 
        public byte[] AHTattachement(AverageHandleTime configration, Session ObjectSpace)
        {
            ASPxSpreadsheet newspreadsheet = new ASPxSpreadsheet();        
            IWorkbook xlWorkBook = newspreadsheet.Document;
            if (configration.Tickets.Count > 0)
            {
              
                #region Departments AHT 
                xlWorkBook.Worksheets.Add("Departments AHT");
                Worksheet xldepWorkSheet = newspreadsheet.Document.Worksheets["Departments AHT"];
                xldepWorkSheet.Range["A1:B1"].FillColor = Color.MediumPurple;
                xldepWorkSheet.Range["A1:B1"].Font.Color = Color.Black;
                xldepWorkSheet.Cells.Font.Size = 14;
                xldepWorkSheet.Cells[0, 0].Value = "Dept./Que";
                xldepWorkSheet.Cells[0, 1].Value = "AHT";
                int itera = 1;
                foreach (DepartmentAHT DepAHT in configration.DepartmentsAHT)
                {
                    xldepWorkSheet[itera, 0].Value = DepAHT.Department.Id.ToString();
                    xldepWorkSheet[itera, 1].Value = DepAHT.Value;
                    itera++;
                    if (configration.QueuesValues.Where(x => x.Type.Department == DepAHT.Department).Count() > 0)
                    {
                        //xldepWorkSheet.Cells[itera, 0].Value = "Queue.";
                        //xldepWorkSheet.Cells[itera, 1].Value = "AHT";
                        switch (DepAHT.Department.Id.ToString())
                        {
                            case "Client Services":
                                xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].FillColor = Color.Purple;
                                xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].Font.Color = Color.White;
                                xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].Font.FontStyle = SpreadsheetFontStyle.Bold;
                                break;
                            case "Product":
                                xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].FillColor = Color.Orange;
                                xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].Font.Color = Color.White;
                                xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].Font.FontStyle = SpreadsheetFontStyle.Bold;
                                break;
                            case "Software Development":
                                xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].FillColor = Color.Gray;
                                xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].Font.Color = Color.White;
                                xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].Font.FontStyle = SpreadsheetFontStyle.Bold;

                                break;
                            case "Information Technology":
                                xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].FillColor = Color.Blue;
                                xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].Font.Color = Color.White;
                                xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].Font.FontStyle = SpreadsheetFontStyle.Bold;
                                break;
                            case "Finance":
                                xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].FillColor = Color.LightBlue;
                                xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].Font.Color = Color.Black;
                                xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].Font.FontStyle = SpreadsheetFontStyle.Bold;
                                break;
                            default:
                                xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].FillColor = Color.Gray;
                                break;
                        }
                    }
                    foreach (QueueValues queueval in configration.QueuesValues.Where(x => x.Type.Department == DepAHT.Department))
                    {

                        xldepWorkSheet[itera, 0].Value = queueval.Type.Name.ToString();
                        xldepWorkSheet[itera, 1].Value = queueval.Average;
                        itera++;
                    }

                    
                }
                //Range range = xldepWorkSheet.GetDataRange();
                //int lastcolumn = range.RightColumnIndex;
                //int lastrow = range.BottomRowIndex;
                //if (lastcolumn > 0)
                //    xldepWorkSheet.Columns.Remove(lastcolumn, 1048576);
                //if (lastrow > 0)
                //    xldepWorkSheet.Rows.Remove(lastrow, 1048576);
#endregion 
                #region Resources total AHT Tab 
                xlWorkBook.Worksheets.Add("Resources AHT");
                Worksheet xldepWorkSheetres = newspreadsheet.Document.Worksheets["Resources AHT"];
                xldepWorkSheetres.Range["A1:D1"].FillColor = Color.LightBlue;
                xldepWorkSheetres.Range["A1:D1"].Font.Color = Color.Black;
                xldepWorkSheetres.Cells.Font.Size = 14;
                xldepWorkSheetres.Cells[0, 0].Value = "Resource";
                xldepWorkSheetres.Cells[0, 1].Value = "Total Tickets Ages";
                xldepWorkSheetres.Cells[0, 2].Value = "# Tickets";
                xldepWorkSheetres.Cells[0, 3].Value = "AHT";
                int Rownum = 1;
                foreach (ResourcesAHT ResourceAHT in configration.ResourcesAHT)
                {
                    xldepWorkSheetres.Cells[Rownum, 0].Value = ResourceAHT.ResourceName.Name;
                    xldepWorkSheetres.Cells[Rownum, 1].Value = ResourceAHT.AHTForAllTicket.ToString();
                    xldepWorkSheetres.Cells[Rownum, 2].Value = ResourceAHT.NumberOfTickets.ToString();
                    xldepWorkSheetres.Cells[Rownum, 3].Value = ResourceAHT.AHTAvg.ToString();
                    Rownum++;
                }
                //Range range1 = xldepWorkSheetres.GetDataRange();
                //int lastcolumn1 = range1.RightColumnIndex;
                //int lastrow1 = range1.BottomRowIndex;
                //if (lastcolumn1 > 0)
                //    xldepWorkSheetres.Columns.Remove(lastcolumn1 + 1, 10000000);
                //if (lastrow1 > 0)
                //    xldepWorkSheetres.Rows.Remove(lastrow1, 1048576-lastrow1);
                #endregion 
                #region TicketResource AHT tab
                //ATA new Sheet to show the tickets with resources AHT 
                xlWorkBook.Worksheets.Add("Tickets Resources AHT(Duration)");
                Worksheet xlWorkSheet1 = newspreadsheet.Document.Worksheets["Tickets Resources AHT(Duration)"];
                System.Collections.ICollection Resources = configration.Session.GetObjects(configration.Session.Dictionary.GetClassInfo(typeof(Resources)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
                IList<Resources> allResources = Resources.Cast<Resources>().ToList();

                /// fill the header of the sheet and colored it 
                xlWorkSheet1.Range["A1:BB1"].FillColor = Color.LightBlue;
                xlWorkSheet1.Range["A1:BB1"].Font.Color = Color.Black;
                xlWorkSheet1.Cells[0, 0].Value = "Ticket Name";
                xlWorkSheet1.Cells[0, 1].Value = "Account";
                xlWorkSheet1.Cells[0, 2].Value = "Title";
                xlWorkSheet1.Cells[0, 3].Value = "PrimaryResource";
                xlWorkSheet1.Cells[0, 4].Value = "Completed Date";
                xlWorkSheet1.Cells[0, 5].Value = "Creation Date";
                xlWorkSheet1.Cells[0, 6].Value = "Category";
                xlWorkSheet1.Cells[0, 7].Value = "Status";
                xlWorkSheet1.Cells[0, 8].Value = "Ticket Age";
                for (int queueindex = 0; queueindex < allResources.Count; queueindex++)
                {
                    int columnnumber = queueindex + 9;
                    xlWorkSheet1.Cells[0, columnnumber].Value = allResources[queueindex].Name;
                }
                xlWorkSheet1.Cells[0, allResources.Count + 9].Value = "InActiveResource";

                for (int y = 0; y <= configration.Tickets.Count; y++)
                {
                    int i = y + 1;
                    /// I represent the row number started with y = 0 that mean row number 2 
                    if (y == configration.Tickets.Count)
                    {
                        /// while y = ticket count that mean all tickets already added to the queue so we need to fill the footer
                        xlWorkSheet1.Cells[y + 1, 0].Value = "Total Averages";
                        int index = 7;
                        /// loop on column and set the footer cell with the formula that calculate the averagefor this column 
                        for (char column = 'I'; column <= 'Z'; column++)
                        {
                            index++;
                            if (column == 'Z')
                            {
                                 string cell1 = char.ToString(column);
                                string from1 = char.ToString(column) + 2;
                                string to1 = char.ToString(column) + (y + 1);
                                //xlWorkSheet.Cells[i, "F"] = "=AVERAGE(F1,F2)";
                                xlWorkSheet1.Cells[y + 1, index].Formula = "=IFERROR(Round(AVERAGE(" + from1 + ":" + to1 + "),0),)";
                                char[] newcolumn = new char[2];
                                newcolumn[0] = 'A';
                                newcolumn[1] = 'A';
                                for (char onecolumn = newcolumn[1]; newcolumn[1] <= 'Z'; newcolumn[1]++)
                                {
                                    index++;
                                    string cell = char.ToString(newcolumn[0]) + char.ToString(newcolumn[1]);
                                    string from = cell + 2;
                                    string to = cell + (y + 1);
                                    //xlWorkSheet.Cells[i, "F"] = "=AVERAGE(F1,F2)";
                                    xlWorkSheet1.Cells[y + 1, index].Formula = "=IFERROR(Round(AVERAGE(" + from + ":" + to + "),0),)";
                                }
                            }
                            else
                            {
                                string cell1 = char.ToString(column);
                                string from1 = char.ToString(column) + 2;
                                string to1 = char.ToString(column) + (y + 1);
                                //xlWorkSheet.Cells[i, "F"] = "=AVERAGE(F1,F2)";
                                xlWorkSheet1.Cells[y + 1, index].Formula = "=IFERROR(Round(AVERAGE(" + from1 + ":" + to1 + "),0),)";
                            }

                            //"=AVERAGE('" + from + "','" + to + "')";
                        }
                        xlWorkSheet1.Range["A" + (y + 2) + ":AE" + (y + 2) + ""].FillColor = Color.Yellow;
                        xlWorkSheet1.Range["A" + 2 + ":A" + (y + 1)].FillColor = Color.LightGray;
                    }
                    /// if y is not equal tickets count that is mean not all tickets added so add this tickets 
                    else
                    {

                        xlWorkSheet1.Cells[i, 0].Value = configration.Tickets[y].TiecketNum;
                        if (configration.Tickets[y].Account_profile != null)
                        {
                            xlWorkSheet1.Cells[i, 1].Value = configration.Tickets[y].Account_profile.Name;
                        }
                        else
                        {
                            xlWorkSheet1.Cells[i, 1].Value = configration.Tickets[y].Account;
                        }
                        xlWorkSheet1.Cells[i, 2].Value = configration.Tickets[y].Title;
                        if (configration.Tickets[y].PrimaryResource != null)
                        {
                            xlWorkSheet1.Cells[i, 3].Value = configration.Tickets[y].PrimaryResource.Name;
                        }
                        xlWorkSheet1.Cells[i, 4].Value = configration.Tickets[y].Completedate;
                        xlWorkSheet1.Cells[i, 4].NumberFormat = "m/d/yyyy";
                        xlWorkSheet1.Cells[i, 5].Value = configration.Tickets[y].CreateDate;
                        xlWorkSheet1.Cells[i, 5].NumberFormat = "m/d/yyyy";
                        xlWorkSheet1.Cells[i, 6].Value = configration.Tickets[y].UDF.ToString();
                        foreach (Resources ResourceName in allResources)
                        {
                            ResourcesAHT value = configration.Tickets[y].ResourcesAHT.FirstOrDefault(x => x.ResourceName == ResourceName);
                            if (value != null)
                            {
                                int columnnumber = allResources.IndexOf(ResourceName) + 9;
                                xlWorkSheet1.Cells[i, columnnumber].Value = value.AHTAvg;
                            }
                        }
                        ResourcesAHT unknownres = configration.Tickets[y].ResourcesAHT.FirstOrDefault(x => x.ResourceName == null);
                        if (unknownres != null)
                        {

                            xlWorkSheet1.Cells[i, Resources.Count + 9].Value = unknownres.AHTAvg;
                        }
                        xlWorkSheet1.Cells[i, 8].Value = configration.Tickets[y].Ticketage;
                        xlWorkSheet1.Cells[i, 7].Value = configration.Tickets[y].Status.ToString();
                    }
                }


                //ATA calc user time entry


                //ATA new Sheet to show the tickets with resources AHT 
                xlWorkBook.Worksheets.Add("Tickets Resources Time Efforts");
                Worksheet xlWorkSheet2 = newspreadsheet.Document.Worksheets["Tickets Resources Time Efforts"];
                //System.Collections.ICollection Resources = configration.Session.GetObjects(configration.Session.Dictionary.GetClassInfo(typeof(Resources)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
                //IList<Resources> allResources = Resources.Cast<Resources>().ToList();

                /// fill the header of the sheet and colored it 
                xlWorkSheet2.Range["A1:BB1"].FillColor = Color.LightBlue;
                xlWorkSheet2.Range["A1:BB1"].Font.Color = Color.Black;
                xlWorkSheet2.Cells[0, 0].Value = "Ticket Name";
                xlWorkSheet2.Cells[0, 1].Value = "Account";
                xlWorkSheet2.Cells[0, 2].Value = "Title";
                xlWorkSheet2.Cells[0, 3].Value = "PrimaryResource";
                xlWorkSheet2.Cells[0, 4].Value = "Completed Date";
                xlWorkSheet2.Cells[0, 5].Value = "Creation Date";
                xlWorkSheet2.Cells[0, 6].Value = "Category";
                xlWorkSheet2.Cells[0, 7].Value = "Status";
                xlWorkSheet2.Cells[0, 8].Value = "Ticket Age";
                for (int queueindex = 0; queueindex < allResources.Count; queueindex++)
                {
                    int columnnumber = queueindex + 9;
                    xlWorkSheet2.Cells[0, columnnumber].Value = allResources[queueindex].Name;
                }
                // xlWorkSheet2.Cells[0, allResources.Count + 9].Value = "UnknownResource";

                for (int y = 0; y <= configration.Tickets.Count; y++)
                {
                    int i = y + 1;
                    /// I represent the row number started with y = 0 that mean row number 2 
                    if (y == configration.Tickets.Count)
                    {
                        /// while y = ticket count that mean all tickets already added to the queue so we need to fill the footer
                        xlWorkSheet2.Cells[y + 1, 0].Value = "Total Averages";
                        int index = 7;
                        /// loop on column and set the footer cell with the formula that calculate the averagefor this column 
                        for (char column = 'I'; column <= 'Z'; column++)
                        {
                            index++;
                            if (column == 'Z')
                            {
                                char[] newcolumn = new char[2];
                                newcolumn[0] = 'A';
                                newcolumn[1] = 'A';
                                for (char onecolumn = newcolumn[1]; newcolumn[1] <= 'Z'; newcolumn[1]++)
                                {
                                    index++;
                                    string cell = char.ToString(newcolumn[0]) + char.ToString(newcolumn[1]);
                                    string from = cell + 2;
                                    string to = cell + (y + 1);
                                    //xlWorkSheet.Cells[i, "F"] = "=AVERAGE(F1,F2)";
                                    xlWorkSheet2.Cells[y + 1, index].Formula = "=IFERROR(Round(AVERAGE(" + from + ":" + to + "),0),)";
                                }
                            }
                            else
                            {
                                string cell1 = char.ToString(column);
                                string from1 = char.ToString(column) + 2;
                                string to1 = char.ToString(column) + (y + 1);
                                //xlWorkSheet.Cells[i, "F"] = "=AVERAGE(F1,F2)";
                                xlWorkSheet2.Cells[y + 1, index].Formula = "=IFERROR(Round(AVERAGE(" + from1 + ":" + to1 + "),0),)";
                            }

                            //"=AVERAGE('" + from + "','" + to + "')";
                        }
                        xlWorkSheet2.Range["A" + (y + 2) + ":BB" + (y + 2) + ""].FillColor = Color.Yellow;
                        xlWorkSheet2.Range["A" + 2 + ":A" + (y + 1)].FillColor = Color.LightGray;
                    }
                    /// if y is not equal tickets count that is mean not all tickets added so add this tickets 
                    else
                    {

                        xlWorkSheet2.Cells[i, 0].Value = configration.Tickets[y].TiecketNum;
                        if (configration.Tickets[y].Account_profile != null)
                        {
                            xlWorkSheet2.Cells[i, 1].Value = configration.Tickets[y].Account_profile.Name;
                        }
                        else
                        {
                            xlWorkSheet2.Cells[i, 1].Value = configration.Tickets[y].Account;
                        }
                        xlWorkSheet2.Cells[i, 2].Value = configration.Tickets[y].Title;
                        if (configration.Tickets[y].PrimaryResource != null)
                        {
                            xlWorkSheet2.Cells[i, 3].Value = configration.Tickets[y].PrimaryResource.Name;
                        }
                        xlWorkSheet2.Cells[i, 4].Value = configration.Tickets[y].Completedate;
                        xlWorkSheet2.Cells[i, 4].NumberFormat = "m/d/yyyy";
                        xlWorkSheet2.Cells[i, 5].Value = configration.Tickets[y].CreateDate;
                        xlWorkSheet2.Cells[i, 5].NumberFormat = "m/d/yyyy";
                        xlWorkSheet2.Cells[i, 6].Value = configration.Tickets[y].UDF.ToString();

                        //ResourcesAHT value = configration.Tickets[y].ResourcesAHT.FirstOrDefault(x => x.ResourceName == ResourceName);
                        //if (value != null)
                        //{
                        //    int columnnumber = allResources.IndexOf(ResourceName) + 9;
                        //    xlWorkSheet1.Cells[i, columnnumber].Value = value.AHTAvg;
                        //}
                        if (configration.Tickets[y].ID != 0)
                        {
                            StringBuilder SelecttimeEntry = new StringBuilder();
                            SelecttimeEntry.Append("<queryxml><entity>TimeEntry</entity>").Append(System.Environment.NewLine);
                            SelecttimeEntry.Append("<query><condition><field>TicketID<expression op=\"Equals\">" + configration.Tickets[y].ID + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                            // SelecttimeEntry.Append("<condition><field>TicketID<expression op=\"Equals\">" + configration.Tickets[y].Account + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                            SelecttimeEntry.Append("</queryxml>").Append(System.Environment.NewLine);
                            var TimeEntries = clientAuto.query(at_integrations, SelecttimeEntry.ToString());
                            if (TimeEntries.ReturnCode == 1)
                            {
                                foreach (Resources ResourceName in allResources)
                                {
                                    if (ResourceName.AutoTaskID != null)
                                    {
                                        long ResourceTime = 0;
                                        foreach (TimeEntry TimeEntry in TimeEntries.EntityResults)
                                        {
                                            if (TimeEntry.ResourceID.ToString() == ResourceName.AutoTaskID.ToString())
                                            {
                                                ResourceTime += long.Parse(Regex.Match(TimeEntry.HoursWorked.ToString(), @"\d+").Value);
                                                // += int.Parse(Regex.Match(TimeEntry.HoursWorked.ToString(), @"\d+").Value);
                                            }
                                        }
                                        int columnnumber = allResources.IndexOf(ResourceName) + 9;
                                        if (ResourceTime > 0)
                                            xlWorkSheet2.Cells[i, columnnumber].Value = ResourceTime;
                                    }


                                }

                            }
                        }
                        
                        //ResourcesAHT unknownres = configration.Tickets[y].ResourcesAHT.FirstOrDefault(x => x.ResourceName == null);
                        //if (unknownres != null)
                        //{

                        //    xlWorkSheet1.Cells[i, Resources.Count + 9].Value = unknownres.AHTAvg;
                        //}
                        //xlWorkSheet1.Cells[i, 8].Value = configration.Tickets[y].Ticketage;
                        //xlWorkSheet1.Cells[i, 7].Value = configration.Tickets[y].Status.ToString();
                    }
                }
                #endregion 
                #region Tickets AHT tab sheet
                xlWorkBook.Worksheets.Add("Tickets AHT");
                xlWorkBook.Worksheets["sheet1"].Visible = false;
                Worksheet xlWorkSheet = newspreadsheet.Document.Worksheets["Tickets AHT"];
                System.Collections.ICollection Queues = configration.Session.GetObjects(configration.Session.Dictionary.GetClassInfo(typeof(QueueName)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
                IList<QueueName> allqueues = Queues.Cast<QueueName>().ToList();

                /// fill the header of the sheet and colored it 
                xlWorkSheet.Range["A1:AE1"].FillColor = Color.LightBlue;
                xlWorkSheet.Range["A1:AE1"].Font.Color = Color.Black;
                xlWorkSheet.Cells[0, 0].Value = "Ticket Name";
                xlWorkSheet.Cells[0, 1].Value = "Account";
                xlWorkSheet.Cells[0, 2].Value = "Title";
                xlWorkSheet.Cells[0, 3].Value = "PrimaryResource";
                xlWorkSheet.Cells[0, 4].Value = "Completed Date";
                xlWorkSheet.Cells[0, 5].Value = "Creation Date";
                xlWorkSheet.Cells[0, 6].Value = "Category";
                xlWorkSheet.Cells[0, 7].Value = "Status";
                xlWorkSheet.Cells[0, 8].Value = "Ticket Age";
                for (int queueindex = 0; queueindex < allqueues.Count; queueindex++)
                {
                    int columnnumber = queueindex + 9;
                    xlWorkSheet.Cells[0, columnnumber].Value = allqueues[queueindex].Name;
                }


                for (int y = 0; y <= configration.Tickets.Count; y++)
                {
                    int i = y + 1;
                    /// I represent the row number started with y = 0 that mean row number 2 
                    if (y == configration.Tickets.Count)
                    {
                        /// while y = ticket count that mean all tickets already added to the queue so we need to fill the footer
                        xlWorkSheet.Cells[y + 1, 0].Value = "Total Averages";
                        int index = 7;
                        /// loop on column and set the footer cell with the formula that calculate the averagefor this column 
                        for (char column = 'I'; column <= 'Z'; column++)
                        {
                            index++;
                            if (column == 'Z')
                            {
                                string cell1 = char.ToString(column);
                                string from1 = char.ToString(column) + 2;
                                string to1 = char.ToString(column) + (y + 1);
                                //xlWorkSheet.Cells[i, "F"] = "=AVERAGE(F1,F2)";
                                xlWorkSheet.Cells[y + 1, index].Formula = "=IFERROR(Round(AVERAGE(" + from1 + ":" + to1 + "),0),)";
                                char[] newcolumn = new char[2];
                                newcolumn[0] = 'A';
                                newcolumn[1] = 'A';
                                for (char onecolumn = newcolumn[1]; newcolumn[1] <= 'Z'; newcolumn[1]++)
                                {
                                    index++;
                                    string cell = char.ToString(newcolumn[0]) + char.ToString(newcolumn[1]);
                                    string from = cell + 2;
                                    string to = cell + (y + 1);
                                    //xlWorkSheet.Cells[i, "F"] = "=AVERAGE(F1,F2)";
                                    xlWorkSheet.Cells[y + 1, index].Formula = "=IFERROR(Round(AVERAGE(" + from + ":" + to + "),0),)";
                                }
                            }
                            else
                            {
                                string cell1 = char.ToString(column);
                                string from1 = char.ToString(column) + 2;
                                string to1 = char.ToString(column) + (y + 1);
                                //xlWorkSheet.Cells[i, "F"] = "=AVERAGE(F1,F2)";
                                xlWorkSheet.Cells[y + 1, index].Formula = "=IFERROR(Round(AVERAGE(" + from1 + ":" + to1 + "),0),)";
                            }

                            //"=AVERAGE('" + from + "','" + to + "')";
                        }
                        xlWorkSheet.Range["A" + (y + 2) + ":AZ" + (y + 2) + ""].FillColor = Color.Yellow;
                        xlWorkSheet.Range["A" + 2 + ":A" + (y + 1)].FillColor = Color.LightGray;
                    }
                    /// if y is not equal tickets count that is mean not all tickets added so add this tickets 
                    else
                    {

                        xlWorkSheet.Cells[i, 0].Value = configration.Tickets[y].TiecketNum;
                        if (configration.Tickets[y].Account_profile != null)
                        {
                            xlWorkSheet.Cells[i, 1].Value = configration.Tickets[y].Account_profile.Name;
                        }
                        else
                        {
                            xlWorkSheet.Cells[i, 1].Value = configration.Tickets[y].Account;
                        }
                        xlWorkSheet.Cells[i, 2].Value = configration.Tickets[y].Title;
                        if (configration.Tickets[y].PrimaryResource != null)
                        {
                            xlWorkSheet.Cells[i, 3].Value = configration.Tickets[y].PrimaryResource.Name;
                        }
                        xlWorkSheet.Cells[i, 4].Value = configration.Tickets[y].Completedate;
                        xlWorkSheet.Cells[i, 4].NumberFormat = "m/d/yyyy";
                        xlWorkSheet.Cells[i, 5].Value = configration.Tickets[y].CreateDate;
                        xlWorkSheet.Cells[i, 5].NumberFormat = "m/d/yyyy";
                        xlWorkSheet.Cells[i, 6].Value = configration.Tickets[y].UDF.ToString();
                        foreach (QueueName queuename in allqueues)
                        {
                            QueueValues value = configration.Tickets[y].QueuesValues.FirstOrDefault(x => x.Type == queuename);
                            if (value != null)
                            {
                                int columnnumber = allqueues.IndexOf(queuename) + 9;
                                xlWorkSheet.Cells[i, columnnumber].Value = value.Value;
                            }
                        }
                        xlWorkSheet.Cells[i, 8].Value = configration.Tickets[y].Ticketage;
                        xlWorkSheet.Cells[i, 7].Value = configration.Tickets[y].Status.ToString();
                    }
                }
                #endregion 
            }
            byte[] buffer = xlWorkBook.SaveDocument(DocumentFormat.Xlsx);
            return buffer;
        }
        #region AHT calc
        //ATA [strat]
        /// <summary>
        /// generate Average Handling time with this date range and assigned to it all ticke AHT and calculate queues and departments AHT and assigned to it 
        /// </summary>
        /// <param name="datefrom"></param>
        /// <param name="dateto"></param>
        /// <param name="objectspace"></param>
        /// ATA modify the function paramter to take 4 parameter to take AverageHandlinkTime object or null 1/19/2017 [Start]
        public void generateAHT(DateTime datefrom, DateTime dateto, AverageHandleTime recivedReport, Session objectspace)
        {
            //ATA verfiy that this function start work [start]
            if (recivedReport == null)
            {
                Email VerfiyEmail = new Email();
                VerfiyEmail.FromEmail = "qua@ariasystems.biz";
                VerfiyEmail.EmailPassword = "quality_123";
                VerfiyEmail.ToEmail = "ahmed.r@ariany.com";
                VerfiyEmail.EmailTitle = "OTD&AHTCalculationReportVerfication";
                VerfiyEmail.EmailBody = "Generate AHT Function start work";
                VerfiyEmail.SendEmail();
            }

            //ATA verfiy that this function start work [End]
            //ATA checkthat if the configration is empty that mean the webjob which call the function so we creat it
            AverageHandleTime configration = null;
            if (recivedReport == null)
            {
                configration = new AverageHandleTime(objectspace);
                configration.DateFrom = datefrom;
                configration.DateTO = dateto;
              configration.Title = "Aria AHT KPI " + datefrom.ToString("MMMM") + " " + datefrom.Year;
               //configration.Title = "Aria AHT KPI " + datefrom.ToString("MMMM") + " " + datefrom.Year +"testing the result22";
                // configration.Title = "Test again New Tickets AHT Sheet";
                configration.Save();
            }
            else
            {
                configration = recivedReport;
            }
            getAHTdata(datefrom, dateto, objectspace);
            calculateAHTForTickets(configration, objectspace);
            //ATA calc AHT for opened tickets  1/16/2017
            calculateAHTFornotcompletedTickets(configration, objectspace);
            calculateQueueAHT(configration, objectspace);
            calculateDepartmentAHT(configration, objectspace);
            calculateResourceAHT(configration, objectspace);
            if (recivedReport == null)
            {
                Email newone = new Email();
                Stream attachemntasasstream = new MemoryStream(AHTattachement(configration, objectspace));
                System.Net.Mail.Attachment attach = new System.Net.Mail.Attachment(attachemntasasstream, configration.Title.ToString() + ".xlsx");
                newone.Attachement = attach;
                newone.FromEmail = "qua@ariasystems.biz";
                newone.EmailPassword = "quality_123";
                newone.EmailBody = "Dear All,<br /><br /> Please find the attached Excel file for the Tickets AHT of " + datefrom.ToString("MMMM") + " KPI " + datefrom.Year;
               // newone.ToEmail = "ahmed.t@ariany.com";
                newone.ToEmail = "Ahmed.r@ariasystems.biz,ahmed.r@ariany.com,sara.a@ariany.com";//,Wael.A@ariasystems.biz,amr.h@ariasystems.biz,Khaled.m@ariasystems.biz,Ahmed.f@ariasystems.biz";//,ahmed.r@ariasystems.biz";
                newone.EmailTitle = "Tickets AHT KPI " + datefrom.ToString("MMMM") + " " + datefrom.Year;
                newone.EmailSubject = "Tickets AHT KPI " + datefrom.ToString("MMMM") + " " + datefrom.Year;
                // newone.EmailCC = "omar.r@ariasystems.biz";//"omar.r@ariasystems.biz"
               newone.SendEmail();
            }

        }
        //ATA [End]

        //ATA get all tickets from auto task that completed within Configration start end time 
        // and if it already calculated assigned it to the configration if not calculate it and assign it to configration 
        //[Start]
        public void calculateAHTForTickets(AverageHandleTime Configration, Session Objectspace)
        {
            Dictionary<string, object[]> Allqueuesdic = new Dictionary<string, object[]>();
            System.Collections.ICollection queuesName = Objectspace.GetObjects(Objectspace.Dictionary.GetClassInfo(typeof(QueueName)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
            IList<QueueName> allqueues = queuesName.Cast<QueueName>().ToList();
            foreach (var queuname in allqueues)
            {
                Allqueuesdic.Add(((QueueName)queuname).Name, new object[6]);
            }

            //ATA Add resource AHT for the aht calc 12/6/2016
            Dictionary<string, object[]> Allresoudic = new Dictionary<string, object[]>();
            System.Collections.ICollection Resources = Objectspace.GetObjects(Objectspace.Dictionary.GetClassInfo(typeof(Resources)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
            IList<Resources> allresources = Resources.Cast<Resources>().ToList();
            foreach (var Resourcename in allresources)
            {
                if (((Resources)Resourcename).AutoTaskName != null)
                {
                    Allresoudic.Add(((Resources)Resourcename).AutoTaskName, new object[6]);
                }
            }
            //ATA Add resource AHT for the aht calc 12/6/2016
            StringBuilder Selecttiecket = new StringBuilder();
            Selecttiecket.Append("<queryxml><entity>Ticket</entity>").Append(System.Environment.NewLine);
            Selecttiecket.Append("<query><condition><field>CompletedDate<expression op=\"greaterthan\">" + Configration.DateFrom + "</expression></field></condition>").Append(System.Environment.NewLine);
            Selecttiecket.Append("<condition><field>CompletedDate<expression op=\"LessThan\">" + Configration.DateTO + "</expression></field></condition></query>").Append(System.Environment.NewLine);
            Selecttiecket.Append("</queryxml>").Append(System.Environment.NewLine);
            var tieckets = clientAuto.query(at_integrations, Selecttiecket.ToString());
            if (tieckets.ReturnCode == 1)
            {

                foreach (Ticket ticket in tieckets.EntityResults)
                {
                    //check if this ticket have user defined field or not if it have we can calculate it's AHT if not we needn't to calculate it 
                    //if (ticket.UserDefinedFields.Count() > 0 && ticket.UserDefinedFields[8].Value != null)
                   // {
                        if (((Ticket)ticket).TicketNumber != null)
                        {
                            TicketAHT existticket = Objectspace.FindObject<TicketAHT>(CriteriaOperator.Parse("[TiecketNum] = '" + ((Ticket)ticket).TicketNumber + "'"));
                            if (existticket != null && existticket.Ticketage == Math.Round(DateTime.Parse(ticket.CompletedDate.ToString()).Date.Subtract(DateTime.Parse(ticket.CreateDate.ToString()).Date).TotalDays, 0))
                            {
                                //CreateAHTlist(array, AHT, existticket, newobject);
                                Configration.Tickets.Add(existticket);
                                continue;
                            }
                            else
                            {
                                
                               TicketAHT AHT_for_ticket = CalculateAHT(Objectspace, ticket, Allqueuesdic, allqueues, Allresoudic);//Queues_without_pmo_dic, Queues_with_pmo_dic);
                               // TicketAHT AHT_for_ticket = CalculateAHT(Objectspace, ticket, Allqueuesdic, allqueues, Allresoudic);//Queues_without_pmo_dic, Queues_with_pmo_dic);
                                if (AHT_for_ticket != null)
                                {
                                    Configration.Tickets.Add(AHT_for_ticket);
                                }

                            }
                        }

                   // }
                }
            }
            Configration.Save();
        }
        //[End]

        //ATA try to calculate AHT for not completed tickets [Start]

        public void calculateAHTFornotcompletedTickets(AverageHandleTime Configration, Session Objectspace)
        {
            Dictionary<string, object[]> Allqueuesdic = new Dictionary<string, object[]>();
            System.Collections.ICollection queuesName = Objectspace.GetObjects(Objectspace.Dictionary.GetClassInfo(typeof(QueueName)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
            IList<QueueName> allqueues = queuesName.Cast<QueueName>().ToList();
            foreach (var queuname in allqueues)
            {
                Allqueuesdic.Add(((QueueName)queuname).Name, new object[6]);
            }

            //ATA Add resource AHT for the aht calc 12/6/2016
            Dictionary<string, object[]> Allresoudic = new Dictionary<string, object[]>();
            System.Collections.ICollection Resources = Objectspace.GetObjects(Objectspace.Dictionary.GetClassInfo(typeof(Resources)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
            IList<Resources> allresources = Resources.Cast<Resources>().ToList();
            foreach (var Resourcename in allresources)
            {
                if (((Resources)Resourcename).AutoTaskName != null)
                {
                    Allresoudic.Add(((Resources)Resourcename).AutoTaskName, new object[6]);
                }
            }
            //ATA Add resource AHT for the aht calc 12/6/2016
            StringBuilder Selecttiecket = new StringBuilder();
            Selecttiecket.Append("<queryxml><entity>Ticket</entity>").Append(System.Environment.NewLine);
            Selecttiecket.Append("<query><condition><field>Status<expression op=\"NotEqual\">" + 5 + "</expression></field></condition>").Append(System.Environment.NewLine);
            Selecttiecket.Append("<condition><field>Status<expression op=\"NotEqual\">" + 14 + "</expression></field></condition></query>").Append(System.Environment.NewLine);
            Selecttiecket.Append("</queryxml>").Append(System.Environment.NewLine);
            var tieckets = clientAuto.query(at_integrations, Selecttiecket.ToString());
            if (tieckets.ReturnCode == 1)
            {

                foreach (Ticket ticket in tieckets.EntityResults)
                {
                    //check if this ticket have user defined field or not if it have we can calculate it's AHT if not we needn't to calculate it 
                    // if (ticket.UserDefinedFields.Count() > 0 && ticket.UserDefinedFields[8].Value != null)
                    // {
                    if (((Ticket)ticket).TicketNumber != null)
                    {
                        TicketAHT existticket = Objectspace.FindObject<TicketAHT>(CriteriaOperator.Parse("[TiecketNum] = '" + ((Ticket)ticket).TicketNumber + "'"));
                        if (existticket != null)
                        {
                            if (existticket.Status == TicketStatus.Complete || existticket.Ticketage != Math.Round(DateTime.Now.Date.Subtract(DateTime.Parse(((Ticket)ticket).CreateDate.ToString()).Date).TotalDays, 0))
                            {
                                existticket.Delete();
                                TicketAHT AHT_for_ticket = CalculateAHT(Objectspace, ticket, Allqueuesdic, allqueues, Allresoudic);//Queues_without_pmo_dic, Queues_with_pmo_dic);
                                if (AHT_for_ticket != null)
                                {
                                    Configration.Tickets.Add(AHT_for_ticket);
                                }
                            }
                            else
                            {
                                Configration.Tickets.Add(existticket);
                                continue;
                            }

                        }
                        else
                        {
                            TicketAHT AHT_for_ticket = CalculateAHT(Objectspace, ticket, Allqueuesdic, allqueues, Allresoudic);//Queues_without_pmo_dic, Queues_with_pmo_dic);
                            if (AHT_for_ticket != null)
                            {
                                Configration.Tickets.Add(AHT_for_ticket);
                            }

                        }
                    }
                    else
                    {

                    }

                    // }
                }
            }
            Configration.Save();
        }
        /// <summary>
        /// Calculate AHT for each queue name in the system for specific period 
        /// </summary>
        /// <param name="AHTList"></param>
        /// <param name="objectspace"></param>
        public void calculateQueueAHT(AverageHandleTime configration, Session objectspace)
        {
            System.Collections.ICollection Queues = configration.Session.GetObjects(configration.Session.Dictionary.GetClassInfo(typeof(QueueName)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
            IList<QueueName> allqueues = Queues.Cast<QueueName>().ToList();
            AverageHandleTime Averagehandlingtime = objectspace.FindObject<AverageHandleTime>(CriteriaOperator.Parse("[Oid] = '" + configration.Oid + "'"));
            Dictionary<QueueName, int[]> queuedic = new Dictionary<QueueName, int[]>();
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
                QueueValues queuvalue = new QueueValues(objectspace);
                if (queuedic[key][0] > 0)
                {
                    queuvalue.Type = key;
                    queuvalue.Value = Convert.ToDouble(queuedic[key][0]);
                    queuvalue.NumberOfTickets = queuedic[key][1];
                    queuvalue.Average = Math.Round(queuvalue.Value / queuvalue.NumberOfTickets, 0);
                    queuvalue.AverageHandleTime = Averagehandlingtime;
                    queuvalue.Save();
                }

            }

        }


        /// <summary>
        /// Calculate AHT for each department 
        /// </summary>
        /// <param name="datefrom"></param>
        /// <param name="dateto"></param>
        /// <param name="objectspace"></param>
        public void calculateDepartmentAHT(AverageHandleTime configration, Session objectspace)
        {
            System.Collections.ICollection Queues = objectspace.GetObjects(objectspace.Dictionary.GetClassInfo(typeof(QueueName)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
            IList<QueueName> allqueues = Queues.Cast<QueueName>().ToList();
            // IList<TicketAHT> ticketslist = objectspace.GetObjects<TicketAHT>(CriteriaOperator.Parse("[comletedate] >= '" + datefrom + "'and [comletedate] <= '" + dateto + "'"));
            System.Collections.ICollection alldepartments = objectspace.GetObjects(objectspace.Dictionary.GetClassInfo(typeof(Aria5SystemAdmin.Module.BusinessObjects.Department)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
            IList<Aria5SystemAdmin.Module.BusinessObjects.Department> departments = alldepartments.Cast<Aria5SystemAdmin.Module.BusinessObjects.Department>().ToList();
            AverageHandleTime configration_1 = objectspace.FindObject<AverageHandleTime>(CriteriaOperator.Parse("[Oid] = '" + configration.Oid + "'"));
            // Dictionary<QueueName, int> queuedic = new Dictionary<QueueName, int>();
            foreach (Aria5SystemAdmin.Module.BusinessObjects.Department Dep in departments)
            {
                int queuevalues = 0;
                int queuetickets = 0;
                foreach (QueueValues value in configration_1.QueuesValues.Where(x => x.Type.Department == Dep))
                {
                    queuevalues += (int)value.Value;
                    queuetickets += value.NumberOfTickets;
                }
                if (queuetickets > 0)
                {
                    Aria5SystemAdmin.Module.BusinessObjects.DepartmentAHT depAHT = new DepartmentAHT(objectspace);
                    depAHT.Department = Dep;
                    depAHT.Value = (int)Math.Round((double)queuevalues / queuetickets, 0);
                    depAHT.AverageHandleTime = configration_1;
                    depAHT.Save();
                    // depAHT.Session.CommitTransaction();
                }
            }

        }
        /// <summary>
        /// calculate aht for each ticket primary resource 
        /// </summary>
        /// <param name="configration"></param>
        /// <param name="objectspace"></param>
        public void calculateResourceAHT(AverageHandleTime configration, Session objectspace)
        {
            System.Collections.ICollection Resources = objectspace.GetObjects(objectspace.Dictionary.GetClassInfo(typeof(Resources)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
            IList<Resources> allqueues = Resources.Cast<Resources>().ToList();
            // IList<TicketAHT> ticketslist = objectspace.GetObjects<TicketAHT>(CriteriaOperator.Parse("[comletedate] >= '" + datefrom + "'and [comletedate] <= '" + dateto + "'"));
            AverageHandleTime configration_1 = objectspace.FindObject<AverageHandleTime>(CriteriaOperator.Parse("[Oid] = '" + configration.Oid + "'"));
            // Dictionary<QueueName, int> queuedic = new Dictionary<QueueName, int>();
            Dictionary<Resources, int[]> Resdic = new Dictionary<Resources, int[]>();
            configration_1.Session.LockingOption = LockingOption.None;

            foreach (Resources Res in Resources)
            {
                Resdic.Add(Res, new int[2]);
            }
            foreach (TicketAHT ticket in configration_1.Tickets)
            {
                foreach (ResourcesAHT TicketResAHT in ticket.ResourcesAHT)
                {
                    if (TicketResAHT.ResourceName != null)
                    {
                        Resdic[TicketResAHT.ResourceName][0] += (int)TicketResAHT.AHTAvg;
                        Resdic[TicketResAHT.ResourceName][1]++;
                    }

                }
            }
            foreach (Resources Resource in Resdic.Keys)
            {
                if (Resdic[Resource][0] > 0)
                {
                    ResourcesAHT ResourcenewAHT = new ResourcesAHT(objectspace);
                    ResourcenewAHT.ResourceName = Resource;
                    ResourcenewAHT.AHTForAllTicket = Resdic[Resource][0];
                    ResourcenewAHT.NumberOfTickets = Resdic[Resource][1];
                    ResourcenewAHT.AHTAvg = Math.Round((double)Resdic[Resource][0] / Resdic[Resource][1], 2);
                    ResourcenewAHT.AvgAHTreport = configration_1;
                    ResourcenewAHT.Save();
                }
            }

        }
        public void getAHTdata(DateTime datefrom, DateTime dateto, Session Objectspace)
        {
            //ATA Add resource AHT for the aht calc 12/6/2016[start]
            CheckresourcesSync(Objectspace);
            //ATA Add resource AHT for the aht calc 12/6/2016[End]

            Dictionary<string, object[]> Allqueuesdic = new Dictionary<string, object[]>();
            System.Collections.ICollection queuesName = Objectspace.GetObjects(Objectspace.Dictionary.GetClassInfo(typeof(QueueName)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
            IList<QueueName> allqueues = queuesName.Cast<QueueName>().ToList();
            foreach (var queuname in allqueues)
            {
                Allqueuesdic.Add(((QueueName)queuname).Name, new object[6]);
            }
            //ATA Add resource AHT for the aht calc 12/6/2016
            Dictionary<string, object[]> Allresoudic = new Dictionary<string, object[]>();
            System.Collections.ICollection Resources = Objectspace.GetObjects(Objectspace.Dictionary.GetClassInfo(typeof(Resources)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
            IList<Resources> allresources = Resources.Cast<Resources>().ToList();
            foreach (var Resourcename in allresources)
            {
                if (((Resources)Resourcename).AutoTaskName != null)
                {
                    Allresoudic.Add(((Resources)Resourcename).AutoTaskName, new object[6]);
                }
                
            }
            StringBuilder Selecttiecket = new StringBuilder();
            Selecttiecket.Append("<queryxml><entity>Ticket</entity>").Append(System.Environment.NewLine);
            Selecttiecket.Append("<query><condition><field>CompletedDate<expression op=\"greaterthan\">" + datefrom + "</expression></field></condition>").Append(System.Environment.NewLine);
            Selecttiecket.Append("<condition><field>CompletedDate<expression op=\"LessThan\">" + dateto + "</expression></field></condition></query>").Append(System.Environment.NewLine);
            Selecttiecket.Append("</queryxml>").Append(System.Environment.NewLine);
            var tieckets = clientAuto.query(at_integrations, Selecttiecket.ToString());
            if (tieckets.ReturnCode == 1)
             {

                foreach (Ticket ticket in tieckets.EntityResults)
                {
                    //check if this ticket have user defined field or not if it have we can calculate it's AHT if not we needn't to calculate it 
                    //if (ticket.UserDefinedFields.Count() > 0 && ticket.UserDefinedFields[8].Value != null)
                    //{
                        if (((Ticket)ticket).TicketNumber != null)
                        {
                            TicketAHT existticket = Objectspace.FindObject<TicketAHT>(CriteriaOperator.Parse("[TiecketNum] = '" + ((Ticket)ticket).TicketNumber + "'"));
                            if (existticket != null)
                            {
                                if ((existticket.Ticketage == Math.Round(DateTime.Parse(ticket.CompletedDate.ToString()).Date.Subtract(DateTime.Parse(ticket.CreateDate.ToString()).Date).TotalDays, 0)) || (existticket.PrimaryResource != null && ticket.AssignedResourceID!=null && existticket.PrimaryResource.AutoTaskID != ticket.AssignedResourceID.ToString()) || existticket.PrimaryResource == null)
                                {
                                    //CreateAHTlist(array, AHT, existticket, newobject);
                                    if (existticket.Status != TicketStatus.Complete)
                                    {
                                        existticket.Completedate = DateTime.Parse(ticket.CompletedDate.ToString()).Date;
                                        existticket.Status = TicketStatus.Complete;
                                        existticket.Ticketage = Math.Round(DateTime.Parse(ticket.CompletedDate.ToString()).Date.Subtract(DateTime.Parse(ticket.CreateDate.ToString()).Date).TotalDays, 0);
                                        existticket.Save();
                                    }
                                    if (existticket.CreateDate == DateTime.MinValue || existticket.PrimaryResource == null) 
                                    {
                                        existticket.CreateDate = DateTime.Parse(ticket.CreateDate.ToString());
                                        Resources ticketresource = Objectspace.FindObject<Resources>(CriteriaOperator.Parse("[AutoTaskID] = '" + ticket.AssignedResourceID + "'"));
                                        if (ticketresource != null)
                                        {
                                            existticket.PrimaryResource = ticketresource;
                                        }
                                        existticket.Save();
                                    }
                                    continue;
                                }else
                                {
                                    existticket.Delete();
                                    TicketAHT AHT_for_ticket = CalculateAHT(Objectspace, ticket, Allqueuesdic, allqueues, Allresoudic);//Queues_without_pmo_dic, Queues_with_pmo_dic);

                                }
                            }
                            else
                            {
                               // TicketAHT AHT_for_ticket = CalculateAHT(Objectspace, ticket, Allqueuesdic, allqueues,Allresoudic);//Queues_without_pmo_dic, Queues_with_pmo_dic);
                                TicketAHT AHT_for_ticket = CalculateAHT(Objectspace, ticket, Allqueuesdic, allqueues, Allresoudic);//Queues_without_pmo_dic, Queues_with_pmo_dic);
                                // TicketAHT AHT_for_ticket = CalculateAHT(Objectspace, ticket, Allqueuesdic, allqueues, Allresoudic);//Queues_without_pmo_dic, Queues_with_pmo_dic);
                               
                            }
                        }
                        else
                        {

                        }

                   // }
                }
            }
        }

        public void CheckresourcesSync(Session Objectspace)
        {
            #region Commentedarea
            //StringBuilder Dep = new StringBuilder();
            //Dep.Append("<queryxml><entity>Department</entity>").Append(System.Environment.NewLine);
            ////ATA add oneresource instead of this
            //Dep.Append("<query><condition><field>Id<expression op=\"IsNotNull\"></expression></field></condition></query>").Append(System.Environment.NewLine);
            //Dep.Append("</queryxml>").Append(System.Environment.NewLine);

            //var r12 = clientAuto.query(at_integrations, Dep.ToString());
            //if (r12.ReturnCode == 1)
            //{
            //    if (r12.EntityResults.Length > 0)
            //    {
            //        foreach (Aria5SystemAdmin.Module.SubAutoTask1.Department dep in r12.EntityResults)
            //        {
            //            if (dep.id != null)
            //            {
            //                Aria5SystemAdmin.Module.BusinessObjects.Department Department = Objectspace.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Department>(CriteriaOperator.Parse("[Id] = '" + dep.Name + "'"));
            //                if (Department != null)
            //                {
            //                    StringBuilder Allocationcodequery = new StringBuilder();
            //                    Allocationcodequery.Append("<queryxml><entity>ResourceRole</entity>").Append(System.Environment.NewLine);
            //                    Allocationcodequery.Append("<query><condition><field>DepartmentID<expression op=\"Equals\">" + dep.id.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
            //                    Allocationcodequery.Append("</queryxml>").Append(System.Environment.NewLine);

            //                    var AllocationCoderesponse = clientAuto.query(at_integrations, Allocationcodequery.ToString());
            //                    if (AllocationCoderesponse.ReturnCode == 1)
            //                    {
            //                        if (AllocationCoderesponse.EntityResults.Length > 0)
            //                        {
            //                            foreach (ResourceRole Resourcerole in AllocationCoderesponse.EntityResults)
            //                            {
            //                                StringBuilder SelectResource = new StringBuilder();
            //                                SelectResource.Append("<queryxml><entity>Resource</entity>").Append(System.Environment.NewLine);
            //                                SelectResource.Append("<query><condition><field>Active<expression op=\"equals\">" + "True" + "</expression></field></condition>").Append(System.Environment.NewLine);
            //                                SelectResource.Append("<condition><field>id<expression op=\"Equals\">" + Resourcerole.ResourceID.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
            //                                SelectResource.Append("</queryxml>").Append(System.Environment.NewLine);
            //                                var Resourceslis = clientAuto.query(at_integrations, SelectResource.ToString());
            //                                if (Resourceslis.ReturnCode == 1)
            //                                {
            //                                    foreach (Aria5SystemAdmin.Module.SubAutoTask1.Resource Res in Resourceslis.EntityResults)
            //                                    {
            //                                        Resources Existresource = Objectspace.FindObject<Resources>(CriteriaOperator.Parse("[AutoTaskID] = '" + Res.id + "'"));
            //                                        if (Existresource == null && Res.Email != null)
            //                                        {
            //                                            Resources Newresource = new Resources(Objectspace);
            //                                            Newresource.Name = Res.FirstName.ToString() + Res.LastName.ToString();
            //                                            Newresource.AutoTaskName = Res.FirstName.ToString() + " " + Res.LastName.ToString();
            //                                            Newresource.CurrentEmailAddress = Res.Email.ToString();
            //                                            Newresource.AutoTaskID = Res.id.ToString();
            //                                            Newresource.Department = Department;
            //                                            Newresource.Save();

            //                                        }
            //                                        else if (Existresource != null && Existresource.AutoTaskName != Res.FirstName.ToString() + Res.LastName.ToString())
            //                                        {
            //                                            Existresource.AutoTaskName = Res.FirstName.ToString() + Res.LastName.ToString();
            //                                            Existresource.Save();
            //                                        }
            //                                    }
            //                                }
            //                            }
            //                        }
            //                    }
            //                }
                       
            //           }
            //        }
            //    }
            //}
            #endregion
            StringBuilder SelectResource = new StringBuilder();
            SelectResource.Append("<queryxml><entity>Resource</entity>").Append(System.Environment.NewLine);
            SelectResource.Append("<query><condition><field>Active<expression op=\"equals\">" + "True" + "</expression></field></condition></query>").Append(System.Environment.NewLine);
            // SelectResource.Append("<condition><field>CompletedDate<expression op=\"LessThan\">" + dateto + "</expression></field></condition>").Append(System.Environment.NewLine);
            SelectResource.Append("</queryxml>").Append(System.Environment.NewLine);
            var Resourceslis = clientAuto.query(at_integrations, SelectResource.ToString());
            if (Resourceslis.ReturnCode == 1)
            {
                foreach (Aria5SystemAdmin.Module.SubAutoTask1.Resource Res in Resourceslis.EntityResults)
                {
                        Resources Existresource = Objectspace.FindObject<Resources>(CriteriaOperator.Parse("[AutoTaskID] = '" + Res.id + "'"));
                        if (Existresource == null && Res.Email != null)
                        {
                            Resources Newresource = new Resources(Objectspace);
                            Newresource.Name = Res.FirstName.ToString() + Res.LastName.ToString();
                            Newresource.AutoTaskName = Res.FirstName.ToString() + " " + Res.LastName.ToString();
                            Newresource.CurrentEmailAddress = Res.Email.ToString();
                            Newresource.AutoTaskID = Res.id.ToString();
          
                                    StringBuilder Allocationcodequery = new StringBuilder();
                                    Allocationcodequery.Append("<queryxml><entity>ResourceRole</entity>").Append(System.Environment.NewLine);
                                    Allocationcodequery.Append("<query><condition><field>ResourceID<expression op=\"Equals\">" + Res.id.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                                    Allocationcodequery.Append("</queryxml>").Append(System.Environment.NewLine);

                                    var AllocationCoderesponse = clientAuto.query(at_integrations, Allocationcodequery.ToString());
                                    if (AllocationCoderesponse.ReturnCode == 1)
                                    {
                                        if (AllocationCoderesponse.EntityResults.Length > 0)
                                        {
                                            foreach (ResourceRole Resourcerole in AllocationCoderesponse.EntityResults)
                                           {
                                               if (Resourcerole.QueueID == null)
                                               {
                                                   if (Resourcerole.DepartmentID != null && Resourcerole.DepartmentID.ToString() != "2")
                                                   {
                                                       StringBuilder Dep = new StringBuilder();
                                                       Dep.Append("<queryxml><entity>Department</entity>").Append(System.Environment.NewLine);
                                                       //ATA add oneresource instead of this
                                                       Dep.Append("<query><condition><field>Id<expression op=\"Equals\">" +Resourcerole.DepartmentID.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
                                                       Dep.Append("</queryxml>").Append(System.Environment.NewLine);

                                                       var r12 = clientAuto.query(at_integrations, Dep.ToString());
                                                       if (r12.ReturnCode == 1)
                                                       {
                                                           if (r12.EntityResults.Length > 0)
                                                           {
                                                               foreach (Aria5SystemAdmin.Module.SubAutoTask1.Department dep in r12.EntityResults)
                                                               {
                                                                   Newresource.Department = Objectspace.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Department>(CriteriaOperator.Parse("[Id] = '" + dep.Name + "'"));
                                                               }
                                                           }
                                                       }
                                                   }
                                               }
                                               

                                            }
                                        }
                                    }
                            Newresource.Save();

                        }
                        else if (Existresource != null && Existresource.AutoTaskName != Res.FirstName.ToString() + Res.LastName.ToString())
                        {
                            Existresource.AutoTaskName = Res.FirstName.ToString() + Res.LastName.ToString();
                            Existresource.Save();
                        }
                }
            }
        }
        #region
        //ATA  commente this function and make a new version of it with the same name 2/14/2017 
        //public TicketAHT CalculateAHT(Session newobject, Ticket AutotaskTicket, Dictionary<string, object[]> Queues_with_pmo_dic, IList<QueueName> allqueues, Dictionary<string, object[]> ResourcesDic)//, Dictionary<string, object[]> Queues_with_pmo_dic)
        //{
        //    // make sure that all dictionary values empty 
        //    foreach (var two_record in Queues_with_pmo_dic)
        //    {
        //        Array.Clear(two_record.Value, 0, two_record.Value.Length);
        //    }
        //    foreach (var Resourcerec in ResourcesDic)
        //    {
        //        Array.Clear(Resourcerec.Value, 0, Resourcerec.Value.Length);
        //    }
        //    TicketAHT newtiecket = new TicketAHT(newobject);
        //    newtiecket.Session.LockingOption = LockingOption.None;
        //    TicketDetail new_ticket_detail = new TicketDetail(newobject);

        //    newtiecket.TiecketNum = AutotaskTicket.TicketNumber.ToString();
        //    newtiecket.Completedate = DateTime.Parse(AutotaskTicket.CompletedDate.ToString());
        //    newtiecket.Status = TicketStatus.Complete;
        //    newtiecket.CreateDate = DateTime.Parse(AutotaskTicket.CreateDate.ToString());
        //   Resources ticketres =  newobject.FindObject<Resources>(CriteriaOperator.Parse("[AutoTaskID] = '" + AutotaskTicket.AssignedResourceID + "'"));
        //    if(ticketres != null)
        //    {
        //         newtiecket.PrimaryResource = ticketres;
        //    }
        //    newtiecket.Ticketage = Math.Round((DateTime.Parse(AutotaskTicket.CompletedDate.ToString())).Date.Subtract(DateTime.Parse(AutotaskTicket.CreateDate.ToString()).Date).TotalDays, 0);
        //    newtiecket.Title = (AutotaskTicket.Title.ToString());
        //    //select from auto task ticket not only which it's discription contain the word forward from to make ensure that all ticket retrived to will be used in calculation  
        //    StringBuilder selectnote = new StringBuilder();
        //    selectnote.Append("<queryxml><entity>TicketNote</entity>").Append(System.Environment.NewLine);
        //    selectnote.Append("<query><condition><field>TicketID<expression op=\"Equals\">" + ((Ticket)AutotaskTicket).id.ToString() + "</expression></field>").Append(System.Environment.NewLine);
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
        //                TicketNote note2 = autotasknotes[i];
        //                //check the index of the word forwared from and forwared to 
        //                int indexfrom = note2.Description.ToString().IndexOf("Forwarded From:");
        //                int indexto = note2.Description.ToString().IndexOf("Forwarded To:");
        //                if (indexto > 0)
        //                {
        //                    string Str = note2.Description.ToString().Substring(indexto);
        //                    //get the index of queue word and primary word to get specfically the name of queue that the ticket forward to                          
        //                    int indexofqueue = Str.IndexOf("Queue:");
        //                    int indexofprimaryresource = Str.IndexOf("Primary");

        //                    //ATA add resource aht calculation to the aht calc  get reource name from forward note [srat ]
        //                    string primaryresource = Str.Substring(indexofprimaryresource);
        //                    int indexofresourcename = primaryresource.IndexOf("Resource:");
        //                    int indexofresourcenameend = primaryresource.IndexOf("(");
        //                    string resourcename = primaryresource.Substring(indexofresourcename + 10, indexofresourcenameend - (indexofresourcename + 10)).Replace(" ", "");
        //                    if (ResourcesDic.Keys.Contains(resourcename.Trim()))
        //                    {
        //                        if (ResourcesDic[resourcename][0] != null)
        //                        {
        //                            ResourcesDic[resourcename][2] = ResourcesDic[resourcename][0];
        //                        }
        //                        ResourcesDic[resourcename][0] = DateTime.Parse(note2.LastActivityDate.ToString());

        //                        if (i == autotasknotes.Count - 1)
        //                        {
        //                            double ahtval = Convert.ToDouble(ResourcesDic[resourcename][4]);
        //                            ahtval += Math.Round((DateTime.Parse(((Ticket)AutotaskTicket).CompletedDate.ToString()).Date).Subtract(((DateTime)ResourcesDic[resourcename][0]).Date).TotalDays, 0);
        //                            ResourcesDic[resourcename][4] = ahtval;
        //                        }
        //                    }
        //                    //ATA add resource aht calcualtion get reource name from forward note  [end]
        //                    string subqueue = Str.Substring(indexofqueue + 7, (indexofprimaryresource - 10) - indexofqueue);
        //                    if (Queues_with_pmo_dic.Keys.Contains(subqueue))
        //                    {
        //                       // string newsub = Str.Substring(Str.IndexOf("Primary"), 50);
        //                       // if (newsub.IndexOf("(Project Manager)") > 0)
        //                       // {
        //                            /// [3] is the date tiem that ticket forwared to queue but assigned  on project manger resource 
        //                           // Queues_with_pmo_dic[subqueue][3] = DateTime.Parse(note2.LastActivityDate.ToString());
        //                        //}
        //                      //  else
        //                       // {
        //                            if (Queues_with_pmo_dic[subqueue][0] != null)
        //                            {
        //                                /// [2] that temporary save the date time that ticket forwared to queue
        //                                Queues_with_pmo_dic[subqueue][2] = Queues_with_pmo_dic[subqueue][0];
        //                            }
        //                            ///[0] the date time that ticket forwared to the queue 
        //                            Queues_with_pmo_dic[subqueue][0] = DateTime.Parse(note2.LastActivityDate.ToString());
        //                       // }
        //                        if (i == autotasknotes.Count - 1)
        //                        {
        //                            double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
        //                            x += Math.Round((DateTime.Parse(((Ticket)AutotaskTicket).CompletedDate.ToString()).Date).Subtract(((DateTime)Queues_with_pmo_dic[subqueue][0]).Date).TotalDays, 0);
        //                            Queues_with_pmo_dic[subqueue][4] = x;
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

        //                    //ATA add resource aht calculation to the aht calc  get reource name from forward note [srat ]
        //                    string primaryresource = Str.Substring(indexofprimaryresource);
        //                    int indexofresourcename = primaryresource.IndexOf("Resource:");
        //                    int indexofresourcenameend = primaryresource.IndexOf("(");
        //                    string resourcename = primaryresource.Substring(indexofresourcename + 10, indexofresourcenameend - (indexofresourcename + 10)).Replace(" ", "");
        //                    if (ResourcesDic.Keys.Contains(resourcename))
        //                    {
        //                        ResourcesDic[resourcename][1] = DateTime.Parse(note2.LastActivityDate.ToString());

        //                        if ((Convert.ToDateTime(ResourcesDic[resourcename][1]) == Convert.ToDateTime(ResourcesDic[resourcename][0]) || ResourcesDic[resourcename] == null) && ResourcesDic[resourcename][3] != null)
        //                        {
        //                            //two [subqueue][4] is the total days spend in this queue 
        //                            double x = Convert.ToDouble(ResourcesDic[resourcename][4]);
        //                            x += Math.Round(((DateTime)ResourcesDic[resourcename][1]).Date.Subtract(((DateTime)ResourcesDic[resourcename][3]).Date).TotalDays, 0);
        //                            ResourcesDic[resourcename][4] = x;
        //                            ResourcesDic[resourcename][3] = null;
        //                        }
        //                        else if ((DateTime)ResourcesDic[resourcename][1] != DateTime.MinValue && Convert.ToDateTime(ResourcesDic[resourcename][1]) != Convert.ToDateTime(ResourcesDic[resourcename][0]) && ResourcesDic[resourcename][0] != null)
        //                        {
        //                            double x = Convert.ToDouble(ResourcesDic[resourcename][4]);
        //                            x += Math.Round(((DateTime)ResourcesDic[resourcename][1]).Date.Subtract(((DateTime)ResourcesDic[resourcename][0]).Date).TotalDays, 0);
        //                            ResourcesDic[resourcename][4] = x;
        //                            ResourcesDic[resourcename][0] = null;
        //                        }
        //                        else if ((DateTime)ResourcesDic[resourcename][1] != DateTime.MinValue && Convert.ToDateTime(ResourcesDic[resourcename][1]) == Convert.ToDateTime(ResourcesDic[resourcename][0]) && ResourcesDic[resourcename][2] != null)
        //                        {
        //                            double x = Convert.ToDouble(ResourcesDic[resourcename][4]);
        //                            x += Math.Round(((DateTime)ResourcesDic[resourcename][1]).Date.Subtract(((DateTime)ResourcesDic[resourcename][2]).Date).TotalDays, 0);
        //                            ResourcesDic[resourcename][4] = x;
        //                            ResourcesDic[resourcename][2] = null;
        //                        }
        //                        else if (ResourcesDic[resourcename][1] != null && Convert.ToDateTime(ResourcesDic[resourcename][0]) == Convert.ToDateTime(ResourcesDic[resourcename][1]) && i == 0)
        //                        {
        //                            double x = Convert.ToDouble(ResourcesDic[resourcename][4]);
        //                            x += Math.Round(((DateTime)ResourcesDic[resourcename][1]).Date.Subtract(DateTime.Parse(((Ticket)AutotaskTicket).CreateDate.ToString()).Date).TotalDays, 0);
        //                            ResourcesDic[resourcename][4] = x;
        //                        }
        //                        else if (ResourcesDic[resourcename][1] != null && ResourcesDic[resourcename][0] == null && i == 0)
        //                        {
        //                            double x = Convert.ToDouble(ResourcesDic[resourcename][4]);
        //                            x += Math.Round(((DateTime)ResourcesDic[resourcename][1]).Date.Subtract(DateTime.Parse(((Ticket)AutotaskTicket).CreateDate.ToString()).Date).TotalDays, 0);
        //                            ResourcesDic[resourcename][4] = x;
        //                        }
        //                    }
        //                    //ATA add resource aht calcualtion get reource name from forward note  [end]

        //                    string subqueue = Str.Substring(indexofqueue + 7, (indexofprimaryresource - 10) - indexofqueue);
        //                    if (Queues_with_pmo_dic.Keys.Contains(subqueue))
        //                    {
        //                        Queues_with_pmo_dic[subqueue][1] = DateTime.Parse(note2.LastActivityDate.ToString());
        //                        //string newsub = Str.Substring(Str.IndexOf(subqueue));
        //                        //if (newsub.IndexOf("(Project Manager)") > 0)
        //                        //{
        //                        //    if ((DateTime)Queues_with_pmo_dic[subqueue][1] != DateTime.MinValue && Queues_with_pmo_dic[subqueue][3] != null)
        //                        //    {
        //                        //        // two [subqueue][3] is the date that assigned to project manager (pmo )
        //                        //        //two [subqueue][5] is the total days ticket spend n project management 
        //                        //        double temp = Convert.ToDouble(Queues_with_pmo_dic[subqueue][5]);
        //                        //        temp += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_with_pmo_dic[subqueue][3]).Date).TotalDays, 0);
        //                        //        Queues_with_pmo_dic[subqueue][5] = temp;
        //                        //    }
        //                        //    else if (Queues_with_pmo_dic[subqueue][1] != null && Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) == Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) && i == 0)
        //                        //    {
        //                        //        double temp = Convert.ToDouble(Queues_with_pmo_dic[subqueue][5]);
        //                        //        temp += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(DateTime.Parse(((Ticket)AutotaskTicket).CreateDate.ToString()).Date).TotalDays, 0);
        //                        //        Queues_with_pmo_dic[subqueue][5] = temp;
        //                        //    }

        //                        //}
        //                        if ((Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) == Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) || Queues_with_pmo_dic[subqueue][0] == null) && Queues_with_pmo_dic[subqueue][3] != null)
        //                        {
        //                            //two [subqueue][4] is the total days spend in this queue 
        //                            double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
        //                            x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_with_pmo_dic[subqueue][3]).Date).TotalDays, 0);
        //                            Queues_with_pmo_dic[subqueue][4] = x;
        //                            Queues_with_pmo_dic[subqueue][3] = null;
        //                        }
        //                        else if ((DateTime)Queues_with_pmo_dic[subqueue][1] != DateTime.MinValue && Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) != Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) && Queues_with_pmo_dic[subqueue][0] != null)
        //                        {
        //                            double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
        //                            x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_with_pmo_dic[subqueue][0]).Date).TotalDays, 0);
        //                            Queues_with_pmo_dic[subqueue][4] = x;
        //                            Queues_with_pmo_dic[subqueue][0] = null;
        //                        }
        //                        else if ((DateTime)Queues_with_pmo_dic[subqueue][1] != DateTime.MinValue && Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) == Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) && Queues_with_pmo_dic[subqueue][2] != null)
        //                        {
        //                            double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
        //                            x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_with_pmo_dic[subqueue][2]).Date).TotalDays, 0);
        //                            Queues_with_pmo_dic[subqueue][4] = x;
        //                            Queues_with_pmo_dic[subqueue][2] = null;
        //                        }
        //                        else if (Queues_with_pmo_dic[subqueue][1] != null && Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) == Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) && i == 0)
        //                        {
        //                            double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
        //                            x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(DateTime.Parse(((Ticket)AutotaskTicket).CreateDate.ToString()).Date).TotalDays, 0);
        //                            Queues_with_pmo_dic[subqueue][4] = x;
        //                        }
        //                        else if (Queues_with_pmo_dic[subqueue][1] != null && Queues_with_pmo_dic[subqueue][0] == null && i == 0)
        //                        {
        //                            double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
        //                            x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(DateTime.Parse(((Ticket)AutotaskTicket).CreateDate.ToString()).Date).TotalDays, 0);
        //                            Queues_with_pmo_dic[subqueue][4] = x;
        //                        }
        //                    }
        //                }
        //            }
        //         }
        //    }
        //    //ATA check on user defined field number 1 in the array which is base line due date and number 2 which is instlation date 
        //    if (((Ticket)AutotaskTicket).UserDefinedFields.Count() > 0 && AutotaskTicket.UserDefinedFields[0].Value != null && AutotaskTicket.UserDefinedFields[2].Value != null)
        //    {
        //        newtiecket.DeliveryOnTime = Math.Round(DateTime.Parse(AutotaskTicket.UserDefinedFields[2].Value.ToString()).Subtract(DateTime.Parse(AutotaskTicket.UserDefinedFields[0].Value.ToString())).TotalDays, 0);
        //        if (AutotaskTicket.UserDefinedFields[8].Value != null)
        //        {
        //            newtiecket.UDF = (Udfenum)int.Parse(AutotaskTicket.UserDefinedFields[8].Value.ToString());
        //        }
           
        //    }
        //    if (AutotaskTicket.AccountID.ToString() != null)
        //    {
        //        StringBuilder selectaccount = new StringBuilder();
        //        selectaccount.Append("<queryxml><entity>Account</entity>").Append(System.Environment.NewLine);
        //        selectaccount.Append("<query><condition><field>id<expression op=\"Equals\">" + AutotaskTicket.AccountID.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
        //        selectaccount.Append("</queryxml>").Append(System.Environment.NewLine);
        //        var account = clientAuto.query(at_integrations, selectaccount.ToString());
        //        if (account.ReturnCode == 1)
        //        {
        //            foreach (Aria5SystemAdmin.Module.SubAutoTask1.Account ac in account.EntityResults)
        //            {
        //                newtiecket.Account = ac.AccountName.ToString();
        //                if (ac.UserDefinedFields[0].Value != null)
        //                {
        //                    Aria5SystemAdmin.Module.BusinessObjects.Account system_account = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Id] = '" + ac.UserDefinedFields[0].Value.ToString() + "'"));
        //                    if (system_account != null)
        //                    {
        //                        newtiecket.Account_profile = system_account;
        //                    }
        //                    else
        //                    {
        //                        // newtiecket.Session.LockingOption = LockingOption.None;
        //                        Aria5SystemAdmin.Module.BusinessObjects.Account account_to_creat = Create_system_admin_account(ac.AccountName.ToString(), ac.UserDefinedFields[0].Value.ToString(), Guid.Empty, newobject);
        //                        Aria5SystemAdmin.Module.BusinessObjects.Account account_Exist = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Oid] = '" + account_to_creat.Oid + "'"));
        //                        if (account_Exist != null)
        //                        {
        //                            newtiecket.Account_profile = account_Exist;
        //                        }
        //                    }
        //                }
        //                else
        //                {
        //                    Aria5SystemAdmin.Module.BusinessObjects.Account system_account = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Name] = '" + ac.AccountName.ToString() + "'"));
        //                    if (system_account != null)
        //                    {

        //                        newtiecket.Account_profile = system_account;
        //                    }
        //                    else
        //                    {
        //                        if (ac.ParentAccountID != null)
        //                        {
        //                            StringBuilder selectparentaccount = new StringBuilder();
        //                            selectparentaccount.Append("<queryxml><entity>Account</entity>").Append(System.Environment.NewLine);
        //                            selectparentaccount.Append("<query><condition><field>id<expression op=\"Equals\">" + ac.ParentAccountID.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
        //                            selectparentaccount.Append("</queryxml>").Append(System.Environment.NewLine);
        //                            var parentaccount = clientAuto.query(at_integrations, selectparentaccount.ToString());
        //                            if (parentaccount.ReturnCode == 1)
        //                            {
        //                                foreach (Aria5SystemAdmin.Module.SubAutoTask1.Account parentac in parentaccount.EntityResults)
        //                                {
        //                                    if (parentac.UserDefinedFields[0].Value.ToString() != null)
        //                                    {
        //                                        Aria5SystemAdmin.Module.BusinessObjects.Account systemparentaccount = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Id] = '" + parentac.UserDefinedFields[0].Value.ToString() + "'"));
        //                                        if (systemparentaccount == null)
        //                                        {
        //                                            Aria5SystemAdmin.Module.BusinessObjects.Account parent_account_to_creat = Create_system_admin_account(parentac.AccountName.ToString(), parentac.UserDefinedFields[0].Value.ToString(), Guid.Empty, newobject);
        //                                            Aria5SystemAdmin.Module.BusinessObjects.Account account_to_creat = Create_system_admin_account(ac.AccountName.ToString(), ac.AccountNumber.ToString(), parent_account_to_creat.Oid, newobject);
        //                                            Aria5SystemAdmin.Module.BusinessObjects.Account account_Exist = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Oid] = '" + account_to_creat.Oid + "'"));
        //                                            if (account_Exist != null)
        //                                            {
        //                                                newtiecket.Account_profile = account_Exist;
        //                                            }
        //                                        }
        //                                        else
        //                                        {
        //                                            Aria5SystemAdmin.Module.BusinessObjects.Account account_to_creat = Create_system_admin_account(ac.AccountName.ToString(), ac.AccountNumber.ToString(), systemparentaccount.Oid, newobject);
        //                                            Aria5SystemAdmin.Module.BusinessObjects.Account account_Exist = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Oid] = '" + account_to_creat.Oid + "'"));
        //                                            if (account_Exist != null)
        //                                            {

        //                                                newtiecket.Account_profile = account_Exist;
        //                                            }
        //                                        }
        //                                    }
        //                                }
        //                            }

        //                        }
        //                    }
        //                }
        //            }
        //        }
        //    }
        //    //System.Collections.IList queuevalues = newobject.CreateCollection(typeof(QueueValues));
        //    foreach (string key in Queues_with_pmo_dic.Keys)
        //    {
        //        QueueValues queuvalue = new QueueValues(newobject);
        //        if (Queues_with_pmo_dic[key][4] != null)
        //        {
        //            // QueueName queuname = newobject.FindObject<QueueName>(CriteriaOperator.Parse("[Name] = '" + key + "'"));
        //            QueueName queuname = allqueues.FirstOrDefault(x => x.Name == key);
        //            queuvalue.Type = queuname;
        //            queuvalue.TicketCompleteDate = newtiecket.Completedate;
        //            if (Convert.ToDouble(Queues_with_pmo_dic[key][4]) < 0)
        //            {
        //                queuvalue.Value = 0;

        //            }
        //            else
        //            {
        //                queuvalue.Value = Convert.ToDouble(Queues_with_pmo_dic[key][4]);
        //            }
        //            // queuvalue.Ticket = newtiecket;
        //            newtiecket.QueuesValues.Add(queuvalue);
        //            //newtiecket.QueuesValues.Add(queuvalue);
        //            //queuvalue.Save();
        //            //queuvalue.Session.CommitTransaction();
        //        }

        //    }

        //    foreach (string key in ResourcesDic.Keys)
        //    {
        //        ResourcesAHT Resource_AHT = new ResourcesAHT(newobject);
        //        if (ResourcesDic[key][4] != null)
        //        {
        //            Resources Resource = newobject.FindObject<Resources>(CriteriaOperator.Parse("[AutoTaskName] = '" + key + "'"));
        //            //Resources queuname = allqueues.FirstOrDefault(x => x.Name == key);
        //            if (Resource != null)
        //            {
        //                Resource_AHT.ResourceName = Resource;
        //                Resource_AHT.AHTForAllTicket = Convert.ToDouble(ResourcesDic[key][4]);
        //                Resource_AHT.NumberOfTickets = 1;
        //                if (Math.Round(Convert.ToDouble(ResourcesDic[key][4]), 2) < 0)
        //                {
        //                    Resource_AHT.AHTAvg = 0;

        //                }
        //                else
        //                {
        //                    Resource_AHT.AHTAvg = Math.Round(Convert.ToDouble(ResourcesDic[key][4]), 2);

        //                }
        //                // queuvalue.Ticket = newtiecket;
        //                newtiecket.ResourcesAHT.Add(Resource_AHT);
        //                //newtiecket.QueuesValues.Add(queuvalue);
        //                //queuvalue.Save();
        //                //queuvalue.Session.CommitTransaction();
        //            }
                   
        //        }

        //    }
        //    System.Collections.ICollection alldepartments = newobject.GetObjects(newobject.Dictionary.GetClassInfo(typeof(Aria5SystemAdmin.Module.BusinessObjects.Department)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
        //    IList<Aria5SystemAdmin.Module.BusinessObjects.Department> Departments = alldepartments.Cast<Aria5SystemAdmin.Module.BusinessObjects.Department>().ToList();
        //    foreach (Aria5SystemAdmin.Module.BusinessObjects.Department Departmen in Departments)
        //    {
        //        if (newtiecket.QueuesValues.Where(x => x.Type.Department == Departmen).Count() > 0)
        //        {
        //            int queuevalues = 0;
        //            foreach (QueueValues queueval in newtiecket.QueuesValues.Where(x => x.Type.Department == Departmen))
        //            {
        //                queuevalues += (int)queueval.Value;
        //            }
        //            Aria5SystemAdmin.Module.BusinessObjects.DepartmentAHT DepAHT = new Aria5SystemAdmin.Module.BusinessObjects.DepartmentAHT(newobject);
        //            DepAHT.Department = Departmen;
        //            DepAHT.Value = queuevalues;
        //            newtiecket.DepartmentsAHT.Add(DepAHT);
        //        }

        //    } 
        //    //new_ticket_detail.QERPpmo = Convert.ToDouble(Queues_with_pmo_dic["70 ERP"][5]);
        //    //new_ticket_detail.QERP = Convert.ToDouble(Queues_with_pmo_dic["70 ERP"][4]) - new_ticket_detail.QERPpmo;
        //    //new_ticket_detail.EDIpmo = Convert.ToDouble(Queues_with_pmo_dic["71 EDI"][5]);
        //    //new_ticket_detail.EDI = Convert.ToDouble(Queues_with_pmo_dic["71 EDI"][4]) - new_ticket_detail.EDIpmo;
        //    //new_ticket_detail.CWApmo = Convert.ToDouble(Queues_with_pmo_dic["72 CWA"][5]);
        //    //new_ticket_detail.CWA = Convert.ToDouble(Queues_with_pmo_dic["72 CWA"][4]) - new_ticket_detail.CWApmo;
        //    //new_ticket_detail.Html5pmo = Convert.ToDouble(Queues_with_pmo_dic["73 HTML5"][5]);
        //    //new_ticket_detail.Html5 = Convert.ToDouble(Queues_with_pmo_dic["73 HTML5"][4]) - new_ticket_detail.Html5pmo;
        //   // newtiecket.TicketDetail.Add(new_ticket_detail);
        //    newtiecket.Save();
        //    return newtiecket;
        //}
        #endregion 
        //ATA Add this function to calc AHT for not completed ticket  [Start]
        public TicketAHT CalculateAHT(Session newobject, Ticket AutotaskTicket, Dictionary<string, object[]> Queues_with_pmo_dic, IList<QueueName> allqueues, Dictionary<string, object[]> ResourcesDic)//, Dictionary<string, object[]> Queues_with_pmo_dic)
        {
            // make sure that all dictionary values empty 
            foreach (var two_record in Queues_with_pmo_dic)
            {
                Array.Clear(two_record.Value, 0, two_record.Value.Length);
            }
            foreach (var Resourcerec in ResourcesDic)
            {
                Array.Clear(Resourcerec.Value, 0, Resourcerec.Value.Length);
            }
            TicketAHT newtiecket = new TicketAHT(newobject);
            newtiecket.Session.LockingOption = LockingOption.None;
            TicketDetail new_ticket_detail = new TicketDetail(newobject);

            //ATA new array for not saved resources 
            object[] unknownResourc = new object[6];
            newtiecket.TiecketNum = AutotaskTicket.TicketNumber.ToString();
         //   newtiecket.Completedate = DateTime.Parse(AutotaskTicket.CompletedDate.ToString());
           // newtiecket.Status = TicketStatus.NotCompleted;
            newtiecket.CreateDate = DateTime.Parse(AutotaskTicket.CreateDate.ToString());
            Resources ticketres = newobject.FindObject<Resources>(CriteriaOperator.Parse("[AutoTaskID] = '" + AutotaskTicket.AssignedResourceID + "'"));
            if (ticketres != null)
            {
                newtiecket.PrimaryResource = ticketres;
            }
            if (AutotaskTicket.Status.ToString() == "5" && AutotaskTicket.CompletedDate !=null)
            {
                newtiecket.Ticketage = Math.Round((DateTime.Parse(AutotaskTicket.CompletedDate.ToString())).Date.Subtract(DateTime.Parse(AutotaskTicket.CreateDate.ToString()).Date).TotalDays, 0);
                newtiecket.Completedate = DateTime.Parse(AutotaskTicket.CompletedDate.ToString());
                newtiecket.Status = TicketStatus.Complete;
            
            }
            else
            {
                newtiecket.Ticketage = Math.Round(DateTime.Now.Date.Subtract(DateTime.Parse(AutotaskTicket.CreateDate.ToString()).Date).TotalDays, 0);
                newtiecket.Status = TicketStatus.NotCompleted;
            
            }
            newtiecket.Title = (AutotaskTicket.Title.ToString());
            newtiecket.ID = int.Parse(AutotaskTicket.id.ToString());
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

                            //ATA add resource aht calculation to the aht calc  get reource name from forward note [srat ]
                            string primaryresource = " ";
                            if (indexofprimaryresource > 1)
                            {
                              primaryresource = Str.Substring(indexofprimaryresource);
                            }
                            int indexofresourcename = primaryresource.IndexOf("Resource:");
                            int indexofresourcenameend = primaryresource.IndexOf("(");
                            string resourcename = " ";
                            if (indexofresourcenameend > 1)
                            {
                                resourcename = primaryresource.Substring(indexofresourcename + 10, indexofresourcenameend - (indexofresourcename + 10)).Replace(" ", "");

                            }
                            if (ResourcesDic.Keys.Contains(resourcename.Trim()))
                            {
                                if (ResourcesDic[resourcename][0] != null)
                                {
                                    ResourcesDic[resourcename][2] = ResourcesDic[resourcename][0];
                                }
                                ResourcesDic[resourcename][0] = DateTime.Parse(note2.LastActivityDate.ToString());

                                if (i == autotasknotes.Count - 1)
                                {
                                    double ahtval = Convert.ToDouble(ResourcesDic[resourcename][4]);
                                    if (AutotaskTicket.Status.ToString() == "5")
                                    {
                                        ahtval += Math.Round((DateTime.Parse(((Ticket)AutotaskTicket).CompletedDate.ToString()).Date).Subtract(((DateTime)ResourcesDic[resourcename][0]).Date).TotalDays, 0);
                                    }
                                    else
                                    {
                                        ahtval += Math.Round((DateTime.Now.Date).Subtract(((DateTime)ResourcesDic[resourcename][0]).Date).TotalDays, 0);
                                    }
                                    ResourcesDic[resourcename][4] = ahtval;
                                }
                            }
                            else
                            {
                                if (unknownResourc[0] != null)
                                {
                                    unknownResourc[2] = unknownResourc[0];
                                }
                                unknownResourc[0] = DateTime.Parse(note2.LastActivityDate.ToString());

                                if (i == autotasknotes.Count - 1)
                                {
                                    double ahtval = Convert.ToDouble(unknownResourc[4]);
                                    if (AutotaskTicket.Status.ToString() == "5")
                                    {
                                        ahtval += Math.Round((DateTime.Parse(((Ticket)AutotaskTicket).CompletedDate.ToString()).Date).Subtract(((DateTime)unknownResourc[0]).Date).TotalDays, 0);
                                    }
                                    else 
                                    {
                                        ahtval += Math.Round((DateTime.Now.Date).Subtract(((DateTime)unknownResourc[0]).Date).TotalDays, 0);
                                    }
                                    unknownResourc[4] = ahtval;
                                }
                            }
                            //ATA add resource aht calcualtion get reource name from forward note  [end]
                            string subqueue = " ";
                            if (indexofprimaryresource > 1)
                            {
                               subqueue = Str.Substring(indexofqueue + 7, (indexofprimaryresource - 10) - indexofqueue);

                            }
                            if (Queues_with_pmo_dic.Keys.Contains(subqueue))
                            {
                                // string newsub = Str.Substring(Str.IndexOf("Primary"), 50);
                                // if (newsub.IndexOf("(Project Manager)") > 0)
                                // {
                                /// [3] is the date tiem that ticket forwared to queue but assigned  on project manger resource 
                                // Queues_with_pmo_dic[subqueue][3] = DateTime.Parse(note2.LastActivityDate.ToString());
                                //}
                                //  else
                                // {
                                if (Queues_with_pmo_dic[subqueue][0] != null)
                                {
                                    /// [2] that temporary save the date time that ticket forwared to queue
                                    Queues_with_pmo_dic[subqueue][2] = Queues_with_pmo_dic[subqueue][0];
                                }
                                ///[0] the date time that ticket forwared to the queue 
                                Queues_with_pmo_dic[subqueue][0] = DateTime.Parse(note2.LastActivityDate.ToString());
                                // }
                                if (i == autotasknotes.Count - 1)
                                {
                                    double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
                                    if (AutotaskTicket.Status.ToString() == "5")
                                    {
                                        x += Math.Round((DateTime.Parse(((Ticket)AutotaskTicket).CompletedDate.ToString()).Date).Subtract(((DateTime)Queues_with_pmo_dic[subqueue][0]).Date).TotalDays, 0);

                                    }
                                    else
                                    {
                                        x += Math.Round((DateTime.Now.Date).Subtract(((DateTime)Queues_with_pmo_dic[subqueue][0]).Date).TotalDays, 0);

                                    }
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

                            //ATA add resource aht calculation to the aht calc  get reource name from forward note [srat ]
                            string primaryresource = " ";
                            if (indexofprimaryresource > 1)
                            {
                                primaryresource = Str.Substring(indexofprimaryresource);
                            }
                            int indexofresourcename = primaryresource.IndexOf("Resource:");
                            int indexofresourcenameend = primaryresource.IndexOf("(");
                            string resourcename = " ";
                            if (indexofresourcenameend > 1)
                            {
                               resourcename = primaryresource.Substring(indexofresourcename + 10, indexofresourcenameend - (indexofresourcename + 10)).Replace(" ", "");

                            }
                            if (ResourcesDic.Keys.Contains(resourcename))
                            {
                                ResourcesDic[resourcename][1] = DateTime.Parse(note2.LastActivityDate.ToString());

                                if ((Convert.ToDateTime(ResourcesDic[resourcename][1]) == Convert.ToDateTime(ResourcesDic[resourcename][0]) || ResourcesDic[resourcename] == null) && ResourcesDic[resourcename][3] != null)
                                {
                                    //two [subqueue][4] is the total days spend in this queue 
                                    double x = Convert.ToDouble(ResourcesDic[resourcename][4]);
                                    x += Math.Round(((DateTime)ResourcesDic[resourcename][1]).Date.Subtract(((DateTime)ResourcesDic[resourcename][3]).Date).TotalDays, 0);
                                    ResourcesDic[resourcename][4] = x;
                                    ResourcesDic[resourcename][3] = null;
                                }
                                else if ((DateTime)ResourcesDic[resourcename][1] != DateTime.MinValue && Convert.ToDateTime(ResourcesDic[resourcename][1]) != Convert.ToDateTime(ResourcesDic[resourcename][0]) && ResourcesDic[resourcename][0] != null)
                                {
                                    double x = Convert.ToDouble(ResourcesDic[resourcename][4]);
                                    x += Math.Round(((DateTime)ResourcesDic[resourcename][1]).Date.Subtract(((DateTime)ResourcesDic[resourcename][0]).Date).TotalDays, 0);
                                    ResourcesDic[resourcename][4] = x;
                                    ResourcesDic[resourcename][0] = null;
                                }
                                else if ((DateTime)ResourcesDic[resourcename][1] != DateTime.MinValue && Convert.ToDateTime(ResourcesDic[resourcename][1]) == Convert.ToDateTime(ResourcesDic[resourcename][0]) && ResourcesDic[resourcename][2] != null)
                                {
                                    double x = Convert.ToDouble(ResourcesDic[resourcename][4]);
                                    x += Math.Round(((DateTime)ResourcesDic[resourcename][1]).Date.Subtract(((DateTime)ResourcesDic[resourcename][2]).Date).TotalDays, 0);
                                    ResourcesDic[resourcename][4] = x;
                                    ResourcesDic[resourcename][2] = null;
                                }
                                else if (ResourcesDic[resourcename][1] != null && Convert.ToDateTime(ResourcesDic[resourcename][0]) == Convert.ToDateTime(ResourcesDic[resourcename][1]) && i == 0)
                                {
                                    double x = Convert.ToDouble(ResourcesDic[resourcename][4]);
                                    x += Math.Round(((DateTime)ResourcesDic[resourcename][1]).Date.Subtract(DateTime.Parse(((Ticket)AutotaskTicket).CreateDate.ToString()).Date).TotalDays, 0);
                                    ResourcesDic[resourcename][4] = x;
                                }
                                else if (ResourcesDic[resourcename][1] != null && ResourcesDic[resourcename][0] == null && i == 0)
                                {
                                    double x = Convert.ToDouble(ResourcesDic[resourcename][4]);
                                    x += Math.Round(((DateTime)ResourcesDic[resourcename][1]).Date.Subtract(DateTime.Parse(((Ticket)AutotaskTicket).CreateDate.ToString()).Date).TotalDays, 0);
                                    ResourcesDic[resourcename][4] = x;
                                }
                            }//ATA add this else to calculate if the resoruce not exist 
                            else
                            {
                                unknownResourc[1] = DateTime.Parse(note2.LastActivityDate.ToString());

                                if ((Convert.ToDateTime(unknownResourc[1]) == Convert.ToDateTime(unknownResourc[0])) && unknownResourc[3] != null)
                                {
                                    //two [subqueue][4] is the total days spend in this queue 
                                    double x = Convert.ToDouble(unknownResourc[4]);
                                    x += Math.Round(((DateTime)unknownResourc[1]).Date.Subtract(((DateTime)unknownResourc[3]).Date).TotalDays, 0);
                                    unknownResourc[4] = x;
                                    unknownResourc[3] = null;
                                }
                                else if ((DateTime)unknownResourc[1] != DateTime.MinValue && Convert.ToDateTime(unknownResourc[1]) != Convert.ToDateTime(unknownResourc[0]) && unknownResourc[0] != null)
                                {
                                    double x = Convert.ToDouble(unknownResourc[4]);
                                    x += Math.Round(((DateTime)unknownResourc[1]).Date.Subtract(((DateTime)unknownResourc[0]).Date).TotalDays, 0);
                                    unknownResourc[4] = x;
                                    unknownResourc[0] = null;
                                }
                                else if ((DateTime)unknownResourc[1] != DateTime.MinValue && Convert.ToDateTime(unknownResourc[1]) == Convert.ToDateTime(unknownResourc[0]) && unknownResourc[2] != null)
                                {
                                    double x = Convert.ToDouble(unknownResourc[4]);
                                    x += Math.Round(((DateTime)unknownResourc[1]).Date.Subtract(((DateTime)unknownResourc[2]).Date).TotalDays, 0);
                                    unknownResourc[4] = x;
                                    unknownResourc[2] = null;
                                }
                                else if (unknownResourc[1] != null && Convert.ToDateTime(unknownResourc[0]) == Convert.ToDateTime(unknownResourc[1]) && i == 0)
                                {
                                    double x = Convert.ToDouble(unknownResourc[4]);
                                    x += Math.Round(((DateTime)unknownResourc[1]).Date.Subtract(DateTime.Parse(((Ticket)AutotaskTicket).CreateDate.ToString()).Date).TotalDays, 0);
                                    unknownResourc[4] = x;
                                }
                                else if (unknownResourc[1] != null && unknownResourc[0] == null && i == 0)
                                {
                                    double x = Convert.ToDouble(unknownResourc[4]);
                                    x += Math.Round(((DateTime)unknownResourc[1]).Date.Subtract(DateTime.Parse(((Ticket)AutotaskTicket).CreateDate.ToString()).Date).TotalDays, 0);
                                    unknownResourc[4] = x;
                                }
                            }
                            //ATA add resource aht calcualtion get reource name from forward note  [end]

                            string subqueue = " ";
                            if (indexofprimaryresource > 1)
                            {
                                subqueue = Str.Substring(indexofqueue + 7, (indexofprimaryresource - 10) - indexofqueue);

                            }
                            if (Queues_with_pmo_dic.Keys.Contains(subqueue))
                            {
                                Queues_with_pmo_dic[subqueue][1] = DateTime.Parse(note2.LastActivityDate.ToString());
                                //string newsub = Str.Substring(Str.IndexOf(subqueue));
                                //if (newsub.IndexOf("(Project Manager)") > 0)
                                //{
                                //    if ((DateTime)Queues_with_pmo_dic[subqueue][1] != DateTime.MinValue && Queues_with_pmo_dic[subqueue][3] != null)
                                //    {
                                //        // two [subqueue][3] is the date that assigned to project manager (pmo )
                                //        //two [subqueue][5] is the total days ticket spend n project management 
                                //        double temp = Convert.ToDouble(Queues_with_pmo_dic[subqueue][5]);
                                //        temp += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_with_pmo_dic[subqueue][3]).Date).TotalDays, 0);
                                //        Queues_with_pmo_dic[subqueue][5] = temp;
                                //    }
                                //    else if (Queues_with_pmo_dic[subqueue][1] != null && Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) == Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) && i == 0)
                                //    {
                                //        double temp = Convert.ToDouble(Queues_with_pmo_dic[subqueue][5]);
                                //        temp += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(DateTime.Parse(((Ticket)AutotaskTicket).CreateDate.ToString()).Date).TotalDays, 0);
                                //        Queues_with_pmo_dic[subqueue][5] = temp;
                                //    }

                                //}
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
                else
                {
                    //double x = Convert.ToDouble(Queues_with_pmo_dic["98 Pending Enhancements"][4]);
                    //x += Math.Round(DateTime.Now.Date.Subtract(DateTime.Parse(((Ticket)AutotaskTicket).CreateDate.ToString()).Date).TotalDays, 0);
                    //Queues_with_pmo_dic["98 Pending Enhancements"][4] = x;
                   
                }
            }
            //ATA check on user defined field number 1 in the array which is base line due date and number 2 which is instlation date 
           
            if (((Ticket)AutotaskTicket).UserDefinedFields.Count() > 0)
            {
                if (AutotaskTicket.UserDefinedFields[0].Value != null && AutotaskTicket.UserDefinedFields[2].Value != null)
                {
                    newtiecket.DeliveryOnTime = Math.Round(DateTime.Parse(AutotaskTicket.UserDefinedFields[2].Value.ToString()).Subtract(DateTime.Parse(AutotaskTicket.UserDefinedFields[0].Value.ToString())).TotalDays, 0);
                }
                if (AutotaskTicket.UserDefinedFields[8].Value != null)
                {
                    newtiecket.UDF = (Udfenum)int.Parse(AutotaskTicket.UserDefinedFields[8].Value.ToString());
                }
            }
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
                        if (ac.UserDefinedFields.Length > 0 && ac.UserDefinedFields[0].Value != null)
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
                                if (ac.ParentAccountID != null)
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
            }
            //System.Collections.IList queuevalues = newobject.CreateCollection(typeof(QueueValues));
            foreach (string key in Queues_with_pmo_dic.Keys)
            {
                QueueValues queuvalue = new QueueValues(newobject);
                if (Queues_with_pmo_dic[key][4] != null)
                {
                    // QueueName queuname = newobject.FindObject<QueueName>(CriteriaOperator.Parse("[Name] = '" + key + "'"));
                    QueueName queuname = allqueues.FirstOrDefault(x => x.Name == key);
                    queuvalue.Type = queuname;
                    //queuvalue.TicketCompleteDate = newtiecket.Completedate;
                    if (Convert.ToDouble(Queues_with_pmo_dic[key][4]) < 0)
                    {
                        queuvalue.Value = 0;
                    }
                    else
                    {
                        queuvalue.Value = Convert.ToDouble(Queues_with_pmo_dic[key][4]);

                    }
                    // queuvalue.Ticket = newtiecket;
                    newtiecket.QueuesValues.Add(queuvalue);
                    //newtiecket.QueuesValues.Add(queuvalue);
                    //queuvalue.Save();
                    //queuvalue.Session.CommitTransaction();
                }

            }

            foreach (string key in ResourcesDic.Keys)
            {
                ResourcesAHT Resource_AHT = new ResourcesAHT(newobject);
                if (ResourcesDic[key][4] != null)
                {
                    Resources Resource = newobject.FindObject<Resources>(CriteriaOperator.Parse("[AutoTaskName] = '" + key + "'"));
                    //Resources queuname = allqueues.FirstOrDefault(x => x.Name == key);
                    if (Resource != null)
                    {
                        Resource_AHT.ResourceName = Resource;
                        Resource_AHT.AHTForAllTicket = Convert.ToDouble(ResourcesDic[key][4]);
                        Resource_AHT.NumberOfTickets = 1;
                        if (Math.Round(Convert.ToDouble(ResourcesDic[key][4]), 2) < 0)
                        {
                            Resource_AHT.AHTAvg = 0; 

                        }
                        else
                        {
                            Resource_AHT.AHTAvg = Math.Round(Convert.ToDouble(ResourcesDic[key][4]), 2);

                        }
                        // queuvalue.Ticket = newtiecket;
                        newtiecket.ResourcesAHT.Add(Resource_AHT);
                        //newtiecket.QueuesValues.Add(queuvalue);
                        //queuvalue.Save();
                        //queuvalue.Session.CommitTransaction();
                    }

                }

            }
            ResourcesAHT unknownResource_AHT = new ResourcesAHT(newobject);

            //unknownResource_AHT.ResourceName = Resource;
            unknownResource_AHT.AHTForAllTicket = Convert.ToDouble(unknownResourc[4]);
            unknownResource_AHT.NumberOfTickets = 1;
            if (Math.Round(Convert.ToDouble(unknownResourc[4]), 2) < 0)
            {
                unknownResource_AHT.AHTAvg = 0;

            }
            else
            {
                unknownResource_AHT.AHTAvg = Math.Round(Convert.ToDouble(unknownResourc[4]), 2);

            }
            // queuvalue.Ticket = newtiecket;
            newtiecket.ResourcesAHT.Add(unknownResource_AHT);
            //newtiecket.QueuesValues.Add(queuvalue);
            //queuvalue.Save();
            //queuvalue.Session.CommitTransaction();

            System.Collections.ICollection alldepartments = newobject.GetObjects(newobject.Dictionary.GetClassInfo(typeof(Aria5SystemAdmin.Module.BusinessObjects.Department)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
            IList<Aria5SystemAdmin.Module.BusinessObjects.Department> Departments = alldepartments.Cast<Aria5SystemAdmin.Module.BusinessObjects.Department>().ToList();
            foreach (Aria5SystemAdmin.Module.BusinessObjects.Department Departmen in Departments)
            {
                if (newtiecket.QueuesValues.Where(x => x.Type.Department == Departmen).Count() > 0)
                {
                    int queuevalues = 0;
                    foreach (QueueValues queueval in newtiecket.QueuesValues.Where(x => x.Type.Department == Departmen))
                    {
                        queuevalues += (int)queueval.Value;
                    }
                    Aria5SystemAdmin.Module.BusinessObjects.DepartmentAHT DepAHT = new Aria5SystemAdmin.Module.BusinessObjects.DepartmentAHT(newobject);
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
            // newtiecket.TicketDetail.Add(new_ticket_detail);
            newtiecket.Save();
            return newtiecket;
        }
        //ATA Add this function to calc AHT for not completed ticket  [End]
      
      public Aria5SystemAdmin.Module.BusinessObjects.Account Create_system_admin_account(string accountname, string accountID, Guid parentaccount, Session newobject)
        {
            Aria5SystemAdmin.Module.BusinessObjects.Account account_to_creat = new BusinessObjects.Account(newobject);
            account_to_creat.Name = accountname;
            account_to_creat.Id = accountID;
            account_to_creat.ParentContact = parentaccount;
            account_to_creat.Save();
            return account_to_creat;
        }
        #endregion 
        #region not used code
        public Guid createDOTconfig(DateTime datefrom, DateTime dateto, string Name, Session objectspace)
        {
            objectspace.ConnectionString = connectionstr;
            objectspace.Connect();
            Guid deliveryontime = Guid.NewGuid();
            objectspace.ExecuteQuery("insert into AverageHandleTime(oid,DateFrom,DateTO,Title) values('" + deliveryontime + "','" + datefrom + "','" + dateto + "','" + Name + "')");
            objectspace.ExecuteQuery("insert into DeliveryonTime(oid) values('" + deliveryontime + "')");
            objectspace.Disconnect();
            return deliveryontime;

        }
        //public void createresourceDOT(String resource, int numberoftasks, double totaldot, int OTDtasks, int DOT, int OTD, String DeliveryOntime, Session objectspace)
        //{
        //    objectspace.ExecuteQuery("insert into Resource_DOT([oid],[ResourceName],[NumberOfTasks],[Total],[NumberofOTDTasks],[DeliveryOnTime],[DOT],[OTD]) Values('" + Guid.NewGuid() + "','" + resource + "','" + numberoftasks + "','" + totaldot + "','" + OTDtasks + "','" + DeliveryOntime + "','" + DOT + "','" + OTD + "')");
        //}
        public void createresourceDOT(DeliveryOnTime DOTconfigration)
        {
            //double Total = 0;

            System.Collections.ICollection listofresources = DOTconfigration.Session.GetObjects(DOTconfigration.Session.Dictionary.GetClassInfo(typeof(Resources)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
            foreach (var item in listofresources)
            {
                CalculateResourcedeliveryontime(DOTconfigration, ((Resources)item), true);
                //System.Collections.ICollection listoftasks = DOTconfigration.Session.GetObjects(DOTconfigration.Session.Dictionary.GetClassInfo(typeof(TasksDOT)), CriteriaOperator.Parse("ResourceId = '" + ((Resources)item).AutoTaskID + "'and[TaskCompleteDate] >= '" + DOTconfigration.DateFrom + "'and [TaskCompleteDate] <= '" + DOTconfigration.DateTO + "' "), null, 1000, false, false);
                //if (listoftasks.Count > 0)
                //{
                //    Resource_DOT resourceDOT = new Resource_DOT(DOTconfigration.Session);
                //    int dot = (int)Math.Round(genericcalculatdotfromtasksdottable(listoftasks.Cast<TasksDOT>().ToList(), 5) / listoftasks.Count, 0);
                //    Total = genericcalculatdotfromtasksdottable(listoftasks.Cast<TasksDOT>().ToList(), 5);
                //    int OTDtaks = DOTconfigration.Session.GetObjects(DOTconfigration.Session.Dictionary.GetClassInfo(typeof(TasksDOT)), CriteriaOperator.Parse("ResourceId = '" + ((Resources)item).AutoTaskID + "'and[TaskCompleteDate] >= '" + DOTconfigration.DateFrom + "'and [TaskCompleteDate] <= '" + DOTconfigration.DateTO+ "'and DeliveryOnTime <= 0"), null, 1000, false, false).Count;
                //    int OTDpercentage = (int)Math.Round(Convert.ToDecimal((OTDtaks / listoftasks.Count) * 100), 0);
                   
                //    resourceDOT.ResourceName = ((Resources)item);
                //    resourceDOT.NumberOfTasks = listoftasks.Count;
                //    resourceDOT.NumberofOTDTasks = OTDtaks;
                //    // createresourceDOT(((Resources)item).Oid.ToString(), listoftasks.Count, Total, OTDtaks, dot, OTDpercentage, configration.ToString(), objectspace);
                //}
            }
        }
        public void createDepartmentDOT(Aria5SystemAdmin.Module.BusinessObjects.Department Department, int numberoftasks, int totaldot, int AvgDOT, int OTDtasks, Guid configration, Session objectspace)
        {

        }
        public  void createtask(string projectname, int projectnumber, DateTime? projectcompletedate, string depname,
            string resourename, int resourceid, string tasktitle, string taskid, DateTime? taskcompletedate, DateTime? baselineduedate, long DOT, Session session)
        {

            if (tasktitle.Contains('\''))
            {
                // tasktitle = tasktitle1.Replace("\"", "&apos;");
                return;
            }
            if (projectname.Contains('\''))
            {
                projectname.Replace("\'", "\"");
            }
            session.ExecuteQuery("insert into TasksDOT ([Oid],[ProjectName],[ProjectNumber],[ProjectCompleteDate],[DepName],[ResourceName]" +
      ",[ResourceId],[TaskTitle],[taskid],[TaskCompleteDate],[BaselineDueDate],[DeliveryOnTime]) Values ('" + Guid.NewGuid() + "','" + projectname + "'," + projectnumber + ",'" + projectcompletedate + "','" + depname + "','" + resourename + "'," + resourceid + ",'" + tasktitle.Replace("'", "''") + "','" + taskid + "','" + taskcompletedate + "','" + baselineduedate + "'," + DOT + ")");
        }
       
        #endregion
    }
}
