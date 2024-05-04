using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Azure.WebJobs;
using DevExpress.ExpressApp;
using Aria5SystemAdmin.Module.SubAutoTask1;
using System.ServiceModel;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Data.Filtering;
using DevExpress.Xpo;
using DevExpress.Xpo.Metadata;
using System.Data.SqlClient;
using Aria5SystemAdmin.Module.Managers;
using System.Data;

namespace DOT_Webjob
{
    // To learn more about Microsoft Azure WebJobs SDK, please see http://go.microsoft.com/fwlink/?LinkID=320976
    class Program
    {
        // Please set the following connection strings in app.config for this WebJob to run:
        // AzureWebJobsDashboard and AzureWebJobsStorage
        # region Declarations
        private static string auth_user_id = "PPMO@ariasystems.biz"; // user@domain.com
        private static string auth_user_password = "password7asbyraby";
        private static ATWSZoneInfo zoneInfo = null;
        private static BasicHttpBinding myBinding;
        public static Dictionary<string, string> ResourceName = new Dictionary<string, string>();
        public static Dictionary<string, string> ResourceID = new Dictionary<string, string>();
        public static Dictionary<string, string> LastName = new Dictionary<string, string>();
        public static Dictionary<string, string> UserName = new Dictionary<string, string>();
        public static AutotaskIntegrations at_integrations = new AutotaskIntegrations();
        public static Aria5SystemAdmin.Module.SubAutoTask1.ATWSSoapClient clientAuto = new Aria5SystemAdmin.Module.SubAutoTask1.ATWSSoapClient();
        private string[,] DepartementsList = new string[150, 2];
        public string[,] resourcesList = new string[150, 6];
        private string[,] resourcesRoles = new string[1000, 2];
        
        #endregion
  // const string connectionstr = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin_Test;User ID=sa;Password=aria_123";
    //   const string connectionstr = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123";
  // const string connectionstr = @"Data Source=NSDE_KHALED;Initial Catalog=Aria5SystemAdmin22;User ID=sa;Password=aria_123";
   const string connectionstr = @"Data Source=NSDE_KHALED;Initial Catalog=Aria5SystemAdmin_testing;User ID=sa;Password=aria_123";
        static void Main()
        {
            var host = new JobHost();
            // The following code ensures that the WebJob will be running continuously
           //host.RunAndBlock();+
            Email fortest = new Email();
           fortest.FromEmail = "qua@ariasystems.biz";
            fortest.EmailPassword = "quality_123";
            fortest.ToEmail = "ahmed.r@ariany.com";
            fortest.EmailTitle = "OTD&AHTCalculationReportVerfication";
            fortest.EmailBody = "web job calculate DOt and AHT start Work";
            fortest.SendEmail();
            Session Objectspace = new Session(); 
            Objectspace.ConnectionString = connectionstr;
           // Objectspace.Connect();
            //DateTime xx = Aria5SystemAdmin.Module.CalcEndate.CalcEnddate(DateTime.Now.Subtract(new TimeSpan(12, 0, 0, 0)), 1).Date.Subtract(new TimeSpan(7,0,0,0));
            //xx = DateTime.Now;
          //  if (DateTime.Now.Date == Aria5SystemAdmin.Module.CalcEndate.CalcEnddate(DateTime.Now.Subtract(new TimeSpan(10, 0, 0, 0)), 1).Date)
          //  {
                AHTandDOTCalculation testobject = new AHTandDOTCalculation();
             //   testobject.generateAHT(Aria5SystemAdmin.Module.CalcEndate.CalcEnddate(DateTime.Now.Subtract(new TimeSpan(10, 0, 0, 0)), 0), Aria5SystemAdmin.Module.CalcEndate.CalcEnddate(DateTime.Now.Subtract(new TimeSpan(10, 0, 0, 0)), 1).Subtract(new TimeSpan(1,0,0,0)), null, Objectspace);
                testobject.generateDOT(Aria5SystemAdmin.Module.CalcEndate.CalcEnddate(DateTime.Now.Subtract(new TimeSpan(10, 0, 0, 0)), 0), Aria5SystemAdmin.Module.CalcEndate.CalcEnddate(DateTime.Now.Subtract(new TimeSpan(10, 0, 0, 0)), 1).Subtract(new TimeSpan(1, 0, 0, 0)), Objectspace, null);
                //testobject.generateAHT(DateTime.Parse("2/5/2017"), DateTime.Parse("2/14/2017"), null, Objectspace);
               // testobject.generateDOT(DateTime.Parse("1/1/2017"), DateTime.Parse("2/5/2017"), Objectspace);
         //  }
            Objectspace.Disconnect();
            fortest.EmailBody = "web job calculate DOt and AHT End Work";
          fortest.SendEmail();
          }
    
        #region commented 
      //  public static void generateDOT(DateTime datefrom, DateTime dateto, Session objectspace)
      //  {
      //      //SqlConnection CS = new SqlConnection(connectionstr);
      //      //CS.Open();
      //      //SqlDataAdapter dp = new SqlDataAdapter("select * from TasksDOT Where [TaskCompleteDate] >= '" + datefrom + "'and [TaskCompleteDate] <= '" + dateto + "'", CS);
      //      //DataSet ds = new DataSet();
      //      //DataTable dt = new DataTable("TasksDOT");
      //      //dp.Fill(dt);
      //      //DataTable dt2 = new DataTable("Resources");
      //      //dp.SelectCommand = new SqlCommand("select * from Resources",CS);
      //      //dp.Fill(dt2);
      //      //ds.Tables.Add(dt);
      //      //CS.Close();
            
      //      objectspace.ConnectionString = connectionstr;
      //      objectspace.Connect();
      //      System.Collections.ICollection listofresources = objectspace.GetObjects(objectspace.Dictionary.GetClassInfo(typeof(Resources)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
      //      foreach (var item in listofresources)
      //      {
      //          System.Collections.ICollection listoftasks = objectspace.GetObjects(objectspace.Dictionary.GetClassInfo(typeof(TasksDOT)), CriteriaOperator.Parse("ResourceId = '" + ((Resources)item).AutoTaskID + "'and[TaskCompleteDate] >= '" + datefrom + "'and [TaskCompleteDate] <= '" + dateto + "' "), null, 1000, false, false);
      //          int OTDtaks = objectspace.GetObjects(objectspace.Dictionary.GetClassInfo(typeof(TasksDOT)), CriteriaOperator.Parse("ResourceId = '" + ((Resources)item).AutoTaskID + "'and[TaskCompleteDate] >= '" + datefrom + "'and [TaskCompleteDate] <= '" + dateto + "'and DeliveryOnTime <= 0"), null, 1000, false, false).Count;

      //      }
      //      Guid configration = createDOTconfig(DateTime.Today.Subtract(new TimeSpan(30, 0, 0, 0)), DateTime.Today, DateTime.Today.Month.ToString() + "DOT", new Session());
      //      //foreach (DataRow row in dt2.Rows)
      //      //{
      //      //    string autoid = row["AutoTaskID"].ToString();
      //      //    int Total = 0;
      //      //    if (!string.IsNullOrEmpty(autoid))
      //      //    {
      //      //        int auottaksid = int.Parse(row["AutoTaskID"].ToString());
      //      //        DataRow[] rowdata = dt.Select("ResourceId = " + auottaksid + "");
      //      //        DataRow[] OtDrows = dt.Select("ResourceId = " + auottaksid + "and DeliveryOnTime <= 0");
      //      //        foreach (DataRow task in rowdata)
      //      //        {
      //      //            Total += int.Parse(task["DeliveryOnTime"].ToString());
      //      //        }
      //      //        if (rowdata.Count() > 0)
      //      //        {
      //      //            createresourceDOT(row["oid"].ToString(), rowdata.Count(), Total, OtDrows.Count(), configration.ToString(), new Session());
      //      //        }
      //      //    }
      //      //}
      //      //DevExpress.Xpo.Session sess = new DevExpress.Xpo.Session();
      //      //sess.ConnectionString = connectionstr;
      //      //sess.Connect();
      //      //QueueName Notification_Project = sess.FindObject<QueueName>(CriteriaOperator.Parse("[oid] = '" + dr[0] + "'"));
      //      //if (Notification_Project != null)
      //      //{
      //      //}
      //      //host.Call(typeof(Functions).GetMethod("Checkprojectalarmtimes"), new {value = Notification_Project });
      //  }
      //  public static Guid createDOTconfig(DateTime datefrom, DateTime dateto, string Name, Session objectspace)
      //  {
      //      objectspace.ConnectionString = connectionstr;
      //      objectspace.Connect();
      //      Guid deliveryontime = Guid.NewGuid();
      //      objectspace.ExecuteQuery("insert into AverageHandleTime(oid,DateFrom,DateTO,Title) values('" + deliveryontime + "','" + datefrom + "','" + dateto + "','" + Name + "')");
      //      objectspace.ExecuteQuery("insert into DeliveryonTime(oid) values('" + deliveryontime + "')");
      //      objectspace.Disconnect();
      //      return deliveryontime;
            
      //  }
      //  public static void createresourceDOT(String resource, int numberoftasks, int totaldot, int OTDtasks, String DOT, Session objectspace)
      //  {
      //      objectspace.ConnectionString = connectionstr;
      //      objectspace.Connect();
      //      objectspace.ExecuteQuery("insert into Resource_DOT([oid],[ResourceName],[NumberOfTasks],[Total],[NumberofOTDTasks],[DeliveryOnTime],[DOT],[OTD]) Values('" + Guid.NewGuid() + "','" + resource + "','" + numberoftasks + "','" + totaldot + "','" + OTDtasks + "','" + DOT + "','" + totaldot / numberoftasks + "','" + OTDtasks / numberoftasks + "')");
      //      objectspace.Disconnect();
      //  }
      //  public static void getAHTdata(DateTime datefrom, DateTime dateto, Session Objectspace)
      //  {
      //      zoneInfo = clientAuto.getZoneInfo(auth_user_id);
      //      clientAuto = new ATWSSoapClient();
      //      myBinding = new BasicHttpBinding();
      //      myBinding.Security.Mode = BasicHttpSecurityMode.Transport;
      //      myBinding.Security.Transport.ClientCredentialType = HttpClientCredentialType.Basic;
      //      myBinding.MaxReceivedMessageSize = 2147483647;
      //      EndpointAddress ea = new EndpointAddress(zoneInfo.URL);
      //      clientAuto = new ATWSSoapClient(myBinding, ea);
      //      clientAuto.ClientCredentials.UserName.UserName = auth_user_id;
      //      clientAuto.ClientCredentials.UserName.Password = auth_user_password;

      //      Dictionary<string, object[]> Allqueuesdic = new Dictionary<string, object[]>();
      //      //ObjectsQuery[] objectquery = new ObjectsQuery[1];
      //      //objectquery[0].Criteria = CriteriaOperator.Parse("[oid] != null");
      //      System.Collections.ICollection allqueues = Objectspace.GetObjects(Objectspace.Dictionary.GetClassInfo(typeof(QueueName)), CriteriaOperator.Parse("oid is not null"), null, 1000, false, false);
      //      foreach (var queuname in allqueues)
      //      {
      //           Allqueuesdic.Add(((QueueName)queuname).Name, new object[6]);
      //      }
      //      //SqlConnection CS = new SqlConnection(connectionstr);
      //      //SqlCommand CMD = new SqlCommand("Select * from QueueName", CS);
      //      //CMD.Connection.Open();
      //      //SqlDataReader dr = CMD.ExecuteReader();
      //      //while (dr.Read())
      //      //{
      //      //    //DevExpress.Xpo.Session sess = new DevExpress.Xpo.Session();
      //      //    //sess.ConnectionString = connectionstr;
      //      //    //sess.Connect();
      //      //    //QueueName Notification_Project = sess.FindObject<QueueName>(CriteriaOperator.Parse("[oid] = '" + dr[0] + "'"));
      //      //    //if (Notification_Project != null)
      //      //    //{

      //      //    Allqueuesdic.Add(dr[1].ToString(), new object[6]);   
      //      //    //}
      //      //    //host.Call(typeof(Functions).GetMethod("Checkprojectalarmtimes"), new {value = Notification_Project });
      //      //}
      //      //CMD.Connection.Close();
      //      Objectspace.ConnectionString = connectionstr;
      //      Objectspace.Connect();
      //      StringBuilder Selecttiecket = new StringBuilder();
      //      Selecttiecket.Append("<queryxml><entity>Ticket</entity>").Append(System.Environment.NewLine);
      //      Selecttiecket.Append("<query><condition><field>CompletedDate<expression op=\"greaterthan\">" + datefrom + "</expression></field></condition>").Append(System.Environment.NewLine);
      //      Selecttiecket.Append("<condition><field>CompletedDate<expression op=\"LessThan\">" + dateto + "</expression></field></condition></query>").Append(System.Environment.NewLine);
      //      Selecttiecket.Append("</queryxml>").Append(System.Environment.NewLine);
      //      var tieckets = clientAuto.query(at_integrations, Selecttiecket.ToString());
      //      if (tieckets.ReturnCode == 1)
      //      {
      //          foreach (Ticket ticket in tieckets.EntityResults)
      //          {
      //              //check if this ticket have user defined field or not if it have we can calculate it's AHT if not we needn't to calculate it 
      //              if (ticket.UserDefinedFields.Count() > 0 && ticket.UserDefinedFields[8].Value != null)
      //              {
      //                  if (((Ticket)ticket).TicketNumber != null)
      //                  {
      //                      TicketAHT existticket = Objectspace.FindObject<TicketAHT>(CriteriaOperator.Parse("[TiecketNum] = '" + ((Ticket)ticket).TicketNumber + "'"));
      //                      if (existticket != null && existticket.Ticketage == Math.Round(DateTime.Parse(ticket.CompletedDate.ToString()).Date.Subtract(DateTime.Parse(ticket.CreateDate.ToString()).Date).TotalDays, 0))
      //                      {
      //                          //CreateAHTlist(array, AHT, existticket, newobject);
      //                          continue;
      //                      }
      //                      else
      //                      {
      //                          //TicketAHT AHT_for_ticket = CalculateAHT(Objectspace, ticket, Allqueuesdic);//Queues_without_pmo_dic, Queues_with_pmo_dic);
                                 
      //                      }
      //                  }

      //              }
      //          }
      //      }
      //  }
      //  public static void CalculateAHT(Session newobject, Ticket AutotaskTicket, Dictionary<string, object[]> Queues_with_pmo_dic,IList<QueueName> allqueues)//, Dictionary<string, object[]> Queues_with_pmo_dic)
      //  {
      //      // make sure that all dictionary values empty 
      //      foreach (var two_record in Queues_with_pmo_dic)
      //      {
      //          Array.Clear(two_record.Value, 0, two_record.Value.Length);
      //      }

      //      //  TicketAHT newtiecket = newobject.CreateObject<TicketAHT>();
      //      // newtiecket.Session.LockingOption = LockingOption.None;
      //      // TicketDetail new_ticket_detail = newobject.CreateObject<TicketDetail>();
      //      String TiecketNum = AutotaskTicket.TicketNumber.ToString();
      //      DateTime Completedate = DateTime.Parse(AutotaskTicket.CompletedDate.ToString());
      //      double Ticketage = Math.Round((DateTime.Parse(AutotaskTicket.CompletedDate.ToString())).Date.Subtract(DateTime.Parse(AutotaskTicket.CreateDate.ToString()).Date).TotalDays, 0);
      //      string Title = (AutotaskTicket.Title.ToString());
      //      string Account_profile;
      //      //select from auto task ticket not only which it's discription contain the word forward from to make ensure that all ticket retrived to will be used in calculation  
      //      StringBuilder selectnote = new StringBuilder();
      //      selectnote.Append("<queryxml><entity>TicketNote</entity>").Append(System.Environment.NewLine);
      //      selectnote.Append("<query><condition><field>TicketID<expression op=\"Equals\">" + ((Ticket)AutotaskTicket).id.ToString() + "</expression></field>").Append(System.Environment.NewLine);
      //      selectnote.Append("<field>Description<expression op=\"Contains\">" + "Forwarded From:" + "</expression></field></condition></query>").Append(System.Environment.NewLine);
      //      selectnote.Append("</queryxml>").Append(System.Environment.NewLine);
      //      var notes = clientAuto.query(at_integrations, selectnote.ToString());
      //      if (notes.ReturnCode == 1)
      //      {
      //          if (notes.EntityResults.Count() > 0)
      //          {
      //              // reorder all ticket notes ascending according to ticket note date 
      //              TicketNote[] array123 = Array.ConvertAll(notes.EntityResults, item => (TicketNote)item);
      //              List<TicketNote> autotasknotes = array123.OrderBy(si => si.LastActivityDate).ToList();
      //              for (int i = 0; i < autotasknotes.Count; i++)//TicketNote n in autotasknotes)
      //              {
      //                  TicketNote note2 = autotasknotes[i];
      //                  //check the index of the word forwared from and forwared to 
      //                  int indexfrom = note2.Description.ToString().IndexOf("Forwarded From:");
      //                  int indexto = note2.Description.ToString().IndexOf("Forwarded To:");
      //                  if (indexto > 0)
      //                  {
      //                      string Str = note2.Description.ToString().Substring(indexto);
      //                      //get the index of queue word and primary word to get specfically the name of queue that the ticket forward to                          
      //                      int indexofqueue = Str.IndexOf("Queue:");
      //                      int indexofprimaryresource = Str.IndexOf("Primary");
      //                      string subqueue = Str.Substring(indexofqueue + 7, (indexofprimaryresource - 10) - indexofqueue);
      //                      if (Queues_with_pmo_dic.Keys.Contains(subqueue))
      //                      {
      //                          string newsub = Str.Substring(Str.IndexOf(subqueue));
      //                          if (newsub.IndexOf("(Project Manager)") > 0)
      //                          {
      //                              /// [3] is the date tiem that ticket forwared to queue but assigned  on project manger resource 
      //                              Queues_with_pmo_dic[subqueue][3] = DateTime.Parse(note2.LastActivityDate.ToString());
      //                          }
      //                          else
      //                          {
      //                              if (Queues_with_pmo_dic[subqueue][0] != null)
      //                              {
      //                                  /// [2] that temporary save the date time that ticket forwared to queue
      //                                  Queues_with_pmo_dic[subqueue][2] = Queues_with_pmo_dic[subqueue][0];
      //                              }
      //                              ///[0] the date time that ticket forwared to the queue 
      //                              Queues_with_pmo_dic[subqueue][0] = DateTime.Parse(note2.LastActivityDate.ToString());
      //                          }
      //                          if (i == autotasknotes.Count - 1)
      //                          {
      //                              double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
      //                              x += Math.Round((DateTime.Parse(((Ticket)AutotaskTicket).CompletedDate.ToString()).Date).Subtract(((DateTime)Queues_with_pmo_dic[subqueue][0]).Date).TotalDays, 0);
      //                              Queues_with_pmo_dic[subqueue][4] = x;
      //                          }
      //                      }
      //                  }
      //                  if (indexfrom > 0)
      //                  {
      //                      // substring 100 character form the discription to prevent 2 queue te repeated in this sub string 
      //                      string Str = note2.Description.ToString().Substring(indexfrom, 100);
      //                      //get the index of queue word and primary wod to get specfically the name of queue that the ticket forward from 
      //                      int indexofqueue = Str.IndexOf("Queue:");
      //                      int indexofprimaryresource = Str.IndexOf("Primary");
      //                      string subqueue = Str.Substring(indexofqueue + 7, (indexofprimaryresource - 10) - indexofqueue);
      //                      if (Queues_with_pmo_dic.Keys.Contains(subqueue))
      //                      {
      //                          Queues_with_pmo_dic[subqueue][1] = DateTime.Parse(note2.LastActivityDate.ToString());
      //                          string newsub = Str.Substring(Str.IndexOf(subqueue));
      //                          if (newsub.IndexOf("(Project Manager)") > 0)
      //                          {
      //                              if ((DateTime)Queues_with_pmo_dic[subqueue][1] != DateTime.MinValue && Queues_with_pmo_dic[subqueue][3] != null)
      //                              {
      //                                  // two [subqueue][3] is the date that assigned to project manager (pmo )
      //                                  //two [subqueue][5] is the total days ticket spend n project management 
      //                                  double temp = Convert.ToDouble(Queues_with_pmo_dic[subqueue][5]);
      //                                  temp += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_with_pmo_dic[subqueue][3]).Date).TotalDays, 0);
      //                                  Queues_with_pmo_dic[subqueue][5] = temp;
      //                              }
      //                              else if (Queues_with_pmo_dic[subqueue][1] != null && Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) == Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) && i == 0)
      //                              {
      //                                  double temp = Convert.ToDouble(Queues_with_pmo_dic[subqueue][5]);
      //                                  temp += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(DateTime.Parse(((Ticket)AutotaskTicket).CreateDate.ToString()).Date).TotalDays, 0);
      //                                  Queues_with_pmo_dic[subqueue][5] = temp;
      //                              }

      //                          }
      //                          if ((Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) == Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) || Queues_with_pmo_dic[subqueue][0] == null) && Queues_with_pmo_dic[subqueue][3] != null)
      //                          {
      //                              //two [subqueue][4] is the total days spend in this queue 
      //                              double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
      //                              x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_with_pmo_dic[subqueue][3]).Date).TotalDays, 0);
      //                              Queues_with_pmo_dic[subqueue][4] = x;
      //                              Queues_with_pmo_dic[subqueue][3] = null;
      //                          }
      //                          else if ((DateTime)Queues_with_pmo_dic[subqueue][1] != DateTime.MinValue && Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) != Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) && Queues_with_pmo_dic[subqueue][0] != null)
      //                          {
      //                              double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
      //                              x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_with_pmo_dic[subqueue][0]).Date).TotalDays, 0);
      //                              Queues_with_pmo_dic[subqueue][4] = x;
      //                              Queues_with_pmo_dic[subqueue][0] = null;
      //                          }
      //                          else if ((DateTime)Queues_with_pmo_dic[subqueue][1] != DateTime.MinValue && Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) == Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) && Queues_with_pmo_dic[subqueue][2] != null)
      //                          {
      //                              double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
      //                              x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(((DateTime)Queues_with_pmo_dic[subqueue][2]).Date).TotalDays, 0);
      //                              Queues_with_pmo_dic[subqueue][4] = x;
      //                              Queues_with_pmo_dic[subqueue][2] = null;
      //                          }
      //                          else if (Queues_with_pmo_dic[subqueue][1] != null && Convert.ToDateTime(Queues_with_pmo_dic[subqueue][0]) == Convert.ToDateTime(Queues_with_pmo_dic[subqueue][1]) && i == 0)
      //                          {
      //                              double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
      //                              x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(DateTime.Parse(((Ticket)AutotaskTicket).CreateDate.ToString()).Date).TotalDays, 0);
      //                              Queues_with_pmo_dic[subqueue][4] = x;
      //                          }
      //                          else if (Queues_with_pmo_dic[subqueue][1] != null && Queues_with_pmo_dic[subqueue][0] == null && i == 0)
      //                          {
      //                              double x = Convert.ToDouble(Queues_with_pmo_dic[subqueue][4]);
      //                              x += Math.Round(((DateTime)Queues_with_pmo_dic[subqueue][1]).Date.Subtract(DateTime.Parse(((Ticket)AutotaskTicket).CreateDate.ToString()).Date).TotalDays, 0);
      //                              Queues_with_pmo_dic[subqueue][4] = x;
      //                          }
      //                      }
      //                  }
      //              }
      //          }
      //      }
      //      //ATA check on user defined field number 1 in the array which is base line due date and number 2 which is instlation date 
      //      if (((Ticket)AutotaskTicket).UserDefinedFields.Count() > 0 && AutotaskTicket.UserDefinedFields[0].Value != null && AutotaskTicket.UserDefinedFields[2].Value != null)
      //      {
      //          double DeliveryOnTime = Math.Round(DateTime.Parse(AutotaskTicket.UserDefinedFields[2].Value.ToString()).Subtract(DateTime.Parse(AutotaskTicket.UserDefinedFields[0].Value.ToString())).TotalDays, 0);
      //      }
      //      int udf = int.Parse(AutotaskTicket.UserDefinedFields[8].Value.ToString());
      //      if (AutotaskTicket.AccountID.ToString() != null)
      //      {
      //          StringBuilder selectaccount = new StringBuilder();
      //          selectaccount.Append("<queryxml><entity>Account</entity>").Append(System.Environment.NewLine);
      //          selectaccount.Append("<query><condition><field>id<expression op=\"Equals\">" + AutotaskTicket.AccountID.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
      //          selectaccount.Append("</queryxml>").Append(System.Environment.NewLine);
      //          var account = clientAuto.query(at_integrations, selectaccount.ToString());
      //          if (account.ReturnCode == 1)
      //          {
      //              foreach (Aria5SystemAdmin.Module.SubAutoTask1.Account ac in account.EntityResults)
      //              {
      //                  string Account = ac.AccountName.ToString();
      //                  if (ac.UserDefinedFields[0].Value != null)
      //                  {
      //                      Aria5SystemAdmin.Module.BusinessObjects.Account system_account = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Id] = '" + ac.UserDefinedFields[0].Value.ToString() + "'"));
      //                      if (system_account != null)
      //                      {
      //                           Account_profile = system_account.Oid.ToString();
      //                      }
      //                      else
      //                      {
      //                          //newtiecket.Session.LockingOption = LockingOption.None;
      //                          //Aria5SystemAdmin.Module.BusinessObjects.Account account_to_creat = Create_system_admin_account(ac.AccountName.ToString(), ac.UserDefinedFields[0].Value.ToString(), Guid.Empty, newobject);
      //                          //Aria5SystemAdmin.Module.BusinessObjects.Account account_Exist = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Oid] = '" + account_to_creat.Oid + "'"));
      //                          //if (account_Exist != null)
      //                          //{
      //                          //    string Account_profile = account_Exist.oid;
      //                          //}
      //                      }
      //                  }
      //                  else
      //                  {
      //                      Aria5SystemAdmin.Module.BusinessObjects.Account system_account = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Name] = '" + ac.AccountName.ToString() + "'"));
      //                      if (system_account != null)
      //                      {

      //                          Account_profile = system_account.Oid.ToString();
      //                      }
      //                      else
      //                      {
      //                      //    StringBuilder selectparentaccount = new StringBuilder();
      //                      //    selectparentaccount.Append("<queryxml><entity>Account</entity>").Append(System.Environment.NewLine);
      //                      //    selectparentaccount.Append("<query><condition><field>id<expression op=\"Equals\">" + ac.ParentAccountID.ToString() + "</expression></field></condition></query>").Append(System.Environment.NewLine);
      //                      //    selectparentaccount.Append("</queryxml>").Append(System.Environment.NewLine);
      //                      //    var parentaccount = clientAuto.query(at_integrations, selectparentaccount.ToString());
      //                      //    if (parentaccount.ReturnCode == 1)
      //                      //    {
      //                      //        foreach (Aria5SystemAdmin.Module.SubAutoTask1.Account parentac in parentaccount.EntityResults)
      //                      //        {
      //                      //            if (parentac.UserDefinedFields[0].Value.ToString() != null)
      //                      //            {
      //                      //                Aria5SystemAdmin.Module.BusinessObjects.Account systemparentaccount = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Id] = '" + parentac.UserDefinedFields[0].Value.ToString() + "'"));
      //                      //                if (systemparentaccount == null)
      //                      //                {
      //                      //                    Aria5SystemAdmin.Module.BusinessObjects.Account parent_account_to_creat = Create_system_admin_account(parentac.AccountName.ToString(), parentac.UserDefinedFields[0].Value.ToString(), Guid.Empty, newobject);
      //                      //                    Aria5SystemAdmin.Module.BusinessObjects.Account account_to_creat = Create_system_admin_account(ac.AccountName.ToString(), ac.AccountNumber.ToString(), parent_account_to_creat.Oid, newobject);
      //                      //                    Aria5SystemAdmin.Module.BusinessObjects.Account account_Exist = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Oid] = '" + account_to_creat.Oid + "'"));
      //                      //                    if (account_Exist != null)
      //                      //                    {

      //                      //                        newtiecket.Account_profile = account_Exist;
      //                      //                    }
      //                      //                }
      //                      //                else
      //                      //                {
      //                      //                    Aria5SystemAdmin.Module.BusinessObjects.Account account_to_creat = Create_system_admin_account(ac.AccountName.ToString(), ac.AccountNumber.ToString(), systemparentaccount.Oid, newobject);
      //                      //                    Aria5SystemAdmin.Module.BusinessObjects.Account account_Exist = newobject.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Oid] = '" + account_to_creat.Oid + "'"));
      //                      //                    if (account_Exist != null)
      //                      //                    {

      //                      //                        newtiecket.Account_profile = account_Exist;
      //                      //                    }
      //                      //                }
      //                      //            }
      //                      //        }
      //                      //    }

      //                      }
      //                  }
      //              }

      //          }
      //      }
      //      //System.Collections.IList queuevalues = newobject.CreateCollection(typeof(QueueValues));
      //      foreach (string key in Queues_with_pmo_dic.Keys)
      //      {
      //          // QueueValues queuvalue = newobject.CreateObject<QueueValues>();
      //          if (Convert.ToInt32(Queues_with_pmo_dic[key][4]) > 0)
      //          {
      //              // QueueName queuname = newobject.FindObject<QueueName>(CriteriaOperator.Parse("[Name] = '" + key + "'"));
      //              QueueName queuname = allqueues.FirstOrDefault(x => x.Name == key);
      //              string queuenameoid = queuname.Oid.ToString();
      //              DateTime TicketCompleteDate = Completedate;
      //              double Value = Convert.ToDouble(Queues_with_pmo_dic[key][4]);
      //              string ticketoid;
      //              // queuvalue.Ticket = newtiecket;
      //              //newtiecket.QueuesValues.Add(queuvalue);
      //              //queuvalue.Save();
      //              //queuvalue.Session.CommitTransaction();
      //          }
      //      }
      //      //newtiecket.Save();
      //      //newobject.CommitChanges();
      //      //return newtiecket;
      //  }
      //  public static Guid insertaccount(string accountname, string accountcode, Session Objectspace)
      //  {
      //      Guid accountoid = Guid.NewGuid();
      //      Objectspace.ExecuteQuery("insert into Entity (oid,Name,Id) values ('" + accountoid + "','" + accountname + "','" + accountcode + "')");
      //       return accountoid;
      //  }
      //  public static Guid insertticket()
      //  {
      //      Guid ticketoid = Guid.NewGuid();
      //      return ticketoid;
      //  }
      //  public static void insertqueuevalues(string queuename, double value, DateTime date, string ticket,Session Objectspace)
      //  {
           
      //      Objectspace.ExecuteQuery("insert into QueueValues(Oid,Type,Value,TicketCompleteDate,Ticket) values('"+Guid.NewGuid()+"','"+queuename+"','"+value+"','"+date+"','"+ticket +"') ");
            
      //  }
      //  public static void createtask(string projectname,int projectnumber,DateTime? projectcompletedate,string depname,
      //      string resourename, int resourceid, string tasktitle, string taskid, DateTime? taskcompletedate, DateTime? baselineduedate, long DOT, Session session)
      //  {
           
      //      if (tasktitle.Contains('\''))
      //      {
      //          // tasktitle = tasktitle1.Replace("\"", "&apos;");
      //          return;
      //      }
      //      if (projectname.Contains('\''))
      //      {
      //          projectname.Replace("\'", "\"");
      //      }
      //      session.ExecuteQuery("insert into TasksDOT ([Oid],[ProjectName],[ProjectNumber],[ProjectCompleteDate],[DepName],[ResourceName]"+
      //",[ResourceId],[TaskTitle],[taskid],[TaskCompleteDate],[BaselineDueDate],[DeliveryOnTime]) Values ('" + Guid.NewGuid() + "','" + projectname + "'," + projectnumber + ",'" + projectcompletedate + "','" + depname + "','" + resourename + "'," + resourceid + ",'" + tasktitle.Replace("'", "''") + "','" + taskid + "','" + taskcompletedate + "','" + baselineduedate + "'," + DOT + ")");
      //  }
       #endregion
    }
}
 