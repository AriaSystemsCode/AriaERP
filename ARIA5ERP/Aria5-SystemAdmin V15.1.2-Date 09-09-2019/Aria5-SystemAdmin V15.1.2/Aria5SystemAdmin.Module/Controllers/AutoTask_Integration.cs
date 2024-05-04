using System;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp;
using DevExpress.Data.Filtering;
using System.Collections.Generic;
using DevExpress.Persistent.Base;
using DevExpress.ExpressApp.Utils;
using DevExpress.ExpressApp.Layout;
using DevExpress.ExpressApp.Actions;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Templates;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.SystemModule;
using DevExpress.ExpressApp.Model.NodeGenerators;
using Aria5SystemAdmin.Module.SubAutoTask1;
using System.ServiceModel;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.ExpressApp.Xpo;
using DevExpress.ExpressApp.Web;
using Excel =  Microsoft.Office.Interop.Excel;
using System.Drawing;
using DevExpress.Spreadsheet;
using DevExpress.XtraSpreadsheet;
using DevExpress.Web.ASPxSpreadsheet;
using System.IO;
using DevExpress.Utils.OAuth.Provider;
using System.Web;
using DevExpress.Web;
using DevExpress.XtraPrinting.Export.Pdf;
using System.Data.SqlClient;
using Aria5SystemAdmin.Module.Managers;
using DevExpress.Xpo;
using DevExpress.ExpressApp.Validation;
namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class AutoTask_Integration : ViewController
    {
         public AutoTask_Integration()
        {
            InitializeComponent();
            RegisterActions(components);
            // Target required Views (via the TargetXXX properties) and create their Actions.
        }
         ProjectTemplate currentObject = null;
        protected override void OnActivated()
        {
            base.OnActivated();
            this.download_AHt_file.Model.SetValue<bool>("IsPostBackRequired", true);
            this.Get_DOT_data.Active["0"] = false;
            #region updateprojectactios
            if (View is DetailView && View.ObjectTypeInfo.Type == typeof(ProjectTemplate))
            {
                if (((ProjectTemplate)View.CurrentObject).AutoTaskID == 0)
                {
                    this.creatproject.Active["0"] = true;
                    this.updateproject.Active["0"] = false;
                }
                else
                {
                    this.creatproject.Active["0"] = false;
                    this.updateproject.Active["0"] = true;
                }

                currentObject = null;

                //View.CurrentObjectChanged += View_CurrentObjectChanged;
                //if (((DetailView)View).ViewEditMode != ViewEditMode.Edit)
                //{
                   
                //    ((DetailView)View).FindItem("IterationNumber").Caption = "Year";
                //}
            }
            else
            {
                this.creatproject.Active["0"] = false;
                this.updateproject.Active["0"] = false;
            }
            #endregion
            #region AHTactions
       if (View is DetailView && View.ObjectTypeInfo.Type == typeof(AverageHandleTime))
        {
            if (((AverageHandleTime)View.CurrentObject).Tickets.Count == 0)
            {
                this.download_AHt_file.Active["0"] = false;
                this.Calculate_AHT.Active["0"] = true;
                this.calculate_dot.Active["0"] = false;
                this.AHTForCompletedTickets.Active["0"] = true;
                this.AHTForNotCompletedTickets.Active["0"] = true;
            }
            else
            {
                this.download_AHt_file.Active["0"] = true;
                this.Calculate_AHT.Active["0"] = false;
                this.calculate_dot.Active["0"] = false;
                this.AHTForCompletedTickets.Active["0"] = false;
                this.AHTForNotCompletedTickets.Active["0"] = false;

            }

        }
        else if (View is DetailView && View.ObjectTypeInfo.Type == typeof(DeliveryOnTime))
        {
                this.download_AHt_file.Active["0"] = false;
                this.calculate_dot.Active["0"] = true;
                this.Calculate_AHT.Active["0"] = false;
               

        }
        else
        {
            this.download_AHt_file.Active["0"] = false;
            this.calculate_dot.Active["0"] = false;
            this.Calculate_AHT.Active["0"] = false;
        }

            #endregion   // Perform various tasks depending on the target View.
     

        }
        protected override void OnViewControlsCreated()
        {
            base.OnViewControlsCreated();

            // Access and customize the target View control.
        }
           protected override void OnDeactivated()
        {
            // Unsubscribe from previously subscribed events and release other references and resources.
            base.OnDeactivated();
            //View.CurrentObjectChanged -= View_CurrentObjectChanged;
        }
        //ATA change caption at run time 
    //     private void View_CurrentObjectChanged(object sender, EventArgs e)
    //{
    //  if (currentObject != null)
    //    currentObject.Changed -= currentObject_Changed;

    //  if (currentObject is ProjectTemplate)
    //  {
    //    currentObject = View.CurrentObject as ProjectTemplate;
    //    if (currentObject != null)
    //      currentObject.Changed += currentObject_Changed;
    //   // this.View;
    //  }
    //}
    //     void currentObject_Changed(object sender, DevExpress.Xpo.ObjectChangeEventArgs e)
    //     {
    //         if (e.OldValue != e.NewValue)
    //         {
    //            // IObjectSpace workingObjectSpace = ObjectSpace.CreateNestedObjectSpace();

    //             switch (e.PropertyName)
    //             {
    //                 case "IterationNumber":
    //                     ((DetailView)View).FindItem("IterationNumber").Caption = "Year";
    //                     break;

    //                 //case "Property2":
    //                 //    break;

    //                 default:
    //                     break;
    //             }
    //         }
    //     }
        private void GetResource_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            Aria5SystemAdmin.Module.Managers.AutoTaskIntegrationManger Resourceobject = new Aria5SystemAdmin.Module.Managers.AutoTaskIntegrationManger();
            System.Net.ServicePointManager.SecurityProtocol = System.Net.SecurityProtocolType.Tls12;
            if (View is DetailView && View.ObjectTypeInfo.Type == typeof(Resources))
            {
                Resourceobject.GetResourceInfo((Resources)View.CurrentObject);
                //GetResourceInfo((Resources)View.CurrentObject);
                View.Refresh();
            }
            else if (View is ListView &&  View.ObjectTypeInfo.Type == typeof(Resources))
            {
              foreach(var resource in View.SelectedObjects)
              {

                  Resourceobject.GetResourceInfo((Resources)ObjectSpace.GetObject(resource));
              }
              View.Refresh();
            }
                    
        }
        
        private void AutoTask_Integration_Activated(object sender, EventArgs e)
        {
            /*ATA ,1 , AutoTaskIntegration [Start]
            */
            if (View.ObjectTypeInfo.Type == typeof(Resources))
            {
                this.GetResource.Active["1"] = true;
            }
            else
            {
                this.GetResource.Active["1"] = false;
            }
            //ATA
            //test 
            //ATA update while openeing 
            //if (View is DetailView && View.ObjectTypeInfo.Type == typeof(ProjectTemplate))
            //{
            //    if (View is DetailView && View.ObjectTypeInfo.Type == typeof(ProjectTemplate))
            //    {
            //        ProjectTemplate openedprojectone = ((ProjectTemplate)View.CurrentObject).Session.FindObject<ProjectTemplate>(CriteriaOperator.Parse("[Oid] = '" + ((ProjectTemplate)View.CurrentObject).Oid + "'"));

            //        if (openedprojectone.AutoTaskID != 0)
            //        {
            //            AutoTaskIntegrationManger openedproject = new AutoTaskIntegrationManger();
            //            openedproject.UpdatePhase(openedprojectone.AutoTaskID, openedprojectone.UseCasePoints);
            //            double[] testarray = new double[4];
            //            testarray[0] = 0;
            //            testarray[1] = 4;
            //            testarray[2] = 44;
            //            testarray[3] = 52;
                     
            //            openedproject.Phaseshours(openedprojectone, openedprojectone.UseCasePoints, openedproject.UpdateTask(openedprojectone));
            //      }
            //        openedprojectone.Session.Save(openedprojectone);
            //        openedprojectone.Session.CommitTransaction();
            //        ObjectSpace.CommitChanges();
            //        //openedprojectone.Reload();
            //        View.Refresh();
                    
            //    }
            //}
            //test 
        }
        private void creatproject_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            Aria5SystemAdmin.Module.Managers.AutoTaskIntegrationManger Project = new Aria5SystemAdmin.Module.Managers.AutoTaskIntegrationManger();
            ((ProjectTemplate)View.CurrentObject).AutoTaskID = Project.CreateNewAutoTaskProject((ProjectTemplate)View.CurrentObject);
            if (((ProjectTemplate)View.CurrentObject).AutoTaskID != 0)
            {
                //Project.CreateNewPhase(((ProjectTemplate)View.CurrentObject).AutoTaskID, ((ProjectTemplate)View.CurrentObject).UseCasePoints);
                //Project.CreateActivity(((ProjectTemplate)View.CurrentObject).AutoTaskID, ((ProjectTemplate)View.CurrentObject).UseCasePoints);
                if (((ProjectTemplate)View.CurrentObject).ProjectEntities != null)
                {
                    foreach (QAProjectEntity ProjectEntity in ((ProjectTemplate)View.CurrentObject).ProjectEntities)
                    {
                        Project.CreateTask(((ProjectTemplate)View.CurrentObject).AutoTaskID, ProjectEntity);
                    }
                }
                View.Refresh();
            }
        }
        private void updateproject_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            Aria5SystemAdmin.Module.Managers.AutoTaskIntegrationManger Project = new Aria5SystemAdmin.Module.Managers.AutoTaskIntegrationManger();
            if (((ProjectTemplate)View.CurrentObject).AutoTaskID != 0)
            {
                
                   //try
                   //{
                        Project.UpdateAutoTaskProject((ProjectTemplate)View.CurrentObject,ObjectSpace);
                       // Project.UpdatePhase(((ProjectTemplate)View.CurrentObject).AutoTaskID, ((ProjectTemplate)View.CurrentObject).UseCasePoints);
                       // Project.UpdatePhaseNew(((ProjectTemplate)View.CurrentObject).AutoTaskID, ObjectSpace);
                        Project.Phaseshours((ProjectTemplate)View.CurrentObject, ((ProjectTemplate)View.CurrentObject).UseCasePoints, Project.UpdateTaskNew((ProjectTemplate)View.CurrentObject, ObjectSpace));
                      
               // Project.Phaseshours((ProjectTemplate)View.CurrentObject, ((ProjectTemplate)View.CurrentObject).UseCasePoints, Project.UpdateTask((ProjectTemplate)View.CurrentObject,ObjectSpace));
         
                    if (((ProjectTemplate)View.CurrentObject).ProjectEntities != null)
                    {
                        foreach (TrackingEntry entity in ((ProjectTemplate)View.CurrentObject).TrackingEntries)
                        {
                            Project.updateprojectentity((ProjectTemplate)View.CurrentObject, entity);
                        }
                    }
                    View.Refresh();
                 // WebWindow.CurrentRequestWindow.RegisterClientScript("ShowAlert", @"alert('Project Updated');");
                //}
                //catch
                //{
                //    WebWindow.CurrentRequestWindow.RegisterClientScript("ShowAlert", @"alert('Project not fully Updated');");
                //}
              
            

            }
        }
        //ATA that is action calculate AHT per ticket complete date  range 5/18/2016 [start]
        private void Calculate_AHT_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
           // Aria5SystemAdmin.Module.Managers.AutoTaskIntegrationManger selecttiecket = new Aria5SystemAdmin.Module.Managers.AutoTaskIntegrationManger();
            if (View.ObjectTypeInfo.Type == typeof(AverageHandleTime))
            {
                IObjectSpace object1 = Application.CreateObjectSpace();
                AHTandDOTCalculation newobj = new AHTandDOTCalculation(); 
                //ATa call the generate aht function that calc all the required aht instead call it one by one 1/19/2017 [start]
                #region oldcode
                // selecttiecket.calculateQueueAHT(((AverageHandleTime)View.CurrentObject), object1);
                //selecttiecket.calculateDepartmentAHT(((AverageHandleTime)View.CurrentObject), object1);
                //  newobj.calculateResourceAHT((AverageHandleTime)View.CurrentObject, ((XPObjectSpace)this.ObjectSpace).Session);
                // selecttiecket.get_tickets_Aht_by_date_range(((AverageHandleTime)View.CurrentObject).DateFrom, ((AverageHandleTime)View.CurrentObject).DateTO, object1, (AverageHandleTime)View.CurrentObject, ((XPObjectSpace)this.ObjectSpace).Session);
                #endregion 
                newobj.generateAHT(((AverageHandleTime)View.CurrentObject).DateFrom, ((AverageHandleTime)View.CurrentObject).DateTO, (AverageHandleTime)View.CurrentObject, ((XPObjectSpace)this.ObjectSpace).Session);
                ((XPObjectSpace)this.ObjectSpace).Session.CommitTransaction();
               
                //ATa call the generate aht function that calc all the required aht instead call it one by one 1/19/2017 [End]
                
                object1.CommitChanges();
                View.Refresh();
                this.Calculate_AHT.Active["0"] = false;
                this.AHTForCompletedTickets.Active["0"] = false;
                this.AHTForNotCompletedTickets.Active["0"] = false;
                this.download_AHt_file.Active["0"] = true;
              }
        }
        //ATA that is action calculate AHT per ticket complete date  range 187/5/2016 [end]
        //ATA that is action calculate AHT per ticket number 18/5/2016 [start]
        private void Calculate_AHT_for_ticket_Execute(object sender, ParametrizedActionExecuteEventArgs e)
        {
            //MMT
            //string ticketnum = e.ParameterCurrentValue.ToString().Trim();
            string ticketnum = ((Aria5SystemAdmin.Module.BusinessObjects.TicketAHT)e.CurrentObject).TiecketNum.ToString().Trim();

            //MMT
            Aria5SystemAdmin.Module.Managers.AutoTaskIntegrationManger AHT_For_One_Ticket = new Aria5SystemAdmin.Module.Managers.AutoTaskIntegrationManger();
            IObjectSpace object1 = Application.CreateObjectSpace();
            Session currentsession = ((XPObjectSpace)this.ObjectSpace).Session;
            object obj = object1.FindObject<TicketAHT>(CriteriaOperator.Parse("[TiecketNum] = '" + ticketnum + "'"));
            if (obj != null)
            {
                e.ShowViewParameters.CreatedView = Application.CreateDetailView(object1, obj);
            }
            else
            {

            TicketAHT ticketcreated = AHT_For_One_Ticket.get_ticket_Aht_by_number(object1, ticketnum, currentsession);
            ((XPObjectSpace)this.ObjectSpace).CommitChanges();
            ticketcreated = object1.FindObject<TicketAHT>(CriteriaOperator.Parse("[Oid] = '"+ticketcreated.Oid+"'"));
                if (ticketcreated != null)
                {
                    e.ShowViewParameters.CreatedView = Application.CreateDetailView(object1, ticketcreated);

                }
            }
        }
        //ATA that is action calculate AHT per ticket number 187/5/2016 [END]
        //ATA this action that download the AHT file from system admin 17/5/2016[start]
        private void download_AHt_file_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            if (View.ObjectTypeInfo.Type == typeof(AverageHandleTime))
            {
                #region Report Code
                //    if (((AverageHandleTime)View.CurrentObject).Tickets.Count > 0)
                //    {
                //        ASPxSpreadsheet newspreadsheet = new ASPxSpreadsheet();
                //        IWorkbook xlWorkBook = newspreadsheet.Document;
                //        xlWorkBook.Worksheets.Add("Tickets AHT");
                //        xlWorkBook.Worksheets["sheet1"].Visible = false;
                //        Worksheet xlWorkSheet = newspreadsheet.Document.Worksheets["Tickets AHT"];
                //        IList<QueueName> allqueues = ObjectSpace.GetObjects<QueueName>();
                //        ///<summary>
                //        ///create microsoft excel sheet 
                //        ///</summary>
                ////        Excel.Application xlApp;
                ////        Excel.Workbook xlWorkBook;
                ////        Excel.Worksheet xlWorkSheet;
                ////        object misValue = System.Reflection.Missing.Value;
                ////        xlApp = new Excel.Application();
                ////        xlWorkBook = xlApp.Workbooks.Add(misValue);
                ////        xlWorkSheet = (Excel.Worksheet)xlWorkBook.Worksheets.get_Item(1);
                ///// fill the header of the sheet and colored it 
                //        xlWorkSheet.Range["A1:AA1"].FillColor = Color.LightBlue;
                //        xlWorkSheet.Range["A1:AA1"].Font.Color = Color.Black;
                //        xlWorkSheet.Cells[0, 0].Value = "Ticket Name";
                //        xlWorkSheet.Cells[0, 1].Value = "Account";
                //        xlWorkSheet.Cells[0, 2].Value = "Title";
                //       // xlWorkSheet.Cells[0, 3].Value= "Account";
                //        xlWorkSheet.Cells[0, 3].Value = "Completed Date";
                //        for (int queueindex = 0; queueindex < allqueues.Count; queueindex++)
                //        {
                //            int columnnumber = queueindex + 4;
                //            xlWorkSheet.Cells[0, columnnumber].Value = allqueues[queueindex].Name;
                //        }
                //        xlWorkSheet.Cells[0, allqueues.Count + 4].Value = "Ticket Age";

                //        #region Commented AHT static Code
                //        //xlWorkSheet.Cells[0, 4].Value = "Help Desk";
                //        //xlWorkSheet.Cells[0, 5].Value = "Customer Care";
                //        //xlWorkSheet.Cells[0, 6].Value = "Customer Acceptance";
                //        //xlWorkSheet.Cells[0, 7].Value = "Sales";
                //        //xlWorkSheet.Cells[0, 8].Value= "ERP";
                //        //xlWorkSheet.Cells[0, 9].Value = "EDI";
                //        //xlWorkSheet.Cells[0, 10].Value = "CWA";
                //        //xlWorkSheet.Cells[0, 11].Value = "Html5";
                //        //xlWorkSheet.Cells[0, 12].Value= "Validation";
                //        //xlWorkSheet.Cells[0, 13].Value= "Review";
                //        //xlWorkSheet.Cells[0, 14].Value= "Sales";
                //        //xlWorkSheet.Cells[0, 15].Value= "Postal Sale";
                //        //xlWorkSheet.Cells[0, 16].Value= "Defects Waiting Sch";
                //        //xlWorkSheet.Cells[0, 17].Value= "Defects in (dev)";
                //        //xlWorkSheet.Cells[0, 18].Value= "Pending Enhanc";
                //        //xlWorkSheet.Cells[0, 19].Value= "Product Management";
                //        //xlWorkSheet.Cells[0, 20].Value= "Destribution";
                //        //xlWorkSheet.Cells[0, 21].Value= "IT";
                //        //xlWorkSheet.Cells[0, 22].Value= "P_C_Waiting";
                //        //xlWorkSheet.Cells[0, 23].Value= "Consult Dispatch";
                //        //xlWorkSheet.Cells[0, 24].Value= "Billing Disputes";
                //        //xlWorkSheet.Cells[0, 25].Value= "ticket Age";
                //        #endregion 
                //        /// start fill the rows with the tickets number of rows will equal the number of thickets  +2 (header and footer that calculate total average)
                //        for (int y = 0; y <= ((AverageHandleTime)View.CurrentObject).Tickets.Count; y++)
                //        {
                //            int i = y + 1;
                //            /// I represent the row number started with y = 0 that mean row number 2 
                //            if (y == ((AverageHandleTime)View.CurrentObject).Tickets.Count)
                //            {
                //                /// while y = ticket count that mean all tickets already added to the queue so we need to fill the footer
                //                xlWorkSheet.Cells[y + 1, 0].Value = "Total Averages";
                //                int index = 3;
                //                /// loop on column and set the footer cell with the formula that calculate the averagefor this column 
                //                for (char column = 'E'; column <= 'Z'; column++)
                //                {
                //                    index ++ ;
                //                    if (column == 'Z')
                //                    {
                //                        char[] newcolumn = new char[2];
                //                        newcolumn[0] = 'A';
                //                        newcolumn[1] = 'A';
                //                        for (char onecolumn = newcolumn[1]; newcolumn[1] < 'B'; newcolumn[1]++)
                //                        {
                //                            index++;
                //                            string cell = char.ToString(newcolumn[0]) + char.ToString(newcolumn[1]);
                //                            string from = cell + 2;
                //                            string to = cell + (y+1);
                //                            //xlWorkSheet.Cells[i, "F"] = "=AVERAGE(F1,F2)";
                //                            xlWorkSheet.Cells[y + 1, index].Formula = "=AVERAGEIF(" + from + ":" + to + ","+"\">0\""+")";
                //                        }
                //                    }
                //                    else
                //                    {
                //                        string cell1 = char.ToString(column);
                //                        string from1 = char.ToString(column) + 2;
                //                        string to1 = char.ToString(column) + (y + 1);
                //                        //xlWorkSheet.Cells[i, "F"] = "=AVERAGE(F1,F2)";
                //                        xlWorkSheet.Cells[y + 1, index].Formula = "=Round(AVERAGE(" + from1 + ":" + to1 +"),0)";
                //                    }

                //                    //"=AVERAGE('" + from + "','" + to + "')";
                //                }
                //                xlWorkSheet.Range["A" + (y + 2) + ":AA" + (y + 2) + ""].FillColor=Color.Yellow ;
                //               xlWorkSheet.Range["A" + 2 + ":A" + (y + 1)].FillColor = Color.LightGray ;
                //            }
                //            /// if y is not equal tickets count that is mean not all tickets added so add this tickets 
                //            else
                //            {

                //                xlWorkSheet.Cells[i, 0].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].TiecketNum;
                //              //  xlWorkSheet.Cells[i, 1].Value= ((Udfenum)((AverageHandleTime)View.CurrentObject).Tickets[y].UDF).ToString();
                //                if (((AverageHandleTime)View.CurrentObject).Tickets[y].Account_profile != null)
                //                {
                //                    xlWorkSheet.Cells[i, 1].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].Account_profile.Name;
                //                }
                //                else
                //                {
                //                    xlWorkSheet.Cells[i, 1].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].Account;
                //                }
                //                xlWorkSheet.Cells[i, 2].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].Title;

                //                xlWorkSheet.Cells[i, 3].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].Completedate;
                //                xlWorkSheet.Cells[i, 3].NumberFormat = "m/d/yyyy";
                //                #region commented static AHT
                //                //xlWorkSheet.Cells[i, 4].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].Helpdesk;                          
                //                //xlWorkSheet.Cells[i, 5].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].CustomerCare;
                //                //xlWorkSheet.Cells[i, 6].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].CustomerAcceptance;
                //                //xlWorkSheet.Cells[i, 7].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].Sales;
                //                //xlWorkSheet.Cells[i, 8].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].QERP;
                //                //xlWorkSheet.Cells[i, 9].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].EDI;
                //                //xlWorkSheet.Cells[i, 10].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].CWA;
                //                //xlWorkSheet.Cells[i, 11].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].Html5;
                //                //xlWorkSheet.Cells[i, 12].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].Validation;
                //                //xlWorkSheet.Cells[i, 13].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].Review;
                //                //xlWorkSheet.Cells[i, 14].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].Sales;
                //                //xlWorkSheet.Cells[i, 15].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].PostalSale;
                //                //xlWorkSheet.Cells[i, 16].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].DefectsWaitingSch;
                //                //xlWorkSheet.Cells[i, 17].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].Defectsindev;
                //                //xlWorkSheet.Cells[i, 18].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].PendingEnhanc;
                //                //xlWorkSheet.Cells[i, 19].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].PMO;
                //                //xlWorkSheet.Cells[i, 20].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].Destribution;
                //                //xlWorkSheet.Cells[i, 21].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].IT;
                //                //xlWorkSheet.Cells[i, 22].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].P_C_Waiting;
                //                //xlWorkSheet.Cells[i, 23].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].ConsultDispatch;
                //                //xlWorkSheet.Cells[i, 24].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].BillingDisputes;
                //                //xlWorkSheet.Cells[i, 25].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].Ticketage;
                //                #endregion
                //                foreach( QueueName queuename in allqueues)
                //                {
                //                    QueueValues value = ((AverageHandleTime)View.CurrentObject).Tickets[y].QueuesValues.FirstOrDefault(x => x.Type == queuename);
                //                    if (value != null)
                //                    {
                //                        int columnnumber = allqueues.IndexOf(queuename) + 4;
                //                        xlWorkSheet.Cells[i, columnnumber].Value = value.Value;
                //                    }
                //                }
                //                xlWorkSheet.Cells[i, allqueues.Count + 4].Value = ((AverageHandleTime)View.CurrentObject).Tickets[y].Ticketage;
                //            }
                //        }
                //        xlWorkSheet.Columns["E"].ColumnWidth = 12;
                //        xlWorkSheet.Columns["A"].ColumnWidth = 20;
                //        xlWorkSheet.Columns["B"].ColumnWidth = 6;
                //        xlWorkSheet.Rows["1"].RowHeight = 37;
                //        xlWorkBook.Worksheets.Add("Departments AHT");
                //        Worksheet xldepWorkSheet = newspreadsheet.Document.Worksheets["Departments AHT"];
                //        xldepWorkSheet.Cells[0, 0].Value = "Dep./Que";
                //        xldepWorkSheet.Cells[0, 1].Value = "AHT";
                //        int itera = 1;
                //        foreach (DepartmentAHT DepAHT in ((AverageHandleTime)View.CurrentObject).DepartmentsAHT)
                //        {
                //            xldepWorkSheet[itera, 0].Value = DepAHT.Department.Id.ToString();
                //            xldepWorkSheet[itera, 1].Value = DepAHT.Value;
                //            itera++;
                //            if (((AverageHandleTime)View.CurrentObject).QueuesValues.Where(x => x.Type.Department == DepAHT.Department).Count() > 0)
                //            {
                //                //xldepWorkSheet.Cells[itera, 0].Value = "Queue.";
                //                //xldepWorkSheet.Cells[itera, 1].Value = "AHT";
                //                switch (DepAHT.Department.Id.ToString())
                //                {
                //                    case "Client Services":
                //                        xldepWorkSheet.Range["A" + itera + ":D" + itera+ ""].FillColor = Color.Purple;
                //                        xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].Font.Color = Color.White;
                //                        break;
                //                    case "Product":
                //                        xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].FillColor = Color.Orange;
                //                        xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].Font.Color = Color.White;
                //                        break;
                //                    case "Development":
                //                        xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].FillColor = Color.Gray;
                //                        xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].Font.Color = Color.White;

                //                        break;
                //                    case "IT":
                //                        xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].FillColor = Color.Blue;
                //                        xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].Font.Color = Color.White;
                //                        break;
                //                    default:
                //                        xldepWorkSheet.Range["A" + itera + ":D" + itera + ""].FillColor = Color.Gray;
                //                        break;
                //                }
                //            }
                //            foreach (QueueValues queueval in ((AverageHandleTime)View.CurrentObject).QueuesValues.Where(x=>x.Type.Department == DepAHT.Department))
                //            {

                //                xldepWorkSheet[itera, 0].Value = queueval.Type.Name.ToString();
                //                xldepWorkSheet[itera, 1].Value = queueval.Average;
                //                itera++;
                //            }


                #endregion
                // how to download the AHT report 
                AHTandDOTCalculation testobject = new AHTandDOTCalculation();
                System.Web.HttpResponse response = System.Web.HttpContext.Current.Response;
                response.Clear();
                response.Buffer = true;
                response.Charset = "";
                response.ContentType = "application/vnd.openxmlformats-     officedocument.spreadsheetml.sheet";
                response.AddHeader("Content-Disposition", "attachment;filename=" + ((AverageHandleTime)View.CurrentObject).Title.ToString() + ".xlsx");
                //response.BinaryWrite(xlWorkBook.SaveDocument(DocumentFormat.Xlsx));
                response.BinaryWrite(testobject.AHTattachement((AverageHandleTime)View.CurrentObject, ((AverageHandleTime)View.CurrentObject).Session));

                response.End();
                #region commented code that save microsoft excel file and get dictionary path
                ///<summary>
                /// microsoft excel sheet split and feez and save report 
                ///<summary>
                //xlWorkSheet.Activate();
                //xlWorkSheet.Application.ActiveWindow.SplitColumn = 1;
                //xlWorkSheet.Application.ActiveWindow.FreezePanes = true;
                //xlWorkBook.Close(true, misValue, misValue);
                //xlApp.Quit();
                //releaseObject(xlWorkSheet);
                //releaseObject(xlWorkBook);
                //releaseObject(xlApp);
                // string user = System.Environment.UserName;
                //xx.WorkDirectory = "C:\\";
                //xx.SettingsDocumentSelector.UploadSettings.Enabled = false;
                //var filePath = Path.Combine(xx.WorkDirectory, "BreakevenAnalysis.xlsx");
                //xx.Open(filePath);
                //string ahmed;
                //string path = System.Environment.GetFolderPath(System.Environment.SpecialFolder.CommonDocuments);
                //if (path.Contains("D:"))
                //{
                //  ahmed =  path.Replace("D:", "C:");
                // //   ahmed = ahmed.Replace("")
                //  System.IO.DirectoryInfo dir = new System.IO.DirectoryInfo(ahmed);
                //  System.Security.AccessControl.DirectorySecurity dd = new System.Security.AccessControl.DirectorySecurity();
                //  dir.Attributes = FileAttributes.Normal;
                //  xlWorkBook.SaveDocument(ahmed + "\\" + ((AverageHandleTime)View.CurrentObject).Title.ToString() + ".xlsx", DocumentFormat.Xlsx);
                //}
                #endregion
            }
            //  string userRoot = System.Environment.GetEnvironmentVariable("USERPROFILE");
            // string downloadFolder = Path.Combine(userRoot, "Downloads\\");
        }
            
            //else if (View.ObjectTypeInfo.Type == typeof(DeliveryOnTime))
            //{
               
            //}
       
        //ATA this action that download the AHT file from system admin 17/5/2016[End]
        
        //private void releaseObject(object obj)
        //{
        //    try
        //    {
        //        System.Runtime.InteropServices.Marshal.ReleaseComObject(obj);
        //        obj = null;
        //    }
        //    catch (Exception ex)
        //    {
            
        //    }
        //    finally
        //    {
        //        GC.Collect();
        //    }
        //}
        private void calculate_dot_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            //Aria5SystemAdmin.Module.Managers.AutoTaskIntegrationManger integration_object = new Aria5SystemAdmin.Module.Managers.AutoTaskIntegrationManger();
            if (View.ObjectTypeInfo.Type == typeof(DeliveryOnTime))
            {
                IObjectSpace object1 = Application.CreateObjectSpace();
                DeliveryOnTime DOT = ((DeliveryOnTime)View.CurrentObject);
                AHTandDOTCalculation AHtandotd = new AHTandDOTCalculation();
                //AHtandotd.getDOTdata(DOT.DateFrom, DOT.DateTO, ((XPObjectSpace)this.ObjectSpace).Session);
               // ((XPObjectSpace)this.ObjectSpace).CommitChanges();
                //integration_object.getDOTdata(DOT.DateFrom, DOT.DateTO, object1);
                AHtandotd.generateDOT(DOT.DateFrom, DOT.DateTO, ((XPObjectSpace)this.ObjectSpace).Session, DOT);
                ((XPObjectSpace)this.ObjectSpace).CommitChanges();
               // integration_object.get_Dot_by_date_range(DOT.DateFrom, DOT.DateTO, object1, DOT, false);
                View.Refresh();
                // this.calculate_dot.Active["0"] = false;
                // this.download_AHt_file.Active["0"] = true;
            }
        }
        private void Get_Task_Note_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
         
        }
        private void ShowTaskNotes_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {
            if (View.ObjectTypeInfo.Type == typeof(TrackingTask))
            {
                if (((TrackingTask)View.CurrentObject).Completedate != DateTime.MinValue)
                {
                    SqlConnection connection;
                    SqlCommand command;
                    SqlDataReader reader;
                    connection = new SqlConnection();
                  //  connection.ConnectionString = @"Data Source=NSDE_Khaled;Initial Catalog=Aria5SystemAdmin22;User ID=sa;Password=aria_123";
                connection.ConnectionString = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin_Test;User ID=sa;Password=aria_123";
                 //connection.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123";

                    connection.Open();
                    command = new SqlCommand();
                    command.Connection = connection;
                    command.CommandText = "delete  from TrackingTaskNote where [Task] = '" + ((TrackingTask)View.CurrentObject).Oid.ToString() + "'";
                    command.ExecuteReader();
                }
               
            }
        }
        private void ShowTaskNotes_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {

              Aria5SystemAdmin.Module.Managers.AutoTaskIntegrationManger tasknotes = new Managers.AutoTaskIntegrationManger();
              if (View.ObjectTypeInfo.Type == typeof(TrackingTask))
              {
                  tasknotes.gettasknote((TrackingTask)View.CurrentObject, ObjectSpace);
              }
             IObjectSpace objectSpace = Application.CreateObjectSpace();
           CollectionSource taskNotes =   new CollectionSource(objectSpace, typeof(TrackingTaskNote)) ;
            taskNotes.Criteria["Filter1"] = CriteriaOperator.Parse("[Task] ='"+((TrackingTask)View.CurrentObject).Oid.ToString()+"'");
            e.View = Application.CreateListView(Application.FindListViewId(typeof(TrackingTaskNote)),
            taskNotes , true);


        }
        private void DOT_download_Execute(object sender, SimpleActionExecuteEventArgs e) 
        {
            if (((DetailView)View).ViewEditMode == ViewEditMode.Edit)
            {
                throw new Exception("you can't download from edit mode please save and close then try to download !");
            }
            if(View.ObjectTypeInfo.Type == typeof(DeliveryOnTime))
            {
#region report 
                //ASPxSpreadsheet newspreadsheet = new ASPxSpreadsheet();
                //DeliveryOnTime DOTconfigration = ((DeliveryOnTime)View.CurrentObject);
                //IWorkbook xlWorkBook = newspreadsheet.Document;
                //xlWorkBook.Worksheets["Sheet1"].Visible = false;
                //System.Collections.ICollection listoftasks = DOTconfigration.Session.GetObjects(DOTconfigration.Session.Dictionary.GetClassInfo(typeof(TasksDOT)), CriteriaOperator.Parse("[TaskCompleteDate] >= '" + DOTconfigration.DateFrom + "'and [TaskCompleteDate] <= '" + DOTconfigration.DateTO + "' "), null, 2000, false, false);
                //IList<TasksDOT> AllTasks = listoftasks.Cast<TasksDOT>().ToList();
                //xlWorkBook.Worksheets.Add("Projects Tasks Data");
                //Worksheet xlalltaskssheet = xlWorkBook.Worksheets["Projects Tasks Data"];
                //xlalltaskssheet[0, 0].Value = "Project Name";
                //xlalltaskssheet[0, 1].Value = "Task Title";
                //xlalltaskssheet[0, 2].Value = "Resource";
                //xlalltaskssheet[0, 3].Value = "Department Name";
                //xlalltaskssheet[0, 4].Value = "Task Complete Date";
                //xlalltaskssheet[0, 5].Value = "Task Base Line";
                //xlalltaskssheet[0, 6].Value = "Sch.Var.";
                //if (AllTasks.Count > 0)
                //{
                //    int taskrow = 1;
                //    foreach (TasksDOT onetask in AllTasks)
                //    {
                //        xlalltaskssheet[taskrow, 0].Value = onetask.ProjectName;
                //        xlalltaskssheet[taskrow, 1].Value = onetask.TaskTitle;
                //        xlalltaskssheet[taskrow, 2].Value = onetask.ResourceName;
                //        xlalltaskssheet[taskrow, 3].Value = onetask.DepName;
                //        xlalltaskssheet[taskrow, 4].Value = onetask.TaskCompleteDate;
                //        xlalltaskssheet[taskrow, 5].Value = onetask.BaselineDueDate;
                //        xlalltaskssheet[taskrow, 6].Value = onetask.DeliveryOnTime;
                //        taskrow++;
                //    }

                //}
                //if (DOTconfigration.ProjectsDOT.Count > 0)
                //{
                //    xlWorkBook.Worksheets.Add("Projects OTD");
                //    xlWorkBook.Worksheets.Add("Analysis Sheet");
                //    Worksheet xlanalysisworksheet = xlWorkBook.Worksheets["Analysis Sheet"];
                //    xlanalysisworksheet[0, 0].Value = "Project Name";
                //    xlanalysisworksheet[0, 1].Value = "Delayed Task Title";
                //    xlanalysisworksheet[0, 2].Value = " Resource Name";
                //    xlanalysisworksheet[0, 3].Value = "Department";
                //    xlanalysisworksheet[0, 4].Value = "# of all tasks";
                //    xlanalysisworksheet[0, 5].Value = "# of Delayed Days";
                //    int xlanalysisrow = 1;
                //    Worksheet xlWorkSheet = xlWorkBook.Worksheets["Projects OTD"];
                //    xlWorkSheet.Cells[0, 0].Value = "Project Name";
                //    xlWorkSheet.Cells[0, 1].Value = "OTD Period Start Date";
                //    //xlWorkSheet.Cells[0, 2].Value = "Total Project Tasks (DOT) / Days";
                //    xlWorkSheet.Cells[0, 2].Value = "# of OTD Tasks";
                //    xlWorkSheet.Cells[0, 3].Value = "# of Tasks";
                //    //xlWorkSheet.Cells[0, 5].Value = "AVG Project Tasks (DOT) / Days";
                //    xlWorkSheet.Cells[0, 4].Value = "Tasks OTD %";

                //    for (int y = 0; y <= DOTconfigration.ProjectsDOT.Count; y++)
                //    {
                //        int i = y + 1;
                //        if (y == DOTconfigration.ProjectsDOT.Count)
                //        {
                //            xlWorkSheet.Cells[i, 0].Value = "Projects Totals";
                //            //xlWorkSheet.Cells[i, 5].Formula = "=ROUND(SUM(" + "C2" + ": C" + (i) + ") /SUM(" + "E2" + ": E" + (i) + "),0) ";
                //            xlWorkSheet.Cells[i, 4].Formula = "=ROUND(SUM(" + "C2" + ": C" + (i) + ") /SUM(" + "D2" + ": D" + (i) + ") *100,0)";
                //            xlWorkSheet.Range["A" + (i + 1) + ":G" + (i + 1) + ""].FillColor = Color.Yellow;
                //        }
                //        else
                //        {
                //            xlWorkSheet.Cells[i, 0].Value = (DOTconfigration.ProjectsDOT[y]).ProjectName;
                //            xlWorkSheet.Cells[i, 1].Value = (DOTconfigration.ProjectsDOT[y]).CreateDate;
                //            //xlWorkSheet.Cells[i, 2].Value = (DOTconfigration.ProjectsDOT[y]).Total;
                //            xlWorkSheet.Cells[i, 2].Value = (DOTconfigration.ProjectsDOT[y]).NumberofOTDTasks;
                //            xlWorkSheet.Cells[i, 3].Value = (DOTconfigration.ProjectsDOT[y]).Numberoftasks;
                //            //xlWorkSheet.Cells[i, 5].Value = (DOTconfigration.ProjectsDOT[y]).Deliveryontime;
                //            xlWorkSheet.Cells[i, 4].Value = (DOTconfigration.ProjectsDOT[y]).OTD;
                //            System.Collections.ICollection listofprojecttasks = DOTconfigration.Session.GetObjects(DOTconfigration.Session.Dictionary.GetClassInfo(typeof(TasksDOT)), CriteriaOperator.Parse(" ProjectNumber= '" + DOTconfigration.ProjectsDOT[y].Id + "'and[TaskCompleteDate] >= '" + DOTconfigration.DateFrom + "'and [TaskCompleteDate] <= '" + DOTconfigration.DateTO + "' "), null, 1000, false, false);
                //            IList<TasksDOT> projecttaks = listofprojecttasks.Cast<TasksDOT>().ToList();
                //            projecttaks = projecttaks.OrderBy(tk => tk.BaselineDueDate).ToList<TasksDOT>();
                //            double delayeddays = 0;
                //            if (projecttaks.Count > 0)
                //            {
                                
                //                xlanalysisworksheet[xlanalysisrow, 4].Value = projecttaks.Count;
                //                foreach (TasksDOT projecttask in projecttaks)
                //                {
                //                    if (projecttask.DeliveryOnTime > delayeddays)
                //                    {
                //                        xlanalysisworksheet[xlanalysisrow, 0].Value = (DOTconfigration.ProjectsDOT[y]).ProjectName;
                //                        xlanalysisworksheet[xlanalysisrow, 1].Value = projecttask.TaskTitle;
                //                        xlanalysisworksheet[xlanalysisrow, 2].Value = projecttask.ResourceName;
                //                        xlanalysisworksheet[xlanalysisrow, 3].Value = projecttask.DepName;
                //                        xlanalysisworksheet[xlanalysisrow, 5].Value = projecttask.DeliveryOnTime;
                //                        xlanalysisrow++;
                //                        delayeddays = projecttask.DeliveryOnTime;
                //                    }
                //                }
                //            }

                //        }

                //    }
                //    xlWorkSheet.Columns["A"].ColumnWidth = 20;
                //    xlWorkSheet.FreezeColumns(0);
                //    xlWorkSheet.FreezeRows(0);
                //    xlWorkSheet.Columns["B"].ColumnWidth = 6;
                //    xlWorkSheet.Rows["1"].RowHeight = 37;
                //}
                //if (DOTconfigration.ResourcesDOT.Count > 0)
                //{
                //    xlWorkBook.Worksheets.Add("Resources OTD");
                //    Worksheet xlWorkSheet1 = xlWorkBook.Worksheets["Resources OTD"];
                //    xlWorkSheet1.Cells[0, 0].Value = "Resource Name";
                //    xlWorkSheet1.Cells[0, 1].Value = "Department Name";
                //    xlWorkSheet1.Cells[0, 2].Value = "OTD Period Start Date";
                //    //xlWorkSheet1.Cells[0, 2].Value = "Total Resource Tasks (DOT) / Days";
                //    xlWorkSheet1.Cells[0, 3].Value = "# of OTD Tasks";
                //    xlWorkSheet1.Cells[0, 4].Value = "# of Tasks";
                //    //xlWorkSheet1.Cells[0, 5].Value = "Resource AVG. (DOT)";
                //    xlWorkSheet1.Cells[0, 5].Value = "Tasks OTD %";
                //    for (int y = 0; y <= DOTconfigration.ResourcesDOT.Count; y++)
                //    {
                //        int i = y + 1;
                //        if (y == DOTconfigration.ResourcesDOT.Count)
                //        {
                //            //xlWorkSheet1.Cells[i, 5].Formula = "=ROUND(SUM(" + "C2" + ": C" + (i) + ") /SUM(" + "E2" + ": E" + (i) + "),0) ";
                //            xlWorkSheet1.Cells[i, 5].Formula = "=ROUND(SUM(" + "D2" + ": D" + (i) + ") /SUM(" + "E2" + ": E" + (i) + ") *100,0)";
                //            xlWorkSheet1.Range["A" + (i + 1) + ":G" + (i + 1) + ""].FillColor = Color.Yellow;

                //        }
                //        else
                //        {
                //            if ((DOTconfigration.ResourcesDOT[y]).ResourceName != null)
                //            {
                //                xlWorkSheet1.Cells[i, 0].Value = (DOTconfigration.ResourcesDOT[y]).ResourceName.Name;
                //                if ((DOTconfigration.ResourcesDOT[y]).ResourceName.Department != null)
                //                {
                //                    xlWorkSheet1.Cells[i, 1].Value = (DOTconfigration.ResourcesDOT[y]).ResourceName.Department.Id;
                //                }
                //            }
                //            xlWorkSheet1.Cells[i, 2].Value = (DOTconfigration.ResourcesDOT[y]).CreateDate;
                //            //xlWorkSheet1.Cells[i, 2].Value = (DOTconfigration.ResourcesDOT[y]).Total;
                //            xlWorkSheet1.Cells[i, 3].Value = (DOTconfigration.ResourcesDOT[y]).NumberofOTDTasks;
                //            xlWorkSheet1.Cells[i, 4].Value = (DOTconfigration.ResourcesDOT[y]).NumberOfTasks;
                //            //xlWorkSheet1.Cells[i, 5].Value = (DOTconfigration.ResourcesDOT[y]).DOT;
                //            xlWorkSheet1.Cells[i, 5].Value = (DOTconfigration.ResourcesDOT[y]).OTD;

                //        }
                //    }
                //}
                //if (DOTconfigration.DepartmentsDOT.Count > 0)
                //{
                //    xlWorkBook.Worksheets.Add("Departments OTD");
                //    Worksheet xlWorkSheet2 = xlWorkBook.Worksheets["Departments OTD"];
                //    xlWorkSheet2.Cells[0, 0].Value = "Department Name";
                //    xlWorkSheet2.Cells[0, 1].Value = "OTD Period Start Date";
                //    //xlWorkSheet2.Cells[0, 2].Value = "Total Department Tasks (DOT) / Days";
                //    xlWorkSheet2.Cells[0, 2].Value = "# of OTD Tasks";
                //    xlWorkSheet2.Cells[0, 3].Value = "# of Tasks";
                //    //xlWorkSheet2.Cells[0, 5].Value = "Department AVG. (DOT)";
                //    xlWorkSheet2.Cells[0, 4].Value = "Tasks OTD %";
                //    for (int y = 0; y < DOTconfigration.DepartmentsDOT.Count; y++)
                //    {
                //        int i = y + 1;
                //        xlWorkSheet2.Cells[i, 0].Value = (DOTconfigration.DepartmentsDOT[y]).Department.Id;
                //        xlWorkSheet2.Cells[i, 1].Value = (DOTconfigration.DepartmentsDOT[y]).CreateDate;
                //        //xlWorkSheet2.Cells[i, 2].Value = (DOTconfigration.DepartmentsDOT[y]).Total;
                //        xlWorkSheet2.Cells[i, 2].Value = (DOTconfigration.DepartmentsDOT[y]).NumberofOTDTasks;
                //        xlWorkSheet2.Cells[i, 3].Value = (DOTconfigration.DepartmentsDOT[y]).NumberOfTasks;
                //        //xlWorkSheet2.Cells[i, 5].Value = (DOTconfigration.DepartmentsDOT[y]).DOT;
                //        xlWorkSheet2.Cells[i, 4].Value = (DOTconfigration.DepartmentsDOT[y]).OTD;

                //    }
                //}

#endregion 
                #region DOT Report
                //if (DOTconfigration.Tickets.Count > 0)
                //{
                //    xlWorkBook.Worksheets.Add("Tickets DOT");
                //    Worksheet xlWorkSheet = xlWorkBook.Worksheets["Tickets DOT"];
                //    xlWorkSheet.Range["A1:F1"].FillColor = Color.LightBlue;
                //    xlWorkSheet.Range["A1:F1"].Font.Color = Color.Black;
                //    xlWorkSheet.Cells[0, 0].Value = "Ticket Name";
                //    xlWorkSheet.Cells[0, 1].Value = "User Defiend field";
                //    xlWorkSheet.Cells[0, 2].Value = "Title";
                //    xlWorkSheet.Cells[0, 3].Value = "Account";
                //    xlWorkSheet.Cells[0, 4].Value = "Completed Date";
                //    xlWorkSheet.Cells[0, 5].Value = "Deliver On Time";
                //    for (int y = 0; y <= DOTconfigration.Tickets.Count; y++)
                //    {
                //        int i = y + 1;
                //        /// I represent the row number started with y = 0 that mean row number 2 
                //        int index = 4;
                //        if (y == DOTconfigration.Tickets.Count)
                //        {
                //            index++;
                //            /// while y = ticket count that mean all tickets already added to the queue so we need to fill the footer
                //            xlWorkSheet.Cells[y + 1, 0].Value = "Total Averages";
                //            string cell1 = "F";
                //            string from1 = "F" + 2;
                //            string to1 = "F" + (y + 1);
                //            //xlWorkSheet.Cells[i, "F"] = "=AVERAGE(F1,F2)";
                //            xlWorkSheet.Cells[y + 1, index].Formula = "=AVERAGE(" + from1 + ":" + to1 + ")";
                //            xlWorkSheet.Range["A" + (y + 2) + ":F" + (y + 2)].Fill.BackgroundColor = Color.Yellow;
                //            xlWorkSheet.Range["A" + 2 + ":A" + (y + 1)].Fill.BackgroundColor = Color.LightGray;
                //            // xlWorkSheet.Range["A" + 2 + ":A" + (y + 1)].Interior.Color = System.Drawing.ColorTranslator.ToOle(System.Drawing.Color.LightGray);
                //        }
                //        else
                //        {
                //            xlWorkSheet.Cells[i, 0].Value = DOTconfigration.Tickets[y].TiecketNum;
                //            xlWorkSheet.Cells[i, 1].Value = ((Udfenum)DOTconfigration.Tickets[y].UDF).ToString();
                //            xlWorkSheet.Cells[i, 2].Value = DOTconfigration.Tickets[y].Title;
                //            if (DOTconfigration.Tickets[y].Account_profile != null)
                //            {
                //                xlWorkSheet.Cells[i, 3].Value = (DOTconfigration).Tickets[y].Account_profile.Name;
                //            }
                //            else
                //            {
                //                xlWorkSheet.Cells[i, 3].Value = DOTconfigration.Tickets[y].Account;
                //            }
                //            xlWorkSheet.Cells[i, 4].Value = DOTconfigration.Tickets[y].Completedate;
                //            xlWorkSheet.Cells[i, 4].NumberFormat = "m/d/yyyy";
                //            xlWorkSheet.Cells[i, 5].Value = DOTconfigration.Tickets[y].DeliveryOnTime;
                //        }

                //    }

                //}
                #endregion
                //Email newone = new Email();
                //byte[] buffer = xlWorkBook.SaveDocument(DocumentFormat.Xlsx);
                //Stream attachemntasasstream = new MemoryStream(buffer);
                //System.Net.Mail.Attachment attach = new System.Net.Mail.Attachment(attachemntasasstream, ((DeliveryOnTime)View.CurrentObject).Title.ToString() + ".xlsx");
                //newone.Attachement = attach;
                //newone.FromEmail = "Khaled.m@ariasystems.biz";
                //newone.EmailPassword = "Kamag@2016";
                //newone.ToEmail = "ahmed.t@ariany.com";
                //newone.EmailTitle = "DOT report";
                //newone.SendEmail();
                //Worksheet worksheet = xlWorkBook.Worksheets.ActiveWorksheet;
                //worksheet.FreezeRows(0);
                //worksheet.FreezeColumns(0);
                AHTandDOTCalculation Object = new AHTandDOTCalculation();
                System.Web.HttpResponse response = System.Web.HttpContext.Current.Response;
                response.Clear();
                //response.ClearContent();
                //response.ClearHeaders();
                response.Buffer = true;
                response.Charset = "";
                response.ContentType = "application/vnd.openxmlformats-     officedocument.spreadsheetml.sheet";
                response.AddHeader("Content-Disposition", "attachment;filename=" + ((DeliveryOnTime)View.CurrentObject).Title.ToString() + ".xlsx");
                //response.BinaryWrite(xlWorkBook.SaveDocument(DocumentFormat.Xlsx));
                response.BinaryWrite(Object.DOTattachement(((DeliveryOnTime)View.CurrentObject)));
                response.End();
           
            }
        }
        private void Get_DOT_data_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            if (View.ObjectTypeInfo.Type == typeof(DeliveryOnTime))
            {
                Aria5SystemAdmin.Module.Managers.AutoTaskIntegrationManger integration_object = new Aria5SystemAdmin.Module.Managers.AutoTaskIntegrationManger();
                IObjectSpace object1 = Application.CreateObjectSpace();
                DeliveryOnTime DOT = ((DeliveryOnTime)View.CurrentObject);
                integration_object.getDOTdata(DOT.DateFrom, DOT.DateTO, object1);

                //Aria5SystemAdmin.Module.Managers.AHTandDOTCalculation OTDmanager = new Aria5SystemAdmin.Module.Managers.AHTandDOTCalculation();
                //OTDmanager.getDOTdata(DOT.DateFrom, DOT.DateTO,)
            }
        }
        //ATA Add this action to create phases separatly 
        private void CreateWBSPhase_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            QAWBS currentphase = (QAWBS)View.CurrentObject;
            if (currentphase.UseCasePoints.Project != null && currentphase.UseCasePoints.Project.AutoTaskID > 0)
            {
                //ATA check if the activities have resource or not 
                if (currentphase.QAActivities.Where(i => i.Resource == null).Count() == 0)
                {
                    foreach (QAWBS phase in currentphase.UseCasePoints.WBS.Where(x => x.Month < currentphase.Month))
                    {
                        if (phase.AutoTaskID == 0)
                        {
                           throw new Exception("This phase '" + phase.Month + "' should be created on autotask and completed before create this phase ");
                        }
                    }
                    AutoTaskIntegrationManger phasemanager = new AutoTaskIntegrationManger();
                    phasemanager.CreateNewPhasenew(currentphase.UseCasePoints.Project.AutoTaskID, currentphase);
                }
                else
                {
                    throw new Exception("this Phase Contain Activity without resource");
                }
                
            }
            else
            {
                throw new Exception("Please check the use case points project if exist check it's autotask id should be greater than 0 ");
            }
        }
        //ATA Add this action to create Tasks separatly 
        private void CreateTrackingtaskonautotask_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            
           
            if (View is DetailView)
            {
                createtaskonautotask((TrackingTask)View.CurrentObject);
            }
            else
            {
                foreach (object Selectedone in View.SelectedObjects)
                {
                    createtaskonautotask((TrackingTask )Selectedone);
                }
            }
        }
        
        public void createtaskonautotask(TrackingTask task)
        {
            AutoTaskIntegrationManger Taskmanager = new AutoTaskIntegrationManger();
            Taskmanager.CreateTaskNew(task.Project.AutoTaskID, task);
        }

        private void AHTForCompletedTickets_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            IObjectSpace object1 = Application.CreateObjectSpace();
            AHTandDOTCalculation newobj = new AHTandDOTCalculation();
            //ATa call the generate aht function that calc all the required aht instead call it one by one 1/19/2017 [start]
            newobj.calculateAHTForTickets((AverageHandleTime)View.CurrentObject, ((XPObjectSpace)this.ObjectSpace).Session);
            newobj.calculateQueueAHT((AverageHandleTime)View.CurrentObject, ((XPObjectSpace)this.ObjectSpace).Session);
            newobj.calculateDepartmentAHT((AverageHandleTime)View.CurrentObject, ((XPObjectSpace)this.ObjectSpace).Session);
            newobj.calculateResourceAHT((AverageHandleTime)View.CurrentObject, ((XPObjectSpace)this.ObjectSpace).Session);
            ((XPObjectSpace)this.ObjectSpace).Session.CommitTransaction();

            //ATa call the generate aht function that calc all the required aht instead call it one by one 1/19/2017 [End]

            object1.CommitChanges();
            View.Refresh();
            this.AHTForCompletedTickets.Active["0"] = false;
            this.AHTForNotCompletedTickets.Active["0"] = false;
            this.Calculate_AHT.Active["0"] = false;
            this.download_AHt_file.Active["0"] = true;
        }

        private void AHTForNotCompletedTickets_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            IObjectSpace object1 = Application.CreateObjectSpace();
            AHTandDOTCalculation newobj = new AHTandDOTCalculation();
            //ATa call the generate aht function that calc all the required aht instead call it one by one 1/19/2017 [start]
            newobj.calculateAHTFornotcompletedTickets((AverageHandleTime)View.CurrentObject, ((XPObjectSpace)this.ObjectSpace).Session);
            newobj.calculateQueueAHT((AverageHandleTime)View.CurrentObject, ((XPObjectSpace)this.ObjectSpace).Session);
            newobj.calculateDepartmentAHT((AverageHandleTime)View.CurrentObject, ((XPObjectSpace)this.ObjectSpace).Session);
            newobj.calculateResourceAHT((AverageHandleTime)View.CurrentObject, ((XPObjectSpace)this.ObjectSpace).Session);
            ((XPObjectSpace)this.ObjectSpace).Session.CommitTransaction();

            //ATa call the generate aht function that calc all the required aht instead call it one by one 1/19/2017 [End]

            object1.CommitChanges();
            View.Refresh();
            this.AHTForCompletedTickets.Active["0"] = false;
            this.AHTForNotCompletedTickets.Active["0"] = false;
            this.Calculate_AHT.Active["0"] = false;
            this.download_AHt_file.Active["0"] = true;
        }
    }
}
