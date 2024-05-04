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
using DevExpress.Web.ASPxSpreadsheet;
using System.IO;
using DevExpress.Spreadsheet;
using Aria5SystemAdmin.Module.BusinessObjects;
using System.Data;

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out https://documentation.devexpress.com/eXpressAppFramework/clsDevExpressExpressAppViewControllertopic.aspx.
    public partial class DefectDensityController : ViewController
    {
        public DefectDensityController()
        {
            InitializeComponent();
            // Target required Views (via the TargetXXX properties) and create their Actions.
        }
        protected override void OnActivated()
        {
            base.OnActivated();
            // Perform various tasks depending on the target View.
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
            
        }

        private void uploadfile_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            //ASPxSpreadsheet newspreadsheet = new ASPxSpreadsheet();
            //IWorkbook xlWorkBook = newspreadsheet.Document;

            //// Load a workbook from the stream. 
            //using (FileStream stream = new FileStream("Documents\\Document.xlsx", FileMode.Open))
            //{
            //    xlWorkBook.LoadDocument(stream, DocumentFormat.OpenXml);
            //}

            DefectDensityReport CurrentReport = ((DefectDensityReport)View.CurrentObject);
            switch (CurrentReport.Application.Id.ToString())
            {
                case "Aria3EDI":
                    calculatedensityfromapplicationfiles(CurrentReport);
                    break;
                case "Aria4XP":
                    calculatedensityfromapplicationfiles(CurrentReport);
                    break;
                default:
                    calculatedensityfromtracking(CurrentReport);
                    break;
            }
           
        }
        public void calculatedensityfromtracking(DefectDensityReport CurrentReport)
        {
            IList<TrackingEntry> alltracking = ObjectSpace.GetObjects<TrackingEntry>(CriteriaOperator.Parse("[Application] = '" + CurrentReport.Application.Oid + "'and [RequestedDate] >= '" + CurrentReport.DateFrom + "' and [RequestedDate] <= '" + CurrentReport.DateTo + "'"));
            int numberofbugs = alltracking.Where(x => x.Type == TrackingEntry.TrackingType.Bug).Count();
            double kloc = CurrentReport.Application.KlocVersions.Where(x => x.Date == CurrentReport.Application.KlocVersions.Max(r => r.Date)).FirstOrDefault().Kloc;
            DefectDensityvalues newdefectdensity = ObjectSpace.CreateObject<DefectDensityvalues>();
            newdefectdensity.Application = CurrentReport.Application;
            newdefectdensity.Date = CurrentReport.DateFrom;
            newdefectdensity.NumberOfBugs = numberofbugs;
            newdefectdensity.KLOC = kloc;
            newdefectdensity.DefectDensity =  Math.Round(numberofbugs / kloc,3) *100;
            newdefectdensity.Report = CurrentReport;
            newdefectdensity.Save();
            newdefectdensity.Session.CommitTransaction();
            ObjectSpace.CommitChanges();
        }
        public void calculatedensityfromapplicationfiles(DefectDensityReport CurrentReport)
        {

            IList<ApplicationDefectFiles> alldefectfiles= ObjectSpace.GetObjects<ApplicationDefectFiles>(CriteriaOperator.Parse("[Application] = '" + CurrentReport.Application.Oid + "'and [Date] >= '" + CurrentReport.DateFrom + "' and [Date] <= '" + CurrentReport.DateTo + "'"));
            double numberofbugs = 0;
            foreach (ApplicationDefectFiles file in alldefectfiles)
            {
                numberofbugs += file.Occurance;
            }
            double kloc = CurrentReport.Application.KlocVersions.Where(x => x.Date == CurrentReport.Application.KlocVersions.Max(r=>r.Date)).FirstOrDefault().Kloc;
            DefectDensityvalues newdefectdensity = ObjectSpace.CreateObject<DefectDensityvalues>();
            newdefectdensity.Application = CurrentReport.Application;
            newdefectdensity.Date = CurrentReport.DateFrom;
            newdefectdensity.NumberOfBugs = (int)numberofbugs;
            newdefectdensity.KLOC = kloc;
            newdefectdensity.DefectDensity =  Math.Round(numberofbugs / kloc, 3)  *100;
            newdefectdensity.Report = CurrentReport;
            newdefectdensity.Save();
            newdefectdensity.Session.CommitTransaction();
            ObjectSpace.CommitChanges();
        }

        private void getdefetfiles_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            DefectDensityFiles currentfile = ((DefectDensityFiles)View.CurrentObject);

                if (currentfile.File != null)
                {
                    DevExpress.Xpo.Metadata.ImageValueConverter x = new DevExpress.Xpo.Metadata.ImageValueConverter();
                    MemoryStream ms = new MemoryStream();
                    currentfile.File.SaveToStream(ms);
                    long length = ms.Length;
                    byte[] buffer = ms.ToArray();
                    Workbook workbook = new Workbook();

                    workbook.LoadDocument(buffer, DocumentFormat.Xlsx);
                    //workbook.Worksheets.Remove(workbook.Worksheets["Sheet1"]);
                    // string filename = File.FileName.Substring(0,File.FileName.Length-4);
                    Worksheet WorkBook = workbook.Worksheets.ActiveWorksheet;
                    int row = WorkBook.Rows.LastUsedIndex;
                    Range range = WorkBook.GetDataRange();
                    DataTable dataTable = new DataTable();// first.CreateDataTable(range, true);
                    for (int y = 1; y < row; y++)
                    {
                        Row item = WorkBook.Rows[y];

                        int rn = y + 1;
                        dataTable.Rows.Add();
                        for (int i = 0; i < 3; i++)
                        {
                            int dd = i + 1;
                            dataTable.Columns.Add();
                            dataTable.Rows[y - 1][i] = WorkBook[y, i].Value;

                        }

                    }
                    if (dataTable.Rows.Count > 0)
                    {
                        foreach (DataRow rw in dataTable.Rows)
                        {
                            ApplicationDefectFiles existfile = currentfile.Session.FindObject<ApplicationDefectFiles>(CriteriaOperator.Parse("[FileName] = '" + rw[0].ToString() + "' and[Date] = '"+currentfile.Date+"' "));
                            if (existfile == null)
                            {
                                ApplicationDefectFiles newFile = new ApplicationDefectFiles(currentfile.Session);
                                newFile.FileName = rw[0].ToString();
                                newFile.Occurance = double.Parse(rw[1].ToString());
                                newFile.TrakingEntries = rw[2].ToString();
                                newFile.Application = currentfile.Application;
                                newFile.Date = currentfile.Date;
                                newFile.DefectDensityFiles = currentfile;
                                newFile.Save();
                                newFile.Session.CommitTransaction();
                            }
                          
                        }

                    }
                
            }
        }
    }
}
