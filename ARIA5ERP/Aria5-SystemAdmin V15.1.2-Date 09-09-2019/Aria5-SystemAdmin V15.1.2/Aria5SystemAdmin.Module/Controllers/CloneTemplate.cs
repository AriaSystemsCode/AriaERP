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
using DevExpress.ExpressApp.Web.SystemModule;
using Aria5SystemAdmin.Module.BusinessObjects;
using System.IO;
using DevExpress.Web.ASPxSpreadsheet;
using DevExpress.Spreadsheet;
using System.Data;
namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out https://documentation.devexpress.com/eXpressAppFramework/clsDevExpressExpressAppViewControllertopic.aspx.
    //ATA add this controller to enable clone usecasepoints from usecasepoints template and link it to the project [start] 
    public partial class CloneTemplate : WebModificationsController
    {
        Guid projectoid;
        public CloneTemplate()
        {
            InitializeComponent();
            // Target required Views (via the TargetXXX properties) and create their Actions.

        }
        protected override void OnActivated()
        {
            base.OnActivated();


        }
        private void ObjectSpace_ObjectChanged(object sender, ObjectManipulatingEventArgs e)
        {

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
        protected override void Save(SimpleActionExecuteEventArgs args)
        {
            //ProcessSelectedObjects(args.SelectedObjects);
            //ATA add this check and reload to solve the problem of 
            //(The object you are trying to save was changed by another user. Please refresh data. ClassName: Aria5SystemAdmin.Module.BusinessObjects.Account. ID: 10272a1e-cb80-4c8a-9e3a-b4cb4334814c)
            //2/16/2017 [start]
            foreach (object obj in this.View.ObjectSpace.ModifiedObjects)
            {
                DevExpress.Persistent.BaseImpl.BaseObject obj1 = obj as DevExpress.Persistent.BaseImpl.BaseObject;
                if (obj1 != null)
                {
                    if (this.View.CurrentObject != obj1)
                        obj1.Reload();
                }
                
            }
            //ATA add new method to check employee if qualified or not 4/10/2017
            if (args.CurrentObject.GetType() == typeof(HREmployee) && ((HREmployee)args.CurrentObject).EmploymentStatus == Employment_status.Active)
            {
                SaveEmployee(((HREmployee)args.CurrentObject));
            }

            // Doaa 05/01/2019 {Start}
            if(args.CurrentObject.GetType() == typeof(TrackingEntry))
            {
                if(string.IsNullOrEmpty(((TrackingEntry)args.CurrentObject).ReferenceNo))
                {
                    throw new Exception("Auto-Task Reference # can't be empty");
                }
            }
            // Doaa 05/01/2019 {End}
           
                //ATA 2/16/2017  [End]
                base.Save(args);
            
            if (args.CurrentObject.GetType() == typeof(ProjectTemplate))
            {
                if (((ProjectTemplate)args.CurrentObject).UseCasePoints == null && ((ProjectTemplate)args.CurrentObject).Type != ProjectTemplate.ProjectType.Key)
                {
                    IObjectSpace objectSpace = Application.CreateObjectSpace();
                    string listViewId = Application.FindListViewId(typeof(QAUseCasePoints));
                    CollectionSourceBase usecasepoints = Application.CreateCollectionSource(objectSpace, typeof(QAUseCasePoints), listViewId, CollectionSourceDataAccessMode.Client, CollectionSourceMode.Normal);
                    if (((ProjectTemplate)View.CurrentObject).Type == ProjectTemplate.ProjectType.Key)
                    {
                        usecasepoints.Criteria["filter1"] = CriteriaOperator.Parse("[Title] = 'Key Template'");
                    }
                    else if (((ProjectTemplate)View.CurrentObject).Type == ProjectTemplate.ProjectType.Custom)
                    {
                        usecasepoints.Criteria["filter1"] = CriteriaOperator.Parse("[Title] = 'Custome Template'");
                    }
                    else
                    {
                        usecasepoints.Criteria["filter1"] = CriteriaOperator.Parse("[Title] = 'Standard Template'");
                    }
                    args.ShowViewParameters.CreatedView = Application.CreateListView(
                       listViewId,
                     usecasepoints,
                       true);
                    DialogController clonetemplate1 = Application.CreateController<DialogController>();
                    clonetemplate1.Accepting += clonetemplate1_Accepting;
                    clonetemplate1.Cancelling += clonetemplate1_Cancelling;
                    args.ShowViewParameters.TargetWindow = TargetWindow.NewWindow;
                    args.ShowViewParameters.Controllers.Add(clonetemplate1);
                    projectoid = ((ProjectTemplate)View.CurrentObject).Oid;
                }

            }
           


        }
        protected override void SaveAndClose(SimpleActionExecuteEventArgs args)
        {
            //ATA add this check and reload to solve the problem of 
            //(The object you are trying to save was changed by another user. Please refresh data. ClassName: Aria5SystemAdmin.Module.BusinessObjects.Account. ID: 10272a1e-cb80-4c8a-9e3a-b4cb4334814c)
            //2/16/2017 [start]
            foreach (object obj in this.View.ObjectSpace.ModifiedObjects)
            {
                DevExpress.Persistent.BaseImpl.BaseObject obj1 = obj as DevExpress.Persistent.BaseImpl.BaseObject;
                if (obj1 != null)
                {
                    if (this.View.CurrentObject != obj1)
                        obj1.Reload();
                }

            }
            //ATA add new method to check employee if qualified or not 4/10/2017
            if (args.CurrentObject.GetType() == typeof(HREmployee) && ((HREmployee)args.CurrentObject).EmploymentStatus == Employment_status.Active)
            {
                SaveEmployee(((HREmployee)args.CurrentObject));
            }
            //ATA 2/16/2017  [End]
            base.SaveAndClose(args);

            if (args.CurrentObject.GetType() == typeof(ProjectTemplate))
            {
                if (((ProjectTemplate)args.CurrentObject).UseCasePoints == null && ((ProjectTemplate)args.CurrentObject).Type != ProjectTemplate.ProjectType.Key)
                {
                    IObjectSpace objectSpace = Application.CreateObjectSpace();
                    string listViewId = Application.FindListViewId(typeof(QAUseCasePoints));
                    CollectionSourceBase usecasepoints = Application.CreateCollectionSource(objectSpace, typeof(QAUseCasePoints), listViewId, CollectionSourceDataAccessMode.Client, CollectionSourceMode.Normal);
                    if (((ProjectTemplate)View.CurrentObject).Type == ProjectTemplate.ProjectType.Key)
                    {
                        usecasepoints.Criteria["filter1"] = CriteriaOperator.Parse("[Title] = 'Key Template'");
                    }
                    else if (((ProjectTemplate)View.CurrentObject).Type == ProjectTemplate.ProjectType.Custom)
                    {
                        usecasepoints.Criteria["filter1"] = CriteriaOperator.Parse("[Title] = 'Custome Template'");
                    }
                    else
                    {
                        usecasepoints.Criteria["filter1"] = CriteriaOperator.Parse("[Title] = 'Standard Template'");
                    }
                    args.ShowViewParameters.CreatedView = Application.CreateListView(
                       listViewId,
                     usecasepoints,
                       true);
                    DialogController clonetemplate1 = Application.CreateController<DialogController>();
                    clonetemplate1.Accepting += clonetemplate1_Accepting;
                    clonetemplate1.Cancelling += clonetemplate1_Cancelling;
                    args.ShowViewParameters.TargetWindow = TargetWindow.NewWindow;
                    args.ShowViewParameters.Controllers.Add(clonetemplate1);
                    projectoid = ((ProjectTemplate)args.CurrentObject).Oid;
                }

            }
             if (args.CurrentObject.GetType() == typeof(ImportRequirement))
            {
                Importrequirement((ImportRequirement)args.CurrentObject);
            }

        }
        protected override void SaveAndNew(SingleChoiceActionExecuteEventArgs args)
        {
            //ATA add this check and reload to solve the problem of 
            //(The object you are trying to save was changed by another user. Please refresh data. ClassName: Aria5SystemAdmin.Module.BusinessObjects.Account. ID: 10272a1e-cb80-4c8a-9e3a-b4cb4334814c)
            //2/16/2017 [start]
            foreach (object obj in this.View.ObjectSpace.ModifiedObjects)
            {
                DevExpress.Persistent.BaseImpl.BaseObject obj1 = obj as DevExpress.Persistent.BaseImpl.BaseObject;
                if (obj1 != null)
                {
                    if (this.View.CurrentObject != obj1)
                        obj1.Reload();
                }

            }
            //ATA add new method to check employee if qualified or not 4/10/2017
            if (args.CurrentObject.GetType() == typeof(HREmployee) && ((HREmployee)args.CurrentObject).EmploymentStatus == Employment_status.Active)
            {
                SaveEmployee(((HREmployee)args.CurrentObject));
            }
            //ATA 2/16/2017  [End]
            base.SaveAndNew(args);
          //  this.View.RefreshDataSource();
            if (args.CurrentObject.GetType() == typeof(ProjectTemplate))
            {
                if (((ProjectTemplate)args.CurrentObject).UseCasePoints == null && ((ProjectTemplate)args.CurrentObject).Type != ProjectTemplate.ProjectType.Key)
                {
                    IObjectSpace objectSpace = Application.CreateObjectSpace();
                    string listViewId = Application.FindListViewId(typeof(QAUseCasePoints));
                    CollectionSourceBase usecasepoints = Application.CreateCollectionSource(objectSpace, typeof(QAUseCasePoints), listViewId, CollectionSourceDataAccessMode.Client, CollectionSourceMode.Normal);
                    if (((ProjectTemplate)View.CurrentObject).Type == ProjectTemplate.ProjectType.Key)
                    {
                        usecasepoints.Criteria["filter1"] = CriteriaOperator.Parse("[Title] = 'Key Template'");
                    }
                    else if (((ProjectTemplate)View.CurrentObject).Type == ProjectTemplate.ProjectType.Custom)
                    {
                        usecasepoints.Criteria["filter1"] = CriteriaOperator.Parse("[Title] = 'Custome Template'");
                    }
                    else
                    {
                        usecasepoints.Criteria["filter1"] = CriteriaOperator.Parse("[Title] = 'Standard Template'");
                    }
                    args.ShowViewParameters.CreatedView = Application.CreateListView(
                       listViewId,
                     usecasepoints,
                       true);
                    DialogController clonetemplate1 = Application.CreateController<DialogController>();
                    clonetemplate1.Accepting += clonetemplate1_Accepting;
                    clonetemplate1.Cancelling += clonetemplate1_Cancelling;
                    args.ShowViewParameters.TargetWindow = TargetWindow.NewWindow;
                    args.ShowViewParameters.Controllers.Add(clonetemplate1);
                    projectoid = ((ProjectTemplate)args.CurrentObject).Oid;
                }
            }

        }

        private void SaveEmployee(HREmployee Emp)
        {
             if (Emp.JobPosition != null)
            {
                if (Emp.JobPosition.Requirements.Count > 0)
                {
                    if (Emp.JobPosition.Requirements.Count > Emp.Qualifications.Count)
                    {
                        throw new Exception("The JobPosition requirements not match the employee qualifications please review it then try to change this EMployee JobPosition");
                    }
                    else
                    {
                        foreach (HRSkillInstance Skill in Emp.JobPosition.Requirements)
                        {
                            HRSkillInstance exist = Emp.Qualifications.FirstOrDefault(x => x.Skill == Skill.Skill);
                            if (exist == null)
                            {
                                throw new Exception(string.Format("This Skill {0} is not found at the Employee Qualifications", Skill.Skill.Name));
                            }
                            else
                            {
                                if (exist.Type == Skillstype.CertificateandTraining || exist.Type == Skillstype.AcademicDegree)
                                {
                                    if (exist.Completed != Skill.Completed)
                                    {
                                        throw new Exception(string.Format("This {0} {1} Should be Completed", Skill.Type.ToString(),Skill.Skill.Name));

                                    }
                                }
                                else
                                {
                                    if (exist.Level == null || (int)exist.Level.Value < (int)Skill.Level.Value)
                                    {
                                        throw new Exception(string.Format("This Skill {1} Should be at the  level {0} like the jobposition requirements",Skill.Level.ToString(),Skill.Skill.Name));

                                    }
                                }
                            }
                           
                        }
                    }
                }
            }
        }

        void clonetemplate1_Cancelling(object sender, EventArgs e)
        {
            //  DevExpress.XtraEditors.XtraMessageBox.Show("cancelled");
            //throw new NotImplementedException();
        }

        void clonetemplate1_Accepting(object sender, DialogControllerAcceptingEventArgs e)
        {
            //DevExpress.XtraEditors.XtraMessageBox.Show("Acceptance done ");
            //throw new NotImplementedException();
            ProcessSelectedObjects(e.AcceptActionArgs.SelectedObjects, projectoid);



        }

        private void ProcessSelectedObjects(System.Collections.IList list, Guid project_OID)
        {
            if (project_OID != Guid.Empty)
            {
                foreach (object UCP in list)
                {
                    QAUseCasePoints testcloning = UCP as QAUseCasePoints;

                    if (UCP is QAUseCasePoints)
                    {
                        QAUseCasePoints Newusecase1 = new QAUseCasePoints(testcloning.Session, testcloning);
                        Newusecase1.Save();
                        Newusecase1.Session.CommitTransaction();
                        ProjectTemplate thisproject = ObjectSpace.FindObject<ProjectTemplate>(CriteriaOperator.Parse("[Oid] = '" + project_OID.ToString() + "'"));
                        QAUseCasePoints Newusecase = ObjectSpace.FindObject<QAUseCasePoints>(CriteriaOperator.Parse("[Oid] = '" + (Newusecase1.Oid).ToString() + "'"));
                        Newusecase.Session.LockingOption = DevExpress.Xpo.LockingOption.None;
                        Newusecase.Project = thisproject;
                        Newusecase.Title = thisproject.Name + "Use Case Points";
                        Newusecase.Save();
                        Newusecase.Session.CommitTransaction();
                        thisproject.UseCasePoints = Newusecase;
                        thisproject.Save();
                        thisproject.Session.CommitTransaction();
                        //((ProjectTemplate)View.CurrentObject).UseCasePoints = Newusecase;
                        //((ProjectTemplate)View.CurrentObject).Save();
                        //((ProjectTemplate)View.CurrentObject).Session.CommitTransaction();
                    }
                }
            }
        }

        public void Importrequirement(ImportRequirement file)
        {
            MemoryStream ms = new MemoryStream();
            file.File.SaveToStream(ms);
            byte[] buffer = ms.ToArray();

            ASPxSpreadsheet xsheet = new ASPxSpreadsheet();
            IWorkbook workbook = xsheet.Document;

            workbook.LoadDocument(buffer, DocumentFormat.Xlsx);
            //workbook.Worksheets.Remove(workbook.Worksheets["Sheet1"]);
            // string filename = File.FileName.Substring(0,File.FileName.Length-4);
            Worksheet WorkSheet = workbook.Worksheets.ActiveWorksheet;
            int row = WorkSheet.Rows.LastUsedIndex;
            //   Range range = WorkBook.GetDataRange();
            DataTable dataTable = new DataTable();// first.CreateDataTable(range, true);
            for (int y = 1; y <= row; y++)
            {
                Row item = WorkSheet.Rows[y];

                int rn = y + 1;
                dataTable.Rows.Add();
                for (int i = 0; i < 6; i++)
                {
                    int dd = i + 1;
                    dataTable.Columns.Add();
                    dataTable.Rows[y - 1][i] = WorkSheet[y, i].Value;

                }

            }
            if (dataTable.Rows.Count > 0)
            {
                Application_T selectedApp =  ObjectSpace.FindObject<Application_T>(CriteriaOperator.Parse("[Oid] ='"+file.Application.Oid+"' "));
                Account AriaAccount = ObjectSpace.FindObject<Account>(CriteriaOperator.Parse("[Name] ='Aria Systems INC.' "));
                
                ProjectTemplate project = ObjectSpace.FindObject<ProjectTemplate>(CriteriaOperator.Parse("Name = 'Aria3EDI Standard Testing Cases'"));
                for (int i = 0; i < dataTable.Rows.Count; i++)
                {

                    string Title = dataTable.Rows[i][0].ToString();
                    string refrence = dataTable.Rows[i][1].ToString();
                    string type = dataTable.Rows[i][2].ToString();
                    string Moscow = dataTable.Rows[i][3].ToString();
                    string Priority = dataTable.Rows[i][4].ToString();
                    string Desc = dataTable.Rows[i][5].ToString();
                    if (Title != string.Empty)
                    {
                        Requirement Newreq = ObjectSpace.CreateObject<Requirement>();
                        Newreq.Title = Title;
                        Newreq.Refrence = refrence;
                        Newreq.RequirementMoscow = (Requirement.Moscow)Enum.Parse(typeof(Requirement.Moscow), Moscow);
                        Newreq.RequirementPriorty = (Requirement.Priority)Enum.Parse(typeof(Requirement.Priority), Priority);
                        Newreq.RequirementStatus = Requirement.RequirmentsStatus.New;
                        Newreq.Description = Desc;
                        Newreq.RequirementType = ObjectSpace.FindObject<RequirementType>(CriteriaOperator.Parse("[Name] ='" + type + "' "));
                        Newreq.Application_T = selectedApp;
                        Newreq.Account = AriaAccount;
                        Newreq.Save();
                    }
                    //    newobject.ObjectName = name;
                    //    newobject.ObjectType = sess.FindObject<ObjectType>(CriteriaOperator.Parse("Name = '" + dataTable.Rows[i][4].ToString() + "'"));
                    //    if (dataTable.Rows[i][1].ToString() != "")
                    //    {
                    //        basetestcase = sess.FindObject<AriaObject>(CriteriaOperator.Parse("ObjectID = '" + dataTable.Rows[i][1].ToString() + "'"));
                    //        if (basetestcase != null)
                    //        {
                    //            newobject.ParentObjectID = basetestcase;
                    //        }
                    //    }
                    //    else if (sess.FindObject<AriaObject>(CriteriaOperator.Parse("ObjectID = '" + dataTable.Rows[i][1].ToString() + "'")) != null)
                    //    {
                    //        // baseparent = sess.FindObject<AriaObject>(CriteriaOperator.Parse("ObjectID = '" + dataTable.Rows[i][1].ToString() + "'"));
                    //        if (basetestcase != null)
                    //        {
                    //            newobject.ParentObjectID = basetestcase;
                    //        }
                    //    }

                    //    newobject.Save();
                    //    if (newobject.AriaObjectSettings.Count > 0)
                    //    {
                    //        AriaObjectSetting Storagefile = newobject.AriaObjectSettings.Where(x => x.SettingType.Name == "Storage File Name").FirstOrDefault();
                    //        if (Storagefile != null)
                    //        {
                    //            Storagefile.Value = dataTable.Rows[i][2].ToString();
                    //            Storagefile.Save();
                    //        }
                    //        AriaObjectSetting subfolder = newobject.AriaObjectSettings.Where(x => x.SettingType.Name == "Sub Folder").FirstOrDefault();
                    //        if (subfolder != null)
                    //        {
                    //            subfolder.Value = dataTable.Rows[i][3].ToString();
                    //            subfolder.Save();
                    //        }
                    //    }
                    //    for (int childs = (i + 1); childs < (i + 3); childs++)
                    //    {
                    //        if (childs != dataTable.Rows.Count)
                    //        {
                    //            AriaObject newobject1 = new AriaObject(sess);
                    //            string name1 = dataTable.Rows[childs][0].ToString();
                    //            newobject1.ObjectName = name1;
                    //            newobject1.ObjectType = sess.FindObject<ObjectType>(CriteriaOperator.Parse("Name = '" + dataTable.Rows[childs][4].ToString() + "'"));
                    //            newobject1.ParentObjectID = newobject;
                    //            newobject1.Save();
                    //            if (newobject1.AriaObjectSettings.Count > 0)
                    //            {
                    //                AriaObjectSetting Storagefile = newobject1.AriaObjectSettings.Where(x => x.SettingType.Name == "Storage File Name").FirstOrDefault();
                    //                if (Storagefile != null)
                    //                {
                    //                    Storagefile.Value = dataTable.Rows[childs][2].ToString();
                    //                    Storagefile.Save();
                    //                }
                    //                AriaObjectSetting subfolder = newobject1.AriaObjectSettings.Where(x => x.SettingType.Name == "Sub Folder").FirstOrDefault();
                    //                if (subfolder != null)
                    //                {
                    //                    subfolder.Value = dataTable.Rows[childs][3].ToString();
                    //                    subfolder.Save();
                    //                }
                    //            }
                    //        }
                    //    }
                    //    i = i + 2;
                }
                ObjectSpace.CommitChanges();
            }
        }
    }
    //ATA add this controller to enable clone usecasepoints from usecasepoints template and link it to the project [End] 

}
