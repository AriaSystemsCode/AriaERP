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
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.ExpressApp.Security;
using DevExpress.Xpo;
using DevExpress.ExpressApp.Web.Editors.ASPx;
//using DevExpress.Web;
using DevExpress.ExpressApp.DC;
using System.ServiceModel;
using DevExpress.ExpressApp.Web;
using System.Collections;
using DevExpress.ExpressApp.Web.SystemModule;
using DevExpress.ExpressApp.Model;
using DevExpress.Xpo.Metadata;
using DevExpress.Web;
namespace Aria5SystemAdmin.Module.Controllers
{
    /*
    ATA,1,21/3/2016 add a new controller to Check base line in specific business object views and disable or enable edit and delete actions ,
     according to end date propperty  
    */
     // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class Baseline : ViewController
    {
        private AriaSecuritySystemUser user;
        private string userName;
              
        
        public Baseline()
        {
            InitializeComponent();
            RegisterActions(components);
            // Target required Views (via the TargetXXX properties) and create their Actions.
            //View.SelectionChanged += View_SelectionChanged;

        }

      #region "Commented"

//        protected override void OnActivated()
//        {
//            base.OnActivated();
//            // Perform various tasks depending on the target View.

//            WebWindow.CurrentRequestWindow.PagePreRender += CurrentRequestWindow_PagePreRender;



//        }




//        void CurrentRequestWindow_PagePreRender(object sender, EventArgs e)
//        {
//            if (View is ListView)
//            {

//                if (View.ObjectTypeInfo.Type == typeof(QAUseCase) || View.ObjectTypeInfo.Type == typeof(QAMainFeature) || View.ObjectTypeInfo.Type == typeof(TestCase)
//                          || View.ObjectTypeInfo.Type == typeof(TestRun) || View.ObjectTypeInfo.Type == typeof(DesignDetailEstimate))
//                {
//                    ASPxGridListEditor gridListEditor = ((ListView)View).Editor as ASPxGridListEditor;
//                    if (gridListEditor.Grid != null)
//                    {
//                        foreach (GridViewColumn column in gridListEditor.Grid.Columns)
//                        {

//                            if (column is GridViewDataActionColumn && ((GridViewDataActionColumn)column).Action.Id == "Edit")
//                            {
//                                column.Visible = false;

//                            }
//                        }
//                    }
//                }
//                else
//                {
//                    ASPxGridListEditor gridListEditor = ((ListView)View).Editor as ASPxGridListEditor;
//                    if (gridListEditor.Grid != null)
//                    {
//                        foreach (GridViewColumn column in gridListEditor.Grid.Columns)
//                        {

//                            if (column is GridViewDataActionColumn && ((GridViewDataActionColumn)column).Action.Id == "Edit")
//                            {
//                                column.Visible = true;

//                            }
//                        }
//                    }


//                }
//            }

//        }
//        protected override void OnViewControlsCreated()
//        {
//            base.OnViewControlsCreated();
//            // Access and customize the target View control.
//        }
//        protected override void OnDeactivated()
//        {
//            // Unsubscribe from previously subscribed events and release other references and resources.
//            base.OnDeactivated();
//            WebWindow.CurrentRequestWindow.PagePreRender -= CurrentRequestWindow_PagePreRender;

//        }

//        private void Baseline_Activated(object sender, EventArgs e)
//        {
//            userName = ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters).UserName;
//            CriteriaOperator criteriaUser = CriteriaOperator.Parse("UserName = '" + userName + "'");
//            user = ObjectSpace.FindObject<AriaSecuritySystemUser>(criteriaUser);



//            if (user != null && userName.ToUpper().Trim() != "ADMIN" && user.Account.Id == "Aria")
//            {
//                var currentobject = View.CurrentObject;

//                if (View is DetailView)
//                {
//                    action(currentobject, View);
//                    //if (View.CurrentObject is QAUseCase)
//                    //{
//                    //   ITypeInfo ahmed = this.Application.TypesInfo.FindTypeInfo(View.ObjectTypeInfo.Type.ToString());
//                    //  // DevExpress.Xpo.Metadata.XPClassInfo ahmed = XafTypesInfo.XpoTypeInfoSource.XPDictionary.GetClassInfo(View.ObjectTypeInfo.Type);
//                    //   Attribute newatt = new DevExpress.ExpressApp.Model.ModelDefaultAttribute("AllowEdit", "false");
//                    //   IMemberInfo property = ahmed.FindMember("Enddate");
//                    //   if (property != null)
//                    //   {
//                    ////   ((QAUseCase)( View.CurrentObject)).Enddate.
//                    //       ModelDefaultAttribute x= new ModelDefaultAttribute("AllowEdit", "false");

//                    //  //     Attribute newatt1 =x((ModelDefaultAttribute).GetAttributeInfo(typeof(ModelDefaultAttribute)) 
//                    //           //new DevExpress.Xpo.CustomAttribute("AllowEdit", "false");

//                    //    //   IList<Attribute> attcollection = new List<Attribute>(property.FindAttributes<Attribute>(true));

//                    //       property.AddAttribute(x);


//                    //   }
//                    //   ((XafMemberInfo)property).Refresh();
//                    //}
//                }
//                else
//                {
//                    ListViewController controller = Frame.GetController<ListViewController>();
//                    if (View.ObjectTypeInfo.Type == typeof(QAUseCase) || View.ObjectTypeInfo.Type == typeof(QAMainFeature) || View.ObjectTypeInfo.Type == typeof(TestCase)
//                               || View.ObjectTypeInfo.Type == typeof(TestRun) || View.ObjectTypeInfo.Type == typeof(DesignDetailEstimate) || View.ObjectTypeInfo.Type == typeof(QAUseCasePoints))
//                    {
//                        if (View.Model.AllowEdit == false)
//                        {
//                            ((ListView)View).AllowDelete["1"] = false;
//                            //((ListView)View).AllowEdit["1"] = false;

//                            controller.EditAction.Active.SetItemValue("EditableListView", false);
//                        }
//                    }
//                    else
//                    {
//                        ((ListView)View).AllowDelete["1"] = true;
//                        controller.EditAction.Active.SetItemValue("EditableListView", true);
//                    }

//                }

//            }
//            else
//            {



//            }

//        }

//        public static void permission(DateTime date, View currentView)
//        {
//            if (DateTime.Today > date)
//            {
//                currentView.AllowEdit["1"] = false;
//                currentView.AllowDelete["1"] = false;
//            }
//            else
//            {
//                currentView.AllowEdit["1"] = true;
//                currentView.AllowDelete["1"] = true;
//            }
//        }
//        public static void action(object new_object, View CurrentView)
//        {
//            if (new_object is QAUseCase)
//            {
//                permission(((QAUseCase)new_object).Enddate, CurrentView);
//            }
//            else if (new_object is QAMainFeature)
//            {
//                permission(((QAMainFeature)new_object).Enddate, CurrentView);
//            }
//            else if (new_object is DesignDetailEstimate)
//            {
//                permission(((DesignDetailEstimate)new_object).Enddate, CurrentView);
//            }
//            else if (new_object is TestCase)
//            {
//                permission(((TestCase)new_object).Enddate, CurrentView);
//            }
//            else if (new_object is TestRun)
//            {
//                permission(((TestRun)new_object).Enddate, CurrentView);
//            }
//            else if (new_object is QAUseCasePoints)
//            {
//                permission(((QAUseCasePoints)new_object).Enddate, CurrentView);
//            }
//            else
//            {
//                CurrentView.AllowEdit["1"] = true;
//                CurrentView.AllowDelete["1"] = true;
//            }

//        }

//        private void Baseline_AfterConstruction(object sender, EventArgs e)
//        {
//        }

//        private void Baseline_ViewControlsCreated(object sender, EventArgs e)
//        {
//            if (View is ListView)
//            {
//                ASPxGridListEditor listeditor = ((ListView)View).Editor as ASPxGridListEditor;


//                //listeditor.AllowEditChanged += listeditor_AllowEditChanged;

//                //listeditor.SelectionChanged +=listeditor_SelectionChanged;
//                //IList collection = listeditor.GetSelectedObjects();

//                // listeditor.Grid.StartRowEditing += Grid_StartRowEditing;

//                //int coun = listeditor.Grid.Selection.Count;

//                //geta(collection);


//                //}
//            }




//            /*
//            void processCurrentObjectController_CustomProcessSelectedItem(object sender, CustomProcessListViewSelectedItemEventArgs e)
//            {
//                if (View.SelectedObjects.Count > 1)
//                {
//                    e.Handled = false;
//                }
//                e.Handled = true;
//                foreach (object  item in View.SelectedObjects)
//                {
//                    action(item , View);
//                }
            
//            }
//        */

//        }

            

       
        
       


# endregion
        protected override void OnActivated()
        {
            base.OnActivated();
            // Perform various tasks depending on the target View.
           // if (((DetailView)View).ViewEditMode == ViewEditMode.Edit)
           // {
             //   ((DetailView)View).ViewEditMode = ViewEditMode.View;
           // }
            WebWindow.CurrentRequestWindow.PagePreRender += CurrentRequestWindow_PagePreRender;
        }

        void CurrentRequestWindow_PagePreRender(object sender, EventArgs e)
        {
            if (View is ListView && Application.MainWindow.View is ListView)
            {
                // this code was added to prevent new detail design estimate as we add estimate field at entity system design 
                //if (View.ObjectTypeInfo.Type == typeof(DesignDetailEstimate))
                //{
                //    View.AllowNew["0"] = false;
                //}
              
                if (View.ObjectTypeInfo.Type == typeof(QAUseCase) || View.ObjectTypeInfo.Type == typeof(QAMainFeature) || View.ObjectTypeInfo.Type == typeof(TestCase)
                          || View.ObjectTypeInfo.Type == typeof(TestRun) || View.ObjectTypeInfo.Type == typeof(DesignDetailEstimate) || View.ObjectTypeInfo.Type == typeof(QAUseCasePoints)
                    || View.ObjectTypeInfo.Type == typeof(QAProjectEntity) || View.ObjectTypeInfo.Type == typeof(Requirement) || View.ObjectTypeInfo.Type == typeof(EntitySystemDesign))
                {
                    ASPxGridListEditor gridListEditor = ((ListView)View).Editor as ASPxGridListEditor;
                    if (gridListEditor.Grid != null)
                    {
                        //ATA column devexpress.web.gridviewcolumn 
                        foreach (GridViewColumn column in gridListEditor.Grid.Columns)
                        {

                            if (column is GridViewDataActionColumn && ((GridViewDataActionColumn)column).Action.Id == "Edit")
                            {
                                column.Visible = false;

                            }
                        }
                    }
                }
                else
                {
                    ASPxGridListEditor gridListEditor = ((ListView)View).Editor as ASPxGridListEditor;
                    if (gridListEditor.Grid != null)
                    {
                        foreach (GridViewColumn column in gridListEditor.Grid.Columns)
                        {

                            if (column is GridViewDataActionColumn && ((GridViewDataActionColumn)column).Action.Id == "Edit")
                            {
                                column.Visible = true;

                            }
                        }
                    }
                }
            }
           
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

            if (WebWindow.CurrentRequestWindow!=null) //the absence of this condition was causing the failure of logging out
            WebWindow.CurrentRequestWindow.PagePreRender -= CurrentRequestWindow_PagePreRender;
        }

        private void Baseline_Activated(object sender, EventArgs e)
        {
            //ATA,1,enhance [start]
            //allow new and modifiy the tab to be other tasks estimation 
            //if ( View.ObjectTypeInfo.Type == typeof(DesignDetailEstimate))
            //{
            //    View.AllowNew["0"] = false;
            //}
            //else
            //{
            //    View.AllowNew["0"] = true;
            //}
            //ATA,1,enhance [end]
            ListViewController controller = Frame.GetController<ListViewController>();
            if (View.ObjectTypeInfo.Type == typeof(QAUseCase) || View.ObjectTypeInfo.Type == typeof(QAMainFeature) || View.ObjectTypeInfo.Type == typeof(TestCase)
                || View.ObjectTypeInfo.Type == typeof(TestRun) || View.ObjectTypeInfo.Type == typeof(DesignDetailEstimate) || View.ObjectTypeInfo.Type == typeof(QAUseCasePoints)
                || View.ObjectTypeInfo.Type == typeof(QAProjectEntity) || View.ObjectTypeInfo.Type == typeof(Requirement) || View.ObjectTypeInfo.Type == typeof(EntitySystemDesign) || View.ObjectTypeInfo.Type == typeof(ProjectScope) || View.ObjectTypeInfo.Type == typeof(QATestPlan))
            {
               
                userName = ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters).UserName;
                CriteriaOperator criteriaUser = CriteriaOperator.Parse("UserName = '" + userName + "'");
                user = ObjectSpace.FindObject<AriaSecuritySystemUser>(criteriaUser);
                //ATA change condition to be checked with role not user [start]
                // if (user != null && userName.ToUpper().Trim() != "ADMIN" && user.Account.Id == "Aria")
                if (user != null && user.Roles.Where(x => x.Name == "IterationAdmin").Count() == 0 && user.Account.Id == "Aria")
                {


                    if (View is DetailView)
                    {
                        // ITypeInfo req = Application.TypesInfo.FindTypeInfo(typeof(Requirement));
                        // IObjectSpace os = Application.CreateObjectSpace();
                        var currentobject = View.CurrentObject;
                        Permissions(currentobject, View, ObjectSpace);
                    }
                    else if (View is ListView && Application.MainWindow.View is ListView)
                    {

                        
                        //if (View.Model.AllowEdit == false)
                        //{
                        //    ((ListView)View).AllowDelete["1"] = false;
                        //    controller.EditAction.Active.SetItemValue("EditableListView", false);
                        //}
                        //if (View.ObjectTypeInfo.Type == typeof(QAUseCase) || View.ObjectTypeInfo.Type == typeof(QAMainFeature) || View.ObjectTypeInfo.Type == typeof(TestCase)
                        //         || View.ObjectTypeInfo.Type == typeof(TestRun) || View.ObjectTypeInfo.Type == typeof(DesignDetailEstimate) || View.ObjectTypeInfo.Type == typeof(QAUseCasePoints)
                        //         || View.ObjectTypeInfo.Type == typeof(QAProjectEntity) || View.ObjectTypeInfo.Type == typeof(Requirement) || View.ObjectTypeInfo.Type == typeof(EntitySystemDesign) || View.ObjectTypeInfo.Type == typeof(ProjectScope) || View.ObjectTypeInfo.Type == typeof(QATestPlan))
                        //{
                            if (View.Model.AllowEdit == false)
                            {
                                ((ListView)View).AllowDelete["1"] = false;
                                controller.EditAction.Active.SetItemValue("EditableListView", false);
                            }
                        //}
                        //else
                        //{
                        //    ((ListView)View).AllowDelete["1"] = true;
                        //    controller.EditAction.Active.SetItemValue("EditableListView", true);
                        //}

                    }

                }
                else
                {
                    if (View is DetailView)
                    {
                        View.AllowEdit["reason"] = true;
                        View.AllowDelete["reason"] = true;
                        View.Refresh();
                    }
                    else
                    {
                        ((ListView)View).AllowDelete["1"] = true;
                        controller.EditAction.Active.SetItemValue("EditableListView", true);
                    }
                }

            }else
            {
                if (View is DetailView)
                {
                    View.AllowEdit["reason"] = true;
                    View.AllowDelete["reason"] = true;
                    View.Refresh();
                }
                else
                {
                    ((ListView)View).AllowDelete["1"] = true;
                    controller.EditAction.Active.SetItemValue("EditableListView", true);
                }
            }
           
           
        }
   
        public static void Check_date(DateTime date,DevExpress.ExpressApp.View currentView)
        {
            if ( DateTime.Today > date)
            {
              ((DetailView)currentView).AllowEdit["reason"] = false;
              ((DetailView)currentView).AllowDelete["reason"] = false;
              //if (((DetailView)currentView).ViewEditMode == ViewEditMode.Edit)
              //{
              //    ((DetailView)currentView).ViewEditMode = ViewEditMode.View;
              //}
              currentView.Refresh();
          }
            else
            {
                currentView.AllowEdit["reason"] = true;
                currentView.AllowDelete["reason"] = true;
                currentView.Refresh();

            }
        }
        public static void Permissions(object new_object,DevExpress.ExpressApp.View CurrentView, IObjectSpace objectspace)
        {
            //ATA 8/5/2016
            CurrentView.AllowEdit["reason"] = true;
            CurrentView.AllowDelete["reason"] = true;
            CurrentView.Refresh();
            //for (int i = 0; i < objectspace.ModifiedObjects.Count; )
            //{
            //    objectspace.RemoveFromModifiedObjects(objectspace.ModifiedObjects[i]);
            //}
            //ATA 
            if (new_object is QAUseCase)
            {
                if (((QAUseCase)new_object).Oid != Guid.Empty)
                {
                    if (((QAUseCase)new_object).ProjectTemplate != null)
                    {
                        if (((QAUseCase)new_object).ProjectTemplate.Type.ToString() == "Key")
                        {
                            if (/*((QAUseCase)new_object).Enddate == DateTime.MinValue || */((QAUseCase)new_object).Enddate < CalcEndate.CalcEnddate(((QAUseCase)new_object).ProjectTemplate.StartDate, 2))
                           {
                                ((QAUseCase)new_object).Enddate = CalcEndate.CalcEnddate(((QAUseCase)new_object).ProjectTemplate.StartDate, 2);
                                ((QAUseCase)new_object).Save();
                                ((QAUseCase)new_object).Session.CommitTransaction();
                            }
                        }
                        else
                        {
                            if (((QAUseCase)new_object).Enddate == DateTime.MinValue || ((QAUseCase)new_object).Enddate < ((QAUseCase)new_object).ProjectTemplate.EndDate)
                            {
                                ((QAUseCase)new_object).Enddate = ((QAUseCase)new_object).ProjectTemplate.EndDate;
                                ((QAUseCase)new_object).Save();
                                ((QAUseCase)new_object).Session.CommitTransaction();
                            }
                        }
                        try
                        {
                            objectspace.CommitChanges();
                        }
                        catch (Exception ex)
                        {

                        }
                        Check_date(((QAUseCase)new_object).Enddate, CurrentView);
                    }
                }
            }
            else if (new_object is QAMainFeature)
            {
                if (((QAMainFeature)new_object).Oid != Guid.Empty)
                {
                    if (((QAMainFeature)new_object).ProjectEntity != null)
                    {
                        if (((QAMainFeature)new_object).ProjectEntity.ProjectTemplate != null)
                        {
                            //if (((QAMainFeature)new_object).ProjectEntity.ProjectTemplate.Type.ToString() == "Key")
                            //{
                              // if (((QAMainFeature)new_object).Enddate == DateTime.MinValue || ((QAMainFeature)new_object).Enddate < CalcEndate.CalcEnddate(((QAMainFeature)new_object).ProjectEntity.ProjectTemplate.StartDate, 2))
                            if ((((QAMainFeature)new_object).Enddate == DateTime.MinValue || ((QAMainFeature)new_object).Enddate < ((QAMainFeature)new_object).ProjectEntity.Enddate) && ((QAMainFeature)new_object).ProjectEntity.Enddate != DateTime.MinValue)   
                              {
                                   // ((QAMainFeature)new_object).Enddate = CalcEndate.CalcEnddate(((QAMainFeature)new_object).ProjectEntity.ProjectTemplate.StartDate, 2);
                                   ((QAMainFeature)new_object).Enddate = ((QAMainFeature)new_object).ProjectEntity.Enddate;
                                   ((QAMainFeature)new_object).Save();
                                    ((QAMainFeature)new_object).Session.CommitTransaction();
                                }

                           // }
                          //  else
                          //  {
                              //  if (((QAMainFeature)new_object).Enddate == DateTime.MinValue || ((QAMainFeature)new_object).Enddate < ((QAMainFeature)new_object).ProjectEntity.ProjectTemplate.EndDate)
                              //  {
                                //    ((QAMainFeature)new_object).Enddate = ((QAMainFeature)new_object).ProjectEntity.ProjectTemplate.EndDate;
                                 //   ((QAMainFeature)new_object).Save();
                                //    ((QAMainFeature)new_object).Session.CommitTransaction();
                               // }
                           // }
                            objectspace.CommitChanges();
                            Check_date(((QAMainFeature)new_object).Enddate, CurrentView);
                        }
                    }
                }
            }
            else if (new_object is DesignDetailEstimate)
            {
                if (((DesignDetailEstimate)new_object).Oid != Guid.Empty)
                {
                    if (((DesignDetailEstimate)new_object).QAProjectEntity != null)
                    {
                        if (((DesignDetailEstimate)new_object).QAProjectEntity.ProjectTemplate != null)
                        {
                            if (((DesignDetailEstimate)new_object).QAProjectEntity.ProjectTemplate.Type.ToString() == "Key")
                            {
                                if (((DesignDetailEstimate)new_object).Enddate == DateTime.MinValue || ((DesignDetailEstimate)new_object).Enddate < CalcEndate.CalcEnddate(((DesignDetailEstimate)new_object).QAProjectEntity.ProjectTemplate.StartDate, 3))
                                {
                                    ((DesignDetailEstimate)new_object).Enddate = CalcEndate.CalcEnddate(((DesignDetailEstimate)new_object).QAProjectEntity.ProjectTemplate.StartDate, 3);
                                    ((DesignDetailEstimate)new_object).Save();
                                    ((DesignDetailEstimate)new_object).Session.CommitTransaction();
                                }
                            }
                            else
                            {
                                if (((DesignDetailEstimate)new_object).Enddate == DateTime.MinValue || ((DesignDetailEstimate)new_object).Enddate < ((DesignDetailEstimate)new_object).QAProjectEntity.ProjectTemplate.EndDate)
                                {
                                    ((DesignDetailEstimate)new_object).Enddate = ((DesignDetailEstimate)new_object).QAProjectEntity.ProjectTemplate.EndDate;
                                    ((DesignDetailEstimate)new_object).Save();
                                    ((DesignDetailEstimate)new_object).Session.CommitTransaction();
                                }
                            }
                            objectspace.CommitChanges();
                            Check_date(((DesignDetailEstimate)new_object).Enddate, CurrentView);
                        }

                    }
                }
            }
            else if (new_object is TestCase)
            {
                if (((TestCase)new_object).Oid != Guid.Empty)
                {
                    if (((TestCase)new_object).ProjectTemplate != null)
                    {
                        if (((TestCase)new_object).ProjectTemplate.Type.ToString() == "Key")
                        {
                            if (!((TestCase)new_object).IsOriginal)
                            {
                                if (((TestCase)new_object).Enddate == DateTime.MinValue || ((TestCase)new_object).Enddate < CalcEndate.CalcEnddate(((TestCase)new_object).ProjectTemplate.StartDate, 4))
                                {
                                    ((TestCase)new_object).Enddate = CalcEndate.CalcEnddate(((TestCase)new_object).ProjectTemplate.StartDate, 4);
                                    ((TestCase)new_object).Save();
                                    ((TestCase)new_object).Session.CommitTransaction();
                                }
                            }
                            else
                            {
                                if (((TestCase)new_object).Enddate == DateTime.MinValue || ((TestCase)new_object).Enddate < CalcEndate.CalcEnddate(((TestCase)new_object).ProjectTemplate.StartDate, 3))
                                {
                                    ((TestCase)new_object).Enddate = CalcEndate.CalcEnddate(((TestCase)new_object).ProjectTemplate.StartDate, 3);
                                    ((TestCase)new_object).Save();
                                    ((TestCase)new_object).Session.CommitTransaction();
                                }
                            }

                        }
                        else
                        {
                            if (((TestCase)new_object).Enddate == DateTime.MinValue || ((TestCase)new_object).Enddate < ((TestCase)new_object).ProjectTemplate.EndDate)
                            {
                                ((TestCase)new_object).Enddate = ((TestCase)new_object).ProjectTemplate.EndDate;
                                ((TestCase)new_object).Save();
                                ((TestCase)new_object).Session.CommitTransaction();
                            }
                        }
                        try
                        {
                            objectspace.CommitChanges();
                        }
                        catch (Exception ex)
                        {

                        }
                        Check_date(((TestCase)new_object).Enddate, CurrentView);
                    }
                }
            }
            else if (new_object is TestRun)
            {
                if (((TestRun)new_object).Oid != Guid.Empty)
                {
                    if (((TestRun)new_object).ProjectTemplate != null)
                    {
                        if (((TestRun)new_object).ProjectTemplate.Type.ToString() == "Key")
                        {
                            if (((TestRun)new_object).Enddate == DateTime.MinValue || ((TestRun)new_object).Enddate < CalcEndate.CalcEnddate(((TestRun)new_object).ProjectTemplate.StartDate, 4))
                            {
                                ((TestRun)new_object).Enddate = CalcEndate.CalcEnddate(((TestRun)new_object).ProjectTemplate.StartDate, 4);
                                ((TestRun)new_object).Save();
                                ((TestRun)new_object).Session.CommitTransaction();
                            }
                        }
                        else
                        {
                            if (((TestRun)new_object).Enddate == DateTime.MinValue || ((TestRun)new_object).Enddate < ((TestRun)new_object).ProjectTemplate.EndDate)
                            {
                                ((TestRun)new_object).Enddate = ((TestRun)new_object).ProjectTemplate.EndDate;
                                ((TestRun)new_object).Save();
                                ((TestRun)new_object).Session.CommitTransaction();
                            }
                        }
                        objectspace.CommitChanges();
                        Check_date(((TestRun)new_object).Enddate, CurrentView);
                    }
                }
            }
            else if (new_object is QAUseCasePoints)
            {
                if (((QAUseCasePoints)new_object).Oid != Guid.Empty)
                {
                    if (((QAUseCasePoints)new_object).Project != null)
                    {
                        if (((QAUseCasePoints)new_object).Project.Type.ToString() == "Key")
                        {
                            if (((QAUseCasePoints)new_object).Enddate == DateTime.MinValue || ((QAUseCasePoints)new_object).Enddate < CalcEndate.CalcEnddate(((QAUseCasePoints)new_object).Project.StartDate, 2))
                            {
                                ((QAUseCasePoints)new_object).Enddate = CalcEndate.CalcEnddate(((QAUseCasePoints)new_object).Project.StartDate, 2);
                             
                            }
                        }
                        else
                        {
                            if (((QAUseCasePoints)new_object).Enddate == DateTime.MinValue || ((QAUseCasePoints)new_object).Enddate < ((QAUseCasePoints)new_object).Project.EndDate)
                            {
                                ((QAUseCasePoints)new_object).Enddate = ((QAUseCasePoints)new_object).Project.EndDate;
                                //((QAUseCasePoints)new_object).Save();
                                //((QAUseCasePoints)new_object).Session.CommitTransaction();
                            }
                        }
                        ((QAUseCasePoints)new_object).Save();
                        ((QAUseCasePoints)new_object).Session.CommitTransaction();
                        objectspace.CommitChanges();
                        Check_date(((QAUseCasePoints)new_object).Enddate, CurrentView);
                    }
                }
            }
            else if (new_object is QAProjectEntity)
            {
                if (((QAProjectEntity)new_object).Oid != Guid.Empty)
                {
                    if (((QAProjectEntity)new_object).ProjectTemplate != null)
                    {
                       // QAProjectEntity obj = objectspace.FindObject<QAProjectEntity>(CriteriaOperator.Parse("[Oid] = '" + ((QAProjectEntity)new_object).Oid + "'"));
                        if (((QAProjectEntity)new_object).ProjectTemplate.Type.ToString() == "Key")
                        {
                            if (((QAProjectEntity)new_object).Enddate == DateTime.MinValue || ((QAProjectEntity)new_object).Enddate < CalcEndate.CalcEnddate(((QAProjectEntity)new_object).ProjectTemplate.StartDate, 2))
                            {
                               // QAProjectEntity obj = objectspace.FindObject<QAProjectEntity>(CriteriaOperator.Parse("[Oid] = '" + ((QAProjectEntity)new_object).Oid + "'"));
                                ((QAProjectEntity)new_object).Enddate = CalcEndate.CalcEnddate(((QAProjectEntity)new_object).ProjectTemplate.StartDate, 2);
                                ((QAProjectEntity)new_object).Save();
                                ((QAProjectEntity)new_object).Session.CommitTransaction();
                               //obj.Enddate = CalcEndate.CalcEnddate(((QAProjectEntity)new_object).ProjectTemplate.StartDate, 2);
                               // obj.Save();
                                
                            }
                        }
                        else
                        {
                            if (((QAProjectEntity)new_object).Enddate == DateTime.MinValue || ((QAProjectEntity)new_object).Enddate < ((QAProjectEntity)new_object).ProjectTemplate.EndDate)
                            {
                                ((QAProjectEntity)new_object).Enddate = ((QAProjectEntity)new_object).ProjectTemplate.EndDate;
                                ((QAProjectEntity)new_object).Save();
                                ((QAProjectEntity)new_object).Session.CommitTransaction();
                                //obj.Enddate = ((QAProjectEntity)new_object).ProjectTemplate.EndDate;
                                //obj.Save();
                            }
                        }
                        //if (objectspace.ModifiedObjects.Contains((QAProjectEntity)new_object))
                       // {
                        //objectspace.RemoveFromModifiedObjects(objectspace.ModifiedObjects.OfType<Account>().Where(s=>s.Oid != null).FirstOrDefault());
                       // objectspace.RemoveFromModifiedObjects(objectspace.ModifiedObjects.OfType<HREmployee>().Where(s => s.Oid != null).FirstOrDefault());
                      //  if (objectspace.ModifiedObjects.Count > 0)
                        //{
                        try
                        {
                            objectspace.CommitChanges();
                        }
                        catch (Exception ex)
                        {
                            
                        }
                       // } if (((DetailView)CurrentView).ViewEditMode == ViewEditMode.Edit)
                       //     ((DetailView)CurrentView).ViewEditMode = ViewEditMode.View;
                       //// }
                        Check_date(((QAProjectEntity)new_object).Enddate, CurrentView);
                    }
                }
            }
            else if (new_object is Requirement)
            {
                if (((Requirement)new_object).Oid != Guid.Empty)
                {
                    //ATA , 19/5/2016 make check box marked if the requirment not have usecase or test case or project entity linked  [start]
                  if (((Requirement)new_object).Usecases.Count > 0)
                    {
                        ((Requirement)new_object).USECASE = false;
                        foreach (QAUseCase requirementusecase in ((Requirement)new_object).Usecases)
                        {
                            if (requirementusecase.ProjectEntities.Count > 0)
                            {
                                ((Requirement)new_object).PROJECTENTITY = false;
                                break;
                            }
                        }
                    }
                    else
                    {
                        ((Requirement)new_object).USECASE = true;
                        ((Requirement)new_object).PROJECTENTITY = true;
                    }
                    if (((Requirement)new_object).TestCases.Count > 0)
                    {
                        ((Requirement)new_object).TESTCASE = false;
                    }
                    else
                    {
                        ((Requirement)new_object).TESTCASE = true;

                    }
                    //ATA iteration 5 ,19/5/2016 [End]
                    if (((Requirement)new_object).ProjectTemplate != null)
                    {
                        if (((Requirement)new_object).ProjectTemplate.Type.ToString() == "Key")
                        {
                            if (((Requirement)new_object).Enddate == DateTime.MinValue || ((Requirement)new_object).Enddate < CalcEndate.CalcEnddate(((Requirement)new_object).ProjectTemplate.StartDate, 2))
                            {
                                //18/5/2016 to make sure that end date calculated correctly 
                                //   if (((Requirement)new_object).Enddate == CalcEndate.CalcEnddate(((Requirement)new_object).ProjectTemplate.StartDate, 2))
                                //  {
                                //    Check_date(((Requirement)new_object).Enddate, CurrentView);
                                //}
                                ((Requirement)new_object).Enddate = CalcEndate.CalcEnddate(((Requirement)new_object).ProjectTemplate.StartDate, 2);
                              
                            }
                        }
                        else
                        {
                            if (((Requirement)new_object).Enddate == DateTime.MinValue || ((Requirement)new_object).Enddate < ((Requirement)new_object).ProjectTemplate.EndDate)
                            {
                                ((Requirement)new_object).Enddate = ((Requirement)new_object).ProjectTemplate.EndDate;
                              //  ((Requirement)new_object).Save();
                                //((Requirement)new_object).Session.CommitTransaction();
                            }
                        }
                        ((Requirement)new_object).Save();
                        ((Requirement)new_object).Session.CommitTransaction();
                        try
                        {
                            objectspace.CommitChanges();
                        }
                        catch (Exception ex)
                        {

                        }
                        Check_date(((Requirement)new_object).Enddate, CurrentView);
                    }
                    else
                    {
                        ((Requirement)new_object).Save();
                        ((Requirement)new_object).Session.CommitTransaction();
                        objectspace.CommitChanges();
                    }              
                }
                else
                {
                    if (((Requirement)new_object).ProjectTemplate != null)
                    {
                        ((Requirement)new_object).USECASE = true;
                        ((Requirement)new_object).PROJECTENTITY = true;
                        ((Requirement)new_object).TESTCASE = true;
                        if (CalcEndate.CalcEnddate(((Requirement)new_object).ProjectTemplate.StartDate, 2) < DateTime.Today)
                        {
                            ((Requirement)new_object).RequirementType = objectspace.FindObject<RequirementType>(CriteriaOperator.Parse("[Name] = 'Change Request  CR'"));
                            //    //// ((Requirement)new_object).Session.CreateObjectTypeRecords();
                            //   // Attribute customeatt = new CustomAttribute("AllowEdit", "False");
                            ////   //info.FindMember("RequirementType").AddAttribute(customeatt);
                        }
                    }
                }
            }
            else if (new_object is EntitySystemDesign)
            {
                if (((EntitySystemDesign)new_object).Oid != Guid.Empty)
                {
                    if (((EntitySystemDesign)new_object).QAProjectEntity != null)
                    {
                        if (((EntitySystemDesign)new_object).QAProjectEntity.ProjectTemplate != null)
                        {
                            if (((EntitySystemDesign)new_object).QAProjectEntity.ProjectTemplate.Type.ToString() == "Key")
                            {
                                if (((EntitySystemDesign)new_object).Enddate == DateTime.MinValue || ((EntitySystemDesign)new_object).Enddate < CalcEndate.CalcEnddate(((EntitySystemDesign)new_object).QAProjectEntity.ProjectTemplate.StartDate, 3))
                                {
                                    ((EntitySystemDesign)new_object).Enddate = CalcEndate.CalcEnddate(((EntitySystemDesign)new_object).QAProjectEntity.ProjectTemplate.StartDate, 3);
                                    ((EntitySystemDesign)new_object).Save();
                                    ((EntitySystemDesign)new_object).Session.CommitTransaction();
                                }
                            }
                            else
                            {
                                if (((EntitySystemDesign)new_object).Enddate == DateTime.MinValue || ((EntitySystemDesign)new_object).Enddate < ((EntitySystemDesign)new_object).QAProjectEntity.ProjectTemplate.EndDate)
                                {
                                    ((EntitySystemDesign)new_object).Enddate = ((EntitySystemDesign)new_object).QAProjectEntity.ProjectTemplate.EndDate;
                                    ((EntitySystemDesign)new_object).Save();
                                    ((EntitySystemDesign)new_object).Session.CommitTransaction();
                                }
                            }
                            objectspace.CommitChanges();
                            Check_date(((EntitySystemDesign)new_object).Enddate, CurrentView);
                        }
                    }
                }
            }
            else if (new_object is ProjectScope)
            {
                if (((ProjectScope)new_object).Oid != Guid.Empty)
                {
                    if (((ProjectScope)new_object).Project != null)
                    {
                        if (((ProjectScope)new_object).Project.Type.ToString() == "Key")
                            {
                                if (((ProjectScope)new_object).EndDate == DateTime.MinValue || ((ProjectScope)new_object).EndDate < CalcEndate.CalcEnddate(((ProjectScope)new_object).Project.StartDate, 1))
                                {
                                    ((ProjectScope)new_object).EndDate = CalcEndate.CalcEnddate(((ProjectScope)new_object).Project.StartDate, 1);
                                    ((ProjectScope)new_object).Save();
                                    ((ProjectScope)new_object).Session.CommitTransaction();
                                }
                            }
                            else
                            {
                                if (((ProjectScope)new_object).EndDate == DateTime.MinValue || ((ProjectScope)new_object).EndDate < ((ProjectScope)new_object).Project .EndDate)
                                {
                                    ((ProjectScope)new_object).EndDate = ((ProjectScope)new_object).Project.EndDate;
                                    ((ProjectScope)new_object).Save();
                                    ((ProjectScope)new_object).Session.CommitTransaction();
                                }
                            }
                            objectspace.CommitChanges();
                            Check_date(((ProjectScope)new_object).EndDate, CurrentView);

                    }
                }
            }
            else if (new_object is QATestPlan)
            {
                if (((QATestPlan)new_object).Oid != Guid.Empty)
                {
                    if (((QATestPlan)new_object).ProjectTemplate != null)
                    {
                        if (((QATestPlan)new_object).ProjectTemplate.Type.ToString() == "Key")
                        {
                            if (((QATestPlan)new_object).Enddate == DateTime.MinValue || ((QATestPlan)new_object).Enddate < CalcEndate.CalcEnddate(((QATestPlan)new_object).ProjectTemplate.StartDate, 1))
                            {
                                ((QATestPlan)new_object).Enddate = CalcEndate.CalcEnddate(((QATestPlan)new_object).ProjectTemplate.StartDate, 1);
                                ((QATestPlan)new_object).Save();
                                ((QATestPlan)new_object).Session.CommitTransaction();
                            }
                        }
                        else
                        {
                            if (((QATestPlan)new_object).Enddate == DateTime.MinValue || ((QATestPlan)new_object).Enddate < ((QATestPlan)new_object).ProjectTemplate.EndDate)
                            {
                                ((QATestPlan)new_object).Enddate = ((QATestPlan)new_object).ProjectTemplate.EndDate;
                                ((QATestPlan)new_object).Save();
                                ((QATestPlan)new_object).Session.CommitTransaction();
                            }
                        }
                        try
                        {
                            objectspace.CommitChanges();

                        }
                        catch (Exception ex)
                        {

                        }
                        Check_date(((QATestPlan)new_object).Enddate, CurrentView);

                    }
                }
            }
            else
            {
                CurrentView.AllowEdit["reason"] = true;
                CurrentView.AllowDelete["reason"] = true;
                CurrentView.Refresh();

            }

        }

        private void Baseline_AfterConstruction(object sender, EventArgs e)
        {
        }

        private void Baseline_ViewControlsCreated(object sender, EventArgs e)
        {
            //if (View is ListView)
            //{
            //     ASPxGridListEditor listeditor = ((ListView)View).Editor as ASPxGridListEditor;
            //}   
        }

        private void ImportRequirement_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            IObjectSpace os = Application.CreateObjectSpace();
            DetailView dv = Application.CreateDetailView(os, os.CreateObject<ImportRequirement>());
            dv.ViewEditMode = ViewEditMode.Edit;
            e.ShowViewParameters.CreatedView = dv;
           
            
        }
    }
    //ATA, end 22/3/2016
}
