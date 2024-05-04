using System;
using System.Collections.Generic;
using System.Text;
using DevExpress.ExpressApp.SystemModule;
using DevExpress.ExpressApp.Actions;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Security;
using System.Collections.Specialized;
using AriaDevExpress.Module.BusinessObjects.SysFiles;
using AriaDevExpress.Module.BusinessObjects.Globalization;
using DevExpress.Data.Filtering;
using AriaDevExpress.Module.BusinessObjects.ClientManager;

namespace AriaDevExpress.Module
{
    public class WindowControlerMenuCustomization : ShowNavigationItemController
    {
        protected override void ShowNavigationItem(SingleChoiceActionExecuteEventArgs args)
        {
            //((DevExpress.ExpressApp.Actions.SimpleActionExecuteEventArgs)(args)).CurrentObject = 
            //((DevExpress.ExpressApp.Actions.SimpleActionExecuteEventArgs)(args)).Action.TypeOfView
            if (args.SelectedChoiceActionItem.Id == "Export")
            {
                IObjectSpace objectSpace = Application.CreateObjectSpace();
                ExportEmptyClass obj = new ExportEmptyClass();
                DetailView detailView = Application.CreateDetailView(objectSpace, obj, true);
                args.ShowViewParameters.CreatedView = detailView;
                args.ShowViewParameters.Context = TemplateContext.ApplicationWindow;
                args.ShowViewParameters.TargetWindow = TargetWindow.Current;
                //   args.CurrentObject = Application.CreateObjectSpace().CreateObject(typeof(ExportEmptyClass));
            }
            else if (args.SelectedChoiceActionItem.Id == "Globalization_DetailView")
            {
                IObjectSpace objectSpace = Application.CreateObjectSpace();
                Globalization obj = new Globalization();
                DetailView detailView = Application.CreateDetailView(objectSpace, obj, true);
                args.ShowViewParameters.CreatedView = detailView;
                args.ShowViewParameters.Context = TemplateContext.ApplicationWindow;
                args.ShowViewParameters.TargetWindow = TargetWindow.Current;
            }
            else if (args.SelectedChoiceActionItem.Id == "NewClient_DetailView")
            {
                IObjectSpace objectSpace = Application.CreateObjectSpace();
                NewClient1 obj = new NewClient1();
                DetailView detailView = Application.CreateDetailView(objectSpace, obj, true);
                args.ShowViewParameters.CreatedView = detailView;
                args.ShowViewParameters.Context = TemplateContext.ApplicationWindow;
                args.ShowViewParameters.TargetWindow = TargetWindow.Current;
            }
            else if (args.SelectedChoiceActionItem.Id == "Configration_DetailView")
            {
                IObjectSpace objectSpace = Application.CreateObjectSpace();
                Configration obj = new Configration();
                DetailView detailView = Application.CreateDetailView(objectSpace, obj, true);
                args.ShowViewParameters.CreatedView = detailView;
                args.ShowViewParameters.Context = TemplateContext.ApplicationWindow;
                args.ShowViewParameters.TargetWindow = TargetWindow.Current;
            }
            else
                base.ShowNavigationItem(args);

            CreateFormExampleDetailView(args, this.Application);
        }

        public static void CreateFormExampleDetailView(SingleChoiceActionExecuteEventArgs pAction, XafApplication pApplication)
        {
            //ObjectSpace objectSpace = pApplication.CreateObjectSpace();

            //FormExample obj = objectSpace.CreateObject<FormExample>();

            //DetailView detailView = pApplication.CreateDetailView(objectSpace, obj, true);

            //pAction.ShowViewParameters.CreatedView = detailView;
            //pAction.ShowViewParameters.Context = TemplateContext.ApplicationWindow;
            //pAction.ShowViewParameters.TargetWindow = TargetWindow.Current;
        }
    }
}
