using System;
using System.Collections.Generic;
using System.Linq;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.SystemModule;
using DevExpress.ExpressApp.Actions;

namespace GenericMessageBox
{
    public class GenericMessageBox
    {

        public delegate void MessageBoxEventHandler(object sender, ShowViewParameters e);

        private event MessageBoxEventHandler localAccept;
        private event EventHandler localCancel;

        public GenericMessageBox(ShowViewParameters svp, XafApplication app, string Message, MessageBoxEventHandler Accept, EventHandler Cancel)
        {
            CreateDetailView(svp, app, Message);
            AttachDialogController(svp, app, Accept, Cancel);
        }

        public GenericMessageBox(ShowViewParameters svp, XafApplication app, string Message, MessageBoxEventHandler Accept)
        {
            CreateDetailView(svp, app, Message);
            AttachDialogController(svp, app, Accept);
        }

        public GenericMessageBox(ShowViewParameters svp, XafApplication app, string Message)
        {
            CreateDetailView(svp, app, Message);
            AttachDialogController(svp, app);
        }

        private void AttachDialogController(ShowViewParameters svp, XafApplication app, MessageBoxEventHandler Accept, EventHandler Cancel)
        {
            localAccept = Accept;
            localCancel = Cancel;
            DialogController dc = app.CreateController<DialogController>();
            dc.AcceptAction.Execute += AcceptAction_Execute;
            dc.Cancelling += dc_Cancelling;
            svp.Controllers.Add(dc);
        }

        private void AttachDialogController(ShowViewParameters svp, XafApplication app, MessageBoxEventHandler Accept)
        {
            localAccept = Accept;
            DialogController dc = app.CreateController<DialogController>();
            dc.AcceptAction.Execute += AcceptAction_Execute;
            dc.CancelAction.Enabled.SetItemValue("Cancel Disabled", false);
            dc.CancelAction.Active.SetItemValue("Cancel Disabled", false);
            svp.Controllers.Add(dc);
        }

        private void AttachDialogController(ShowViewParameters svp, XafApplication app)
        {
            DialogController dc = app.CreateController<DialogController>();
            dc.AcceptAction.Execute += AcceptAction_Execute;
            dc.CancelAction.Enabled.SetItemValue("Cancel Disabled", false);
            dc.CancelAction.Active.SetItemValue("Cancel Disabled", false);
            svp.Controllers.Add(dc);
        }

        private static void CreateDetailView(ShowViewParameters svp, XafApplication app, string Message)
        {   // Look
            IObjectSpace objectSpace = app.CreateObjectSpace();
            //svp.CreatedView = app.CreateDetailView(ObjectSpaceInMemory.CreateNew(), new PingSearchEngine(Message)); ;



            svp.CreatedView = app.CreateDetailView(objectSpace, new PingSearchEngine(Message)); ;
            svp.TargetWindow = TargetWindow.NewModalWindow;
            svp.Context = TemplateContext.PopupWindow;
            svp.CreateAllControllers = true;
        }

        void AcceptAction_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            if (localAccept != null)
                localAccept(this, e.ShowViewParameters);
        }

        void dc_Cancelling(object sender, EventArgs e)
        {
            if (localCancel != null)
                localCancel(this, e);
        }
    }




    public class GenericMessageBox2
    {

        public delegate void MessageBoxEventHandler(object sender, ShowViewParameters e);

        private event MessageBoxEventHandler localAccept;
        private event EventHandler localCancel;

        public GenericMessageBox2(ShowViewParameters svp, XafApplication app, string Message, MessageBoxEventHandler Accept, EventHandler Cancel)
        {
            CreateDetailView(svp, app, Message);
            AttachDialogController(svp, app, Accept, Cancel);
        }

        public GenericMessageBox2(ShowViewParameters svp, XafApplication app, string Message, MessageBoxEventHandler Accept)
        {
            CreateDetailView(svp, app, Message);
            AttachDialogController(svp, app, Accept);
        }

        public GenericMessageBox2(ShowViewParameters svp, XafApplication app, string Message)
        {
            CreateDetailView(svp, app, Message);
            AttachDialogController(svp, app);
        }

        private void AttachDialogController(ShowViewParameters svp, XafApplication app, MessageBoxEventHandler Accept, EventHandler Cancel)
        {
            localAccept = Accept;
            localCancel = Cancel;
            DialogController dc = app.CreateController<DialogController>();
            dc.AcceptAction.Execute += AcceptAction_Execute;
            dc.Cancelling += dc_Cancelling;
            svp.Controllers.Add(dc);
        }

        private void AttachDialogController(ShowViewParameters svp, XafApplication app, MessageBoxEventHandler Accept)
        {
            localAccept = Accept;
            DialogController dc = app.CreateController<DialogController>();
            dc.AcceptAction.Execute += AcceptAction_Execute;
            dc.CancelAction.Enabled.SetItemValue("Cancel Disabled", false);
            dc.CancelAction.Active.SetItemValue("Cancel Disabled", false);
            svp.Controllers.Add(dc);
        }

        private void AttachDialogController(ShowViewParameters svp, XafApplication app)
        {
            DialogController dc = app.CreateController<DialogController>();
            dc.AcceptAction.Execute += AcceptAction_Execute;
            dc.CancelAction.Enabled.SetItemValue("Cancel Disabled", false);
            dc.CancelAction.Active.SetItemValue("Cancel Disabled", false);
            svp.Controllers.Add(dc);
        }

        private static void CreateDetailView(ShowViewParameters svp, XafApplication app, string Message)
        {   // look
            IObjectSpace objectSpace = app.CreateObjectSpace();
            //svp.CreatedView = app.CreateDetailView(ObjectSpaceInMemory.CreateNew(), new RefreshSiteMap(Message)); ;

            svp.CreatedView = app.CreateDetailView(objectSpace, new RefreshSiteMap(Message)); ;
            svp.TargetWindow = TargetWindow.NewModalWindow;
            svp.Context = TemplateContext.PopupWindow;
            svp.CreateAllControllers = true;
        }

        void AcceptAction_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            if (localAccept != null)
                localAccept(this, e.ShowViewParameters);
        }

        void dc_Cancelling(object sender, EventArgs e)
        {
            if (localCancel != null)
                localCancel(this, e);
        }
    }
}
