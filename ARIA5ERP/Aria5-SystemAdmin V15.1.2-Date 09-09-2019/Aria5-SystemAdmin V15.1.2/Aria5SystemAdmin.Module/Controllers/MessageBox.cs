using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DevExpress.Xpo;
using DevExpress.ExpressApp.Web.SystemModule;
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

namespace Aria5SystemAdmin.Module.Controllers
{
    [NonPersistent]
    public class MessageBox
    {
        private string message;
        public string Message { get { return message; } }
        private MessageBox(string message)
        {
            this.message = message;
        }


        public static void Show(XafApplication app, ShowViewParameters svp, string message, Action okMethod)
        {
            IObjectSpace os = ObjectSpaceInMemory.CreateNew();
            MessageBox obj = new MessageBox(message);
            svp.CreatedView = app.CreateDetailView(os, obj);
            DialogController dc = app.CreateController<DialogController>();
            dc.Accepting += new EventHandler<DialogControllerAcceptingEventArgs>(delegate
            {
                if (okMethod != null) okMethod();
            });
            dc.CancelAction.Enabled.SetItemValue("Disable", false);

            svp.Controllers.Add(dc);
            svp.Context = TemplateContext.PopupWindow;
            svp.TargetWindow = TargetWindow.NewModalWindow;
            svp.NewWindowTarget = NewWindowTarget.Separate;
        }
        public static void Show(XafApplication app, string message, Action okMethod)
        {
            ShowViewParameters svp = new ShowViewParameters();
            Show(app, svp, message, okMethod);
            app.ShowViewStrategy.ShowView(svp, new ShowViewSource(null, null));
        }

    }
}
