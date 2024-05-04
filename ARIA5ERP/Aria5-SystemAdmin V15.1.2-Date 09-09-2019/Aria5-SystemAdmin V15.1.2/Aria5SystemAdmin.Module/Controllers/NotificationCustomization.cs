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
using DevExpress.Persistent.Base.General;
using DevExpress.ExpressApp.Notifications;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.ExpressApp.Web.Editors.ASPx;
using DevExpress.Web;

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out https://documentation.devexpress.com/eXpressAppFramework/clsDevExpressExpressAppWindowControllertopic.aspx.
    public partial class NotificationCustomization : WindowController
    {
        internal static bool isSchedulerNotificationInitialize;
        internal static bool isCustomNotificationInitialize;
        internal INotificationsProvider schedulerProvider;
        internal INotificationsProvider defaultProvider;
        internal NotificationsController notificationController;
        public NotificationCustomization()
        {
            InitializeComponent();
            // Target required Windows (via the TargetXXX properties) and create their Actions.
        }
        protected override void OnActivated()
        {
            base.OnActivated();
            // Perform various tasks depending on the target Window.
            

        }
        protected override void OnDeactivated()
        {
            // Unsubscribe from previously subscribed events and release other references and resources.
            base.OnDeactivated();
        }
        public class customnotificationcontroller : ObjectViewController<DetailView, NotificationsObject>
        {
            protected override void OnActivated()
            {
                base.OnActivated();

                Frame.GetController<NotificationsDialogViewController>().Dismiss.Execute += Dismiss_Execute;
                Frame.GetController<NotificationsDialogViewController>().Snooze.Execute += Snooze_Execute;
             
                
            }
            void Snooze_Execute(object sender, DevExpress.ExpressApp.Actions.SimpleActionExecuteEventArgs e)
            {
                foreach (object x in e.SelectedObjects)
                {
                    if (((Notification)x).NotificationSource.GetType() == typeof(TrackingTask))
                    {
                        TrackingTask test = ObjectSpace.FindObject<TrackingTask>(CriteriaOperator.Parse("Oid = '" + ((Notification)x).NotificationSource.UniqueId + "'"));
                        test.Session.LockingOption = DevExpress.Xpo.LockingOption.None;
                        test.AlarmTime = DateTime.Now.Add(base.ViewCurrentObject.Postpone.RemindIn.Value);
                        test.IsPostponed = true;
                        test.Save();
                        test.Session.CommitTransaction();
                        View.Refresh();
                    }
                    else if (((Notification)x).NotificationSource.GetType() == typeof(ProjectTemplate))
                    {
                        ProjectTemplate projecttonotify = ObjectSpace.FindObject<ProjectTemplate>(CriteriaOperator.Parse("Oid = '" + ((Notification)x).NotificationSource.UniqueId + "'"));
                        projecttonotify.Session.LockingOption = DevExpress.Xpo.LockingOption.None;
                        if (projecttonotify.AlarmTime <= CalcEndate.CalcEnddate(projecttonotify.StartDate, 2))
                        {
                            projecttonotify.AlarmTime = DateTime.Now.Add(base.ViewCurrentObject.Postpone.RemindIn.Value);
                            if (projecttonotify.AlarmTime > CalcEndate.CalcEnddate(projecttonotify.StartDate, 2))
                            {
                                throw new Exception("you can't snooze over due date ");
                            }
                        }
                        else if (projecttonotify.AlarmTime <= CalcEndate.CalcEnddate(projecttonotify.StartDate, 3))
                        {
                            projecttonotify.AlarmTime = DateTime.Now.Add(base.ViewCurrentObject.Postpone.RemindIn.Value);
                            if (projecttonotify.AlarmTime > CalcEndate.CalcEnddate(projecttonotify.StartDate, 3))
                            {
                                throw new Exception("you can't snooze over due date ");
                            }
                        }
                        
                        projecttonotify.IsPostponed = true;
                        projecttonotify.Save();
                        projecttonotify.Session.CommitTransaction();
                        View.Refresh();
                    }
                    else if (((Notification)x).NotificationSource.GetType() == typeof(QANonComplains))
                    {
                        QANonComplains test = ObjectSpace.FindObject<QANonComplains>(CriteriaOperator.Parse("Oid = '" + ((Notification)x).NotificationSource.UniqueId + "'"));
                        test.Session.LockingOption = DevExpress.Xpo.LockingOption.None;
                        test.AlarmTime = DateTime.Now.Add(base.ViewCurrentObject.Postpone.RemindIn.Value);
                        test.IsPostponed = true;
                        test.Save();
                        test.Session.CommitTransaction();
                        View.Refresh();
                    }
                    //else if (((Notification)x).NotificationSource.GetType() == typeof(QAProjectEntity))
                    //{
                    //    QAProjectEntity test = ObjectSpace.FindObject<QAProjectEntity>(CriteriaOperator.Parse("Oid = '" + ((Notification)x).NotificationSource.UniqueId + "'"));
                    //    test.Session.LockingOption = DevExpress.Xpo.LockingOption.None;
                    //    test.AlarmTime = DateTime.Now.Add(base.ViewCurrentObject.Postpone.RemindIn.Value);
                    //    test.IsPostponed = true;
                    //    test.Save();
                    //    test.Session.CommitTransaction();
                    //    View.Refresh();
                    //}
                    //((Notification)x).NotificationSource.AlarmTime = DateTime.Now + TimeSpan.MinValue;
                    // ((Notification)x).NotificationSource.IsPostponed = true;

                }
            }
            void Dismiss_Execute(object sender, DevExpress.ExpressApp.Actions.SimpleActionExecuteEventArgs e)
            {
                foreach (object x in e.SelectedObjects)
                {
                    object s = ((Notification)x).NotificationSource;
                    if (((Notification)x).NotificationSource.GetType() == typeof(TrackingTask))
                    {
                        TrackingTask test = ObjectSpace.FindObject<TrackingTask>(CriteriaOperator.Parse("Oid = '" + ((Notification)x).NotificationSource.UniqueId + "'"));
                        test.Session.LockingOption = DevExpress.Xpo.LockingOption.None;
                        test.AlarmTime = null;
                        test.Save();
                        test.Session.CommitTransaction();
                    }
                    else if (((Notification)x).NotificationSource.GetType() == typeof(ProjectTemplate))
                    {
                        ProjectTemplate test = ObjectSpace.FindObject<ProjectTemplate>(CriteriaOperator.Parse("Oid = '" + ((Notification)x).NotificationSource.UniqueId + "'"));
                        test.Session.LockingOption = DevExpress.Xpo.LockingOption.None;
                        test.AlarmTime = null;
                        test.Save();
                        test.Session.CommitTransaction();
                    }
                    else if (((Notification)x).NotificationSource.GetType() == typeof(QANonComplains))
                    {
                        QANonComplains test = ObjectSpace.FindObject<QANonComplains>(CriteriaOperator.Parse("Oid = '" + ((Notification)x).NotificationSource.UniqueId + "'"));
                        test.Session.LockingOption = DevExpress.Xpo.LockingOption.None;
                        //test.AlarmTime = null;
                        test.Save();
                        test.Session.CommitTransaction();
                    }
                    //else if (((Notification)x).NotificationSource.GetType() == typeof(QAProjectEntity))
                    //{
                    //    QAProjectEntity test = ObjectSpace.FindObject<QAProjectEntity>(CriteriaOperator.Parse("Oid = '" + ((Notification)x).NotificationSource.UniqueId + "'"));
                    //    test.Session.LockingOption = DevExpress.Xpo.LockingOption.None;
                    //    test.AlarmTime = null;
                    //    test.Save();
                    //    test.Session.CommitTransaction();
                    //}
                    // ((Notification)x).NotificationSource.AlarmTime = null;
                }
                Frame.GetController<NotificationsDialogViewController>().View.Close();
            }

            protected override void OnDeactivated()
            {
                Frame.GetController<NotificationsDialogViewController>().Dismiss.Execute -= Dismiss_Execute;
                Frame.GetController<NotificationsDialogViewController>().Snooze.Execute -= Snooze_Execute;
                base.ViewCurrentObject.ShowNotificationsWindow = true;
                base.OnDeactivated();
            }


        }
    }
}
