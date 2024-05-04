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
using DevExpress.ExpressApp.Web.SystemModule;

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class AriaObjectShelveEventViewController : ViewController
    {
        public AriaObjectShelveEventViewController()
        {
            InitializeComponent();
            RegisterActions(components);
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


        public static Guid CurrentAriaObjectShelveEvent;
        public static Guid CurrentAriaObjectShelve;

        private void AriaObjectShelveEventViewController_Activated(object sender, EventArgs e)
        {
            if (View is DetailView)
            {
                if (((AriaObjectShelveEvent)(View as DetailView).CurrentObject).AriaObjectShelve.TrackingEntry != null &&
               ((AriaObjectShelveEvent)(View as DetailView).CurrentObject).AriaObjectShelve.TrackingEntry.Status == TrackingEntry.TrackingStatus.Complete)
                {
                    Frame.GetController<ModificationsController>().Actions["Save"].Enabled.SetItemValue("Disable", false);
                    Frame.GetController<ModificationsController>().Actions["SaveAndClose"].Enabled.SetItemValue("Disable", false);
                    Frame.GetController<ModificationsController>().Actions["SaveAndNew"].Enabled.SetItemValue("Disable", false);
                    Frame.GetController<NewObjectViewController>().Actions["New"].Enabled.SetItemValue("Disable", false);
                    Frame.GetController<LinkUnlinkController>().Actions["Link"].Enabled.SetItemValue("Disable", false);
                    Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", false);
                    Frame.GetController<WebModificationsController>().Actions["SwitchToEditMode"].Enabled.SetItemValue("Disable", false);
                    Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Enabled.SetItemValue("Disable", false);

                    foreach (ListPropertyEditor editor in (View as DetailView).GetItems<ListPropertyEditor>())
                    {

                        if (((AriaObjectShelveEvent)editor.CurrentObject).AriaObjectShelve.TrackingEntry != null && ((AriaObjectShelveEvent)editor.CurrentObject).AriaObjectShelve.TrackingEntry.Status == TrackingEntry.TrackingStatus.Complete)
                        {
                            editor.ControlCreated += editor_ControlCreated;
                        }
                    }
                }
                else
                {
                    Frame.GetController<ModificationsController>().Actions["Save"].Enabled.SetItemValue("Disable", true);
                    Frame.GetController<ModificationsController>().Actions["SaveAndClose"].Enabled.SetItemValue("Disable", true);
                    Frame.GetController<ModificationsController>().Actions["SaveAndNew"].Enabled.SetItemValue("Disable", true);
                    Frame.GetController<NewObjectViewController>().Actions["New"].Enabled.SetItemValue("Disable", true);
                    Frame.GetController<LinkUnlinkController>().Actions["Link"].Enabled.SetItemValue("Disable", true);
                    Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", true);
                    Frame.GetController<WebModificationsController>().Actions["SwitchToEditMode"].Enabled.SetItemValue("Disable", true);
                    Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Enabled.SetItemValue("Disable", true);
                
                }
            }

            if (this.View != null && this.View.CurrentObject != null)
            {
                CurrentAriaObjectShelveEvent = ((AriaObjectShelveEvent)this.View.CurrentObject).Oid;
                if (((AriaObjectShelveEvent)this.View.CurrentObject).AriaObjectShelve != null)
                {
                    CurrentAriaObjectShelve = ((AriaObjectShelveEvent)this.View.CurrentObject).AriaObjectShelve.Oid;
                }
                else
                {
                    CurrentAriaObjectShelve = Guid.Empty;
                }


            }
        }

        void editor_ControlCreated(object sender, EventArgs e)
        {
            foreach (ListPropertyEditor editor in (View as DetailView).GetItems<ListPropertyEditor>())
            {
                if (editor.Frame != null)
                {
                    if (editor.PropertyName == "AriaObjectShelveEventParameters")
                    {
                        editor.Frame.GetController<ListViewController>().Actions["Edit"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<NewObjectViewController>().Actions["New"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<LinkUnlinkController>().Actions["Link"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<WebModificationsController>().Actions["SwitchToEditMode"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<AriaObjectShelveEventParameterViewController>().Actions["AriaObjectShelveEventParameterSelectEventParametersToModify"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<AriaObjectShelveEventParameterViewController>().Actions["AriaObjectShelveEventParameterSelectEventParametersToDelete"].Enabled.SetItemValue("Disable", false);
                    }
                }
            }
        }

        private void SelectEventsToModify_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            CollectionSource newCollectionSource = new CollectionSource(objectSpace, typeof(AriaObjectBrowseEvent));
            if (AriaObjectShelveViewController.CurrentAriaObject != Guid.Empty)
            {
                List<string> ariaObjectEvents = objectSpace.GetObjectByKey<AriaObject>(AriaObjectShelveViewController.CurrentAriaObject).AriaObjectEvents.Select(r => r.EventName).Distinct().ToList();

                List<string> ariaObjectShelveEvents = objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveViewController.CurrentAriaObjectShelve).AriaObjectShelveEvents.Select(r => r.EventName).ToList();

                List<string> events = ariaObjectEvents.Except(ariaObjectShelveEvents).ToList();
                events.Sort();

                foreach (var eventName in events)
                {
                    var objectevent = objectSpace.CreateObject<AriaObjectBrowseEvent>();

                    var eventObject = objectSpace.GetObjectByKey<AriaObject>(AriaObjectShelveViewController.CurrentAriaObject).AriaObjectEvents.OrderByDescending(r => r.ObjectRevision).FirstOrDefault(r => r.EventName == eventName);

                    objectevent.EventName = eventName;
                    objectevent.EventOid = eventObject.Oid.ToString();
                    objectevent.EventDescription = eventObject.EventDescription;
                    objectevent.KeyProperty = Guid.NewGuid().ToString();
                    newCollectionSource.Add(objectevent);
                }
            }
            ListView vwAriaObject = Application.CreateListView(Application.FindListViewId(typeof(AriaObjectBrowseEvent)), newCollectionSource, false);

            e.View = vwAriaObject;
        }

        private void SelectEventsToModify_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            var AriaObjectShelve = objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveViewController.CurrentAriaObjectShelve);

            foreach (AriaObjectBrowseEvent selectedAriaObjectBrowse in e.PopupWindow.View.SelectedObjects)
            {
                AriaObjectEvent selectedAriaObject = objectSpace.GetObjectByKey<AriaObjectEvent>(Guid.Parse(selectedAriaObjectBrowse.EventOid));

                var events = new AriaObjectShelveEvent(AriaObjectShelve.Session);

                events.EventName = selectedAriaObject.EventName;
                events.EventDescription = selectedAriaObject.EventDescription;
                events.ModificationType = Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveEvent.ModificationTypes.Modify;
                events.AriaObjectShelve = AriaObjectShelve;


                AriaObjectShelve.AriaObjectShelveEvents.Add(events);

                AriaObjectShelve.Save();

                AriaObjectShelve.Session.CommitTransaction();
            }

            ((NestedFrame)Frame).ViewItem.View.ObjectSpace.Refresh();
            
        }

        private void SelectEventsToDelete_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            CollectionSource newCollectionSource = new CollectionSource(objectSpace, typeof(AriaObjectBrowseEvent));
            
            if (AriaObjectShelveViewController.CurrentAriaObject != Guid.Empty)
            {
                List<string> ariaObjectEvents = objectSpace.GetObjectByKey<AriaObject>(AriaObjectShelveViewController.CurrentAriaObject).AriaObjectEvents.Select(r => r.EventName).Distinct().ToList();

                List<string> ariaObjectShelveEvents = objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveViewController.CurrentAriaObjectShelve).AriaObjectShelveEvents.Select(r => r.EventName).ToList();

                List<string> events = ariaObjectEvents.Except(ariaObjectShelveEvents).ToList();
                events.Sort();

                foreach (var eventName in events)
                {
                    var objectEvent = objectSpace.CreateObject<AriaObjectBrowseEvent>();

                    var eventObject = objectSpace.GetObjectByKey<AriaObject>(AriaObjectShelveViewController.CurrentAriaObject).AriaObjectEvents.OrderByDescending(r => r.ObjectRevision).FirstOrDefault(r => r.EventName == eventName);

                    objectEvent.EventName = eventName;
                    objectEvent.EventOid = eventObject.Oid.ToString();
                    objectEvent.EventDescription = eventObject.EventDescription;
                    objectEvent.KeyProperty = Guid.NewGuid().ToString();
                    newCollectionSource.Add(objectEvent);
                }
            }
            ListView vwAriaObject = Application.CreateListView(Application.FindListViewId(typeof(AriaObjectBrowseEvent)), newCollectionSource, false);

            e.View = vwAriaObject;
        }

        private void SelectEventsToDelete_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            var AriaObjectShelve = objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveViewController.CurrentAriaObjectShelve);

            foreach (AriaObjectBrowseEvent selectedAriaObjectBrowse in e.PopupWindow.View.SelectedObjects)
            {
                AriaObjectEvent selectedAriaObject = objectSpace.GetObjectByKey<AriaObjectEvent>(Guid.Parse(selectedAriaObjectBrowse.EventOid));

                var newEvent = new AriaObjectShelveEvent(AriaObjectShelve.Session);

                newEvent.EventName = selectedAriaObject.EventName;
                newEvent.EventDescription = selectedAriaObject.EventDescription;
                newEvent.ModificationType = Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveEvent.ModificationTypes.Delete;
                newEvent.AriaObjectShelve = AriaObjectShelve;


                AriaObjectShelve.AriaObjectShelveEvents.Add(newEvent);

                AriaObjectShelve.Save();

                AriaObjectShelve.Session.CommitTransaction();
            }

            ((NestedFrame)Frame).ViewItem.View.ObjectSpace.Refresh();    
        }
    }
}
