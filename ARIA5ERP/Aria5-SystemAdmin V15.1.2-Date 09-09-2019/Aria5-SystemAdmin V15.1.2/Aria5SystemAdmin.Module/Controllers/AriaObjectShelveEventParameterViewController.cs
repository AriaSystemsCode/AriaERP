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
    public partial class AriaObjectShelveEventParameterViewController : ViewController
    {
        public AriaObjectShelveEventParameterViewController()
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

        private void AriaObjectShelveEventParameterViewController_Activated(object sender, EventArgs e)
        {
            if ((View is DetailView))
            {
                if (((AriaObjectShelveEventParameter)(View as DetailView).CurrentObject).AriaObjectShelveEvent.AriaObjectShelve.TrackingEntry != null &&
                ((AriaObjectShelveEventParameter)(View as DetailView).CurrentObject).AriaObjectShelveEvent.AriaObjectShelve.TrackingEntry.Status == TrackingEntry.TrackingStatus.Complete)
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

                        if (((AriaObjectShelveEventParameter)editor.CurrentObject).AriaObjectShelveEvent.AriaObjectShelve.TrackingEntry != null && ((AriaObjectShelveEventParameter)editor.CurrentObject).AriaObjectShelveEvent.AriaObjectShelve.TrackingEntry.Status == TrackingEntry.TrackingStatus.Complete)
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
        }

        void editor_ControlCreated(object sender, EventArgs e)
        {
            foreach (ListPropertyEditor editor in (View as DetailView).GetItems<ListPropertyEditor>())
            {
                if (editor.Frame != null)
                {
                    if (editor.PropertyName == "AriaObjectShelveEventParameterSettings")
                    {
                        editor.Frame.GetController<ListViewController>().Actions["Edit"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<WebModificationsController>().Actions["SwitchToEditMode"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Enabled.SetItemValue("Disable", false);
                    }
                }
            }
        }

        private void SelectEventParametersToModify_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            CollectionSource newCollectionSource = new CollectionSource(objectSpace, typeof(AriaObjectBrowseEventParameter));
            
            if (objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveEventViewController.CurrentAriaObjectShelve).AriaObject != null)
            {
                var objectOid = objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveEventViewController.CurrentAriaObjectShelve).AriaObject.Oid;
                var eventName = objectSpace.GetObjectByKey<AriaObjectShelveEvent>(AriaObjectShelveEventViewController.CurrentAriaObjectShelveEvent).EventName;

                List<string> ariaObjectEventParameters = objectSpace.GetObjectByKey<AriaObject>(objectOid).AriaObjectEvents.Where(r => r.EventName == eventName).Select(r => r.AriaObjectEventParameters).FirstOrDefault().Select(r => r.ParameterName).ToList();

                List<string> ariaObjectShelveEventParameters = objectSpace.GetObjectByKey<AriaObjectShelveEvent>(AriaObjectShelveEventViewController.CurrentAriaObjectShelveEvent).AriaObjectShelveEventParameters.Select(r => r.ParameterName).ToList();

                List<string> para = ariaObjectEventParameters.Except(ariaObjectShelveEventParameters).ToList();
                para.Sort();

                foreach (var parName in para)
                {
                    var objectpara = objectSpace.CreateObject<AriaObjectBrowseEventParameter>();

                    var paraObject = objectSpace.GetObjectByKey<AriaObject>(objectOid).AriaObjectEvents.Where(r => r.EventName == eventName).FirstOrDefault().AriaObjectEventParameters.FirstOrDefault(r => r.ParameterName == parName);

                    objectpara.ParameterName = parName;
                    objectpara.EventParameterOid = paraObject.Oid.ToString();
                    objectpara.ParameterNo = paraObject.ParameterNo;
                    objectpara.KeyProperty = Guid.NewGuid().ToString();
                    objectpara.ParameterType = paraObject.ParameterType == null ? null : paraObject.Session.GetObjectByKey<PropertyType>(paraObject.ParameterType.Oid);

                    newCollectionSource.Add(objectpara);
                }
            }
            ListView vwAriaObject = Application.CreateListView(Application.FindListViewId(typeof(AriaObjectBrowseEventParameter)), newCollectionSource, false);

            e.View = vwAriaObject;
        }

        private void SelectEventParametersToModify_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            var AriaObjectShelveEvent = objectSpace.GetObjectByKey<AriaObjectShelveEvent>(AriaObjectShelveEventViewController.CurrentAriaObjectShelveEvent);

            foreach (AriaObjectBrowseEventParameter selectedAriaObjectBrowse in e.PopupWindow.View.SelectedObjects)
            {
                AriaObjectEventParameter selectedAriaObjectEventParameter = objectSpace.GetObjectByKey<AriaObjectEventParameter>(Guid.Parse(selectedAriaObjectBrowse.EventParameterOid));

                var para = new AriaObjectShelveEventParameter(AriaObjectShelveEvent.Session);

                para.ParameterName = selectedAriaObjectEventParameter.ParameterName;
                para.ParameterNo = selectedAriaObjectEventParameter.ParameterNo;
                para.ParameterType = selectedAriaObjectEventParameter.ParameterType == null ? null : para.Session.GetObjectByKey<PropertyType>(selectedAriaObjectEventParameter.ParameterType.Oid);

                para.ModificationType = Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveEventParameter.ModificationTypes.Modify;
                para.AriaObjectShelveEvent = AriaObjectShelveEvent;


                if (selectedAriaObjectEventParameter.ParameterType != null)
                {
                    foreach (var settingType in selectedAriaObjectEventParameter.ParameterType.SettingTypes)
                    {
                        AriaObjectShelveEventParameterSetting shelveEventParaSetting = new AriaObjectShelveEventParameterSetting(selectedAriaObjectEventParameter.Session);

                        shelveEventParaSetting.DataType = settingType.DataType.ToString();
                        shelveEventParaSetting.DecimalPlaces = settingType.Decimal;
                        shelveEventParaSetting.Modified = false;
                        shelveEventParaSetting.SettingType = settingType;
                        if (selectedAriaObjectEventParameter.AriaObjectEventParameterSettings.Where(r => r.SettingType.Oid == settingType.Oid).Count() > 0)
                        {
                            shelveEventParaSetting.Value = selectedAriaObjectEventParameter.AriaObjectEventParameterSettings.First(r => r.SettingType.Oid == settingType.Oid).Value;
                        }
                        shelveEventParaSetting.Width = settingType.Width;


                        para.AriaObjectShelveEventParameterSettings.Add(shelveEventParaSetting);
                    }
                }

                AriaObjectShelveEvent.AriaObjectShelveEventParameters.Add(para);

                AriaObjectShelveEvent.Save();

                AriaObjectShelveEvent.Session.CommitTransaction();
            }

            ((NestedFrame)Frame).ViewItem.View.ObjectSpace.Refresh();
        }

        private void SelectEventParametersToDelete_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            CollectionSource newCollectionSource = new CollectionSource(objectSpace, typeof(AriaObjectBrowseEventParameter));

            if (objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveEventViewController.CurrentAriaObjectShelve).AriaObject != null)
            {
                var objectOid = objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveEventViewController.CurrentAriaObjectShelve).AriaObject.Oid;
                var eventName = objectSpace.GetObjectByKey<AriaObjectShelveEvent>(AriaObjectShelveEventViewController.CurrentAriaObjectShelveEvent).EventName;

                List<string> ariaObjectEventParameters = objectSpace.GetObjectByKey<AriaObject>(objectOid).AriaObjectEvents.Where(r => r.EventName == eventName).Select(r => r.AriaObjectEventParameters).FirstOrDefault().Select(r => r.ParameterName).ToList();

                List<string> ariaObjectShelveEventParameters = objectSpace.GetObjectByKey<AriaObjectShelveEvent>(AriaObjectShelveEventViewController.CurrentAriaObjectShelveEvent).AriaObjectShelveEventParameters.Select(r => r.ParameterName).ToList();

                List<string> para = ariaObjectEventParameters.Except(ariaObjectShelveEventParameters).ToList();
                para.Sort();

                foreach (var parName in para)
                {
                    var objectpara = objectSpace.CreateObject<AriaObjectBrowseEventParameter>();

                    var paraObject = objectSpace.GetObjectByKey<AriaObject>(objectOid).AriaObjectEvents.Where(r => r.EventName == eventName).FirstOrDefault().AriaObjectEventParameters.FirstOrDefault(r => r.ParameterName == parName);

                    objectpara.ParameterName = parName;
                    objectpara.EventParameterOid = paraObject.Oid.ToString();
                    objectpara.ParameterNo = paraObject.ParameterNo;
                    objectpara.KeyProperty = Guid.NewGuid().ToString();
                    objectpara.ParameterType = paraObject.ParameterType == null ? null : paraObject.Session.GetObjectByKey<PropertyType>(paraObject.ParameterType.Oid);

                    newCollectionSource.Add(objectpara);
                }
            }
            ListView vwAriaObject = Application.CreateListView(Application.FindListViewId(typeof(AriaObjectBrowseEventParameter)), newCollectionSource, false);

            e.View = vwAriaObject;      
        }

        private void SelectEventParametersToDelete_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            var AriaObjectShelveEvent = objectSpace.GetObjectByKey<AriaObjectShelveEvent>(AriaObjectShelveEventViewController.CurrentAriaObjectShelveEvent);

            foreach (AriaObjectBrowseEventParameter selectedAriaObjectBrowse in e.PopupWindow.View.SelectedObjects)
            {
                AriaObjectEventParameter selectedAriaObjectEventParameter = objectSpace.GetObjectByKey<AriaObjectEventParameter>(Guid.Parse(selectedAriaObjectBrowse.EventParameterOid));

                var prop = new AriaObjectShelveEventParameter(AriaObjectShelveEvent.Session);

                prop.ParameterName = selectedAriaObjectEventParameter.ParameterName;
                prop.ParameterNo = selectedAriaObjectEventParameter.ParameterNo;
                prop.ParameterType = selectedAriaObjectEventParameter.ParameterType == null ? null : prop.Session.GetObjectByKey<PropertyType>(selectedAriaObjectEventParameter.ParameterType.Oid);

                prop.ModificationType = Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveEventParameter.ModificationTypes.Delete;
                prop.AriaObjectShelveEvent = AriaObjectShelveEvent;


                if (selectedAriaObjectEventParameter.ParameterType != null)
                {
                    foreach (var settingType in selectedAriaObjectEventParameter.ParameterType.SettingTypes)
                    {
                        AriaObjectShelveEventParameterSetting shelvePropSetting = new AriaObjectShelveEventParameterSetting(selectedAriaObjectEventParameter.Session);

                        shelvePropSetting.DataType = settingType.DataType.ToString();
                        shelvePropSetting.DecimalPlaces = settingType.Decimal;
                        shelvePropSetting.Modified = false;
                        shelvePropSetting.SettingType = settingType;
                        if (selectedAriaObjectEventParameter.AriaObjectEventParameterSettings.Where(r => r.SettingType.Oid == settingType.Oid).Count() > 0)
                        {
                            shelvePropSetting.Value = selectedAriaObjectEventParameter.AriaObjectEventParameterSettings.First(r => r.SettingType.Oid == settingType.Oid).Value;
                        }
                        shelvePropSetting.Width = settingType.Width;


                        prop.AriaObjectShelveEventParameterSettings.Add(shelvePropSetting);
                    }
                }

                AriaObjectShelveEvent.AriaObjectShelveEventParameters.Add(prop);

                AriaObjectShelveEvent.Save();

                AriaObjectShelveEvent.Session.CommitTransaction();
            }

            ((NestedFrame)Frame).ViewItem.View.ObjectSpace.Refresh();
        }
    }
}
