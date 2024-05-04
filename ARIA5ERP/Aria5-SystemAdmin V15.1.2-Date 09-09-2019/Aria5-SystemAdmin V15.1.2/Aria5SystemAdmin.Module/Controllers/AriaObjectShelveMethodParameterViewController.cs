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
    public partial class AriaObjectShelveMethodParameterViewController : ViewController
    {
        public AriaObjectShelveMethodParameterViewController()
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

        private void SelectEventsToModify_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            CollectionSource newCollectionSource = new CollectionSource(objectSpace, typeof(AriaObjectBrowseMethodParameter));
  
            if (objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveMethodViewController.CurrentAriaObjectShelve).AriaObject != null)
            {
                var objectOid = objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveMethodViewController.CurrentAriaObjectShelve).AriaObject.Oid;
                var methodName = objectSpace.GetObjectByKey<AriaObjectShelveMethod>(AriaObjectShelveMethodViewController.CurrentAriaObjectShelveMethod).MethodName;

                List<string> ariaObjectMethodParameters = objectSpace.GetObjectByKey<AriaObject>(objectOid).AriaObjectMethods.Where(r => r.MethodName == methodName).Select(r => r.AriaObjectMethodParameters).FirstOrDefault().Select(r => r.ParameterName).ToList();

                List<string> ariaObjectShelveMethodParameters = objectSpace.GetObjectByKey<AriaObjectShelveMethod>(AriaObjectShelveMethodViewController.CurrentAriaObjectShelveMethod).AriaObjectShelveMethodParameters.Select(r => r.ParameterName).ToList();

                List<string> para = ariaObjectMethodParameters.Except(ariaObjectShelveMethodParameters).ToList();
                para.Sort();

                foreach (var parName in para)
                {
                    var objectpara = objectSpace.CreateObject<AriaObjectBrowseMethodParameter>();

                    var paraObject = objectSpace.GetObjectByKey<AriaObject>(objectOid).AriaObjectMethods.Where(r => r.MethodName == methodName).FirstOrDefault().AriaObjectMethodParameters.FirstOrDefault(r => r.ParameterName == parName);

                    objectpara.ParameterName = parName;
                    objectpara.MethodParameterOid = paraObject.Oid.ToString();
                    objectpara.ParameterNo = paraObject.ParameterNo;
                    objectpara.KeyProperty = Guid.NewGuid().ToString();
                    objectpara.ParameterType = paraObject.ParameterType == null ? null : paraObject.Session.GetObjectByKey<PropertyType>(paraObject.ParameterType.Oid);

                    newCollectionSource.Add(objectpara);
                }
            }
            ListView vwAriaObject = Application.CreateListView(Application.FindListViewId(typeof(AriaObjectBrowseMethodParameter)), newCollectionSource, false);

            e.View = vwAriaObject;
            
        }

        private void SelectEventsToModify_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {

            IObjectSpace objectSpace = Application.CreateObjectSpace();

            var AriaObjectShelveMethod = objectSpace.GetObjectByKey<AriaObjectShelveMethod>(AriaObjectShelveMethodViewController.CurrentAriaObjectShelveMethod);

            foreach (AriaObjectBrowseMethodParameter selectedAriaObjectBrowse in e.PopupWindow.View.SelectedObjects)
            {
                AriaObjectMethodParameter selectedAriaObjectMethodParameter = objectSpace.GetObjectByKey<AriaObjectMethodParameter>(Guid.Parse(selectedAriaObjectBrowse.MethodParameterOid));

                var prop = new AriaObjectShelveMethodParameter(AriaObjectShelveMethod.Session);

                prop.ParameterName = selectedAriaObjectMethodParameter.ParameterName;
                prop.ParameterNo = selectedAriaObjectMethodParameter.ParameterNo;
                prop.ParameterType = selectedAriaObjectMethodParameter.ParameterType == null ? null : prop.Session.GetObjectByKey<PropertyType>(selectedAriaObjectMethodParameter.ParameterType.Oid);
               
                prop.ModificationType = Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveMethodParameter.ModificationTypes.Modify;
                prop.AriaObjectShelveMethod = AriaObjectShelveMethod;


                if (selectedAriaObjectMethodParameter.ParameterType != null)
                {
                    foreach (var settingType in selectedAriaObjectMethodParameter.ParameterType.SettingTypes)
                    {
                        AriaObjectShelveMethodParameterSetting shelvePropSetting = new AriaObjectShelveMethodParameterSetting(selectedAriaObjectMethodParameter.Session);

                        shelvePropSetting.DataType = settingType.DataType.ToString();
                        shelvePropSetting.DecimalPlaces = settingType.Decimal;
                        shelvePropSetting.Modified = false;
                        shelvePropSetting.SettingType = settingType;
                        if (selectedAriaObjectMethodParameter.AriaObjectMethodParameterSettings.Where(r => r.SettingType.Oid == settingType.Oid).Count() > 0)
                        {
                            shelvePropSetting.Value = selectedAriaObjectMethodParameter.AriaObjectMethodParameterSettings.First(r => r.SettingType.Oid == settingType.Oid).Value;
                        }
                        shelvePropSetting.Width = settingType.Width;


                        prop.AriaObjectShelveMethodParameterSettings.Add(shelvePropSetting);
                    }
                }

                AriaObjectShelveMethod.AriaObjectShelveMethodParameters.Add(prop);

                AriaObjectShelveMethod.Save();

                AriaObjectShelveMethod.Session.CommitTransaction();
            }

            ((NestedFrame)Frame).ViewItem.View.ObjectSpace.Refresh();


           
   
        }

        private void SelectEventsToDelete_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            CollectionSource newCollectionSource = new CollectionSource(objectSpace, typeof(AriaObjectBrowseMethodParameter));

            if (objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveMethodViewController.CurrentAriaObjectShelve).AriaObject != null)
            {
                var objectOid = objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveMethodViewController.CurrentAriaObjectShelve).AriaObject.Oid;
                var methodName = objectSpace.GetObjectByKey<AriaObjectShelveMethod>(AriaObjectShelveMethodViewController.CurrentAriaObjectShelveMethod).MethodName;

                List<string> ariaObjectMethodParameters = objectSpace.GetObjectByKey<AriaObject>(objectOid).AriaObjectMethods.Where(r => r.MethodName == methodName).Select(r => r.AriaObjectMethodParameters).FirstOrDefault().Select(r => r.ParameterName).ToList();

                List<string> ariaObjectShelveMethodParameters = objectSpace.GetObjectByKey<AriaObjectShelveMethod>(AriaObjectShelveMethodViewController.CurrentAriaObjectShelveMethod).AriaObjectShelveMethodParameters.Select(r => r.ParameterName).ToList();

                List<string> para = ariaObjectMethodParameters.Except(ariaObjectShelveMethodParameters).ToList();
                para.Sort();

                foreach (var parName in para)
                {
                    var objectpara = objectSpace.CreateObject<AriaObjectBrowseMethodParameter>();

                    var paraObject = objectSpace.GetObjectByKey<AriaObject>(objectOid).AriaObjectMethods.Where(r => r.MethodName == methodName).FirstOrDefault().AriaObjectMethodParameters.FirstOrDefault(r => r.ParameterName == parName);

                    objectpara.ParameterName = parName;
                    objectpara.MethodParameterOid = paraObject.Oid.ToString();
                    objectpara.ParameterNo = paraObject.ParameterNo;
                    objectpara.KeyProperty = Guid.NewGuid().ToString();
                    objectpara.ParameterType = paraObject.ParameterType == null ? null : paraObject.Session.GetObjectByKey<PropertyType>(paraObject.ParameterType.Oid);

                    newCollectionSource.Add(objectpara);
                }
            }
            ListView vwAriaObject = Application.CreateListView(Application.FindListViewId(typeof(AriaObjectBrowseMethodParameter)), newCollectionSource, false);

            e.View = vwAriaObject;           
        }

        private void SelectEventsToDelete_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {

            IObjectSpace objectSpace = Application.CreateObjectSpace();

            var AriaObjectShelveMethod = objectSpace.GetObjectByKey<AriaObjectShelveMethod>(AriaObjectShelveMethodViewController.CurrentAriaObjectShelveMethod);

            foreach (AriaObjectBrowseMethodParameter selectedAriaObjectBrowse in e.PopupWindow.View.SelectedObjects)
            {
                AriaObjectMethodParameter selectedAriaObjectMethodParameter = objectSpace.GetObjectByKey<AriaObjectMethodParameter>(Guid.Parse(selectedAriaObjectBrowse.MethodParameterOid));

                var prop = new AriaObjectShelveMethodParameter(AriaObjectShelveMethod.Session);

                prop.ParameterName = selectedAriaObjectMethodParameter.ParameterName;
                prop.ParameterNo = selectedAriaObjectMethodParameter.ParameterNo;
                prop.ParameterType = selectedAriaObjectMethodParameter.ParameterType == null ? null : prop.Session.GetObjectByKey<PropertyType>(selectedAriaObjectMethodParameter.ParameterType.Oid);

                prop.ModificationType = Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveMethodParameter.ModificationTypes.Delete;
                prop.AriaObjectShelveMethod = AriaObjectShelveMethod;


                if (selectedAriaObjectMethodParameter.ParameterType != null)
                {
                    foreach (var settingType in selectedAriaObjectMethodParameter.ParameterType.SettingTypes)
                    {
                        AriaObjectShelveMethodParameterSetting shelvePropSetting = new AriaObjectShelveMethodParameterSetting(selectedAriaObjectMethodParameter.Session);

                        shelvePropSetting.DataType = settingType.DataType.ToString();
                        shelvePropSetting.DecimalPlaces = settingType.Decimal;
                        shelvePropSetting.Modified = false;
                        shelvePropSetting.SettingType = settingType;
                        if (selectedAriaObjectMethodParameter.AriaObjectMethodParameterSettings.Where(r => r.SettingType.Oid == settingType.Oid).Count() > 0)
                        {
                            shelvePropSetting.Value = selectedAriaObjectMethodParameter.AriaObjectMethodParameterSettings.First(r => r.SettingType.Oid == settingType.Oid).Value;
                        }
                        shelvePropSetting.Width = settingType.Width;


                        prop.AriaObjectShelveMethodParameterSettings.Add(shelvePropSetting);
                    }
                }

                AriaObjectShelveMethod.AriaObjectShelveMethodParameters.Add(prop);

                AriaObjectShelveMethod.Save();

                AriaObjectShelveMethod.Session.CommitTransaction();
            }

            ((NestedFrame)Frame).ViewItem.View.ObjectSpace.Refresh();
            
        }

        private void AriaObjectShelveMethodParameterViewController_Activated(object sender, EventArgs e)
        {
            if ((View is DetailView))
            {
                if (((AriaObjectShelveMethodParameter)(View as DetailView).CurrentObject).AriaObjectShelveMethod.AriaObjectShelve.TrackingEntry != null &&
                ((AriaObjectShelveMethodParameter)(View as DetailView).CurrentObject).AriaObjectShelveMethod.AriaObjectShelve.TrackingEntry.Status == TrackingEntry.TrackingStatus.Complete)
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

                        if (((AriaObjectShelveMethodParameter)editor.CurrentObject).AriaObjectShelveMethod.AriaObjectShelve.TrackingEntry != null && ((AriaObjectShelveMethodParameter)editor.CurrentObject).AriaObjectShelveMethod.AriaObjectShelve.TrackingEntry.Status == TrackingEntry.TrackingStatus.Complete)
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
                    if (editor.PropertyName == "AriaObjectShelveMethodParameterSettings")
                    {
                        //editor.Frame.GetController<NewObjectViewController>().Actions["New"].Enabled.SetItemValue("Disable", false);                       
                        //editor.Frame.GetController<LinkUnlinkController>().Actions["Link"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<WebModificationsController>().Actions["SwitchToEditMode"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<ListViewController>().Actions["Edit"].Enabled.SetItemValue("Disable", false);
                    }

                }
            }
        }
    }
}
