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
    public partial class AriaObjectShelvePropertyViewController : ViewController
    {
        public AriaObjectShelvePropertyViewController()
        {
            InitializeComponent();
            RegisterActions(components);
            // Target required Views (via the TargetXXX properties) and create their Actions.
        }

        protected override void OnAfterConstruction()
        {
            base.OnAfterConstruction();
        }



        protected override void EndUpdate()
        {
            base.EndUpdate();
        }
        protected override void OnActivated()
        {
            base.OnActivated();
            // Perform various tasks depending on the target View.
        }

        protected override void OnViewControllersActivated()
        {
            base.OnViewControllersActivated();
        }
        protected override void OnViewChanged()
        {
            base.OnViewChanged();
        }
        protected override void OnViewControlsCreated()
        {
            base.OnViewControlsCreated();
          
        }

        protected override void OnFrameAssigned()
        {
            base.OnFrameAssigned();
        }
        
        protected override void OnDeactivated()
        {
            // Unsubscribe from previously subscribed events and release other references and resources.
            base.OnDeactivated();
        }


        private void SelectPropertiesToModify_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            CollectionSource newCollectionSource = new CollectionSource(objectSpace, typeof(AriaObjectBrowseProperty));
            
            if (AriaObjectShelveViewController.CurrentAriaObject != Guid.Empty)
            {
                List<string> ariaObjectProps = objectSpace.GetObjectByKey<AriaObject>(AriaObjectShelveViewController.CurrentAriaObject).AriaObjectProperties.Select(r => r.PropertyName).Distinct().ToList();

                List<string> ariaObjectShelveProps = objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveViewController.CurrentAriaObjectShelve).AriaObjectShelveProperties.Select(r => r.PropertyName).ToList();

                List<string> props = ariaObjectProps.Except(ariaObjectShelveProps).ToList();
                props.Sort();

                foreach (var propName in props)
                {
                    var objectProp = objectSpace.CreateObject<AriaObjectBrowseProperty>();

                    var porpObject = objectSpace.GetObjectByKey<AriaObject>(AriaObjectShelveViewController.CurrentAriaObject).AriaObjectProperties.OrderByDescending(r => r.ObjectRevision).FirstOrDefault(r => r.PropertyName == propName);

                    objectProp.PropertyName = propName;
                    objectProp.PropertyOid = porpObject.Oid.ToString();
                    objectProp.PropertyDescription = porpObject.PropertyDescription;
                    objectProp.KeyProperty = Guid.NewGuid().ToString();
                    objectProp.PropertyType = porpObject.PropertyType == null ? null : porpObject.Session.GetObjectByKey<PropertyType>(porpObject.PropertyType.Oid);

                    newCollectionSource.Add(objectProp);
                }
            }
            ListView vwAriaObject = Application.CreateListView(Application.FindListViewId(typeof(AriaObjectBrowseProperty)), newCollectionSource, false);

            e.View = vwAriaObject;
            
        }

        private void SelectPropertiesToModify_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            var AriaObjectShelve = objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveViewController.CurrentAriaObjectShelve);

            foreach (AriaObjectBrowseProperty selectedAriaObjectBrowse in e.PopupWindow.View.SelectedObjects)
            {
                AriaObjectProperty selectedAriaObject = objectSpace.GetObjectByKey<AriaObjectProperty>(Guid.Parse(selectedAriaObjectBrowse.PropertyOid));

                var prop = new AriaObjectShelveProperty(AriaObjectShelve.Session);

                prop.PropertyName = selectedAriaObject.PropertyName;
                prop.PropertyDescription = selectedAriaObject.PropertyDescription;
                prop.PropertyType = selectedAriaObject.PropertyType == null ? null : prop.Session.GetObjectByKey<PropertyType>(selectedAriaObject.PropertyType.Oid);
                prop.Required = selectedAriaObject.Required;
                prop.ModificationType = Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveProperty.ModificationTypes.Modify;
                prop.AriaObjectShelve = AriaObjectShelve;


                if (selectedAriaObject.PropertyType != null)
                {
                    foreach (var settingType in selectedAriaObject.PropertyType.SettingTypes)
                    {
                        AriaObjectShelvePropertySetting shelvePropSetting = new AriaObjectShelvePropertySetting(selectedAriaObject.Session);

                        shelvePropSetting.DataType = settingType.DataType.ToString();
                        shelvePropSetting.DecimalPlaces = settingType.Decimal;
                        shelvePropSetting.Modified = false;
                        shelvePropSetting.SettingType = settingType;
                        if (selectedAriaObject.AriaObjectPropertiesSettings.Where(r => r.SettingType.Oid == settingType.Oid).Count() > 0)
                        {
                            shelvePropSetting.Value = selectedAriaObject.AriaObjectPropertiesSettings.First(r => r.SettingType.Oid == settingType.Oid).Value;
                        }
                        shelvePropSetting.Width = settingType.Width;


                        prop.AriaObjectShelvePropertySettings.Add(shelvePropSetting);
                    }
                }

                AriaObjectShelve.AriaObjectShelveProperties.Add(prop);

                AriaObjectShelve.Save();

                AriaObjectShelve.Session.CommitTransaction();
            }

            ((NestedFrame)Frame).ViewItem.View.ObjectSpace.Refresh();
        }

        private void SelectPropertiesToDelete_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            CollectionSource newCollectionSource = new CollectionSource(objectSpace, typeof(AriaObjectBrowseProperty));

            if (AriaObjectShelveViewController.CurrentAriaObject != Guid.Empty)
            {
                List<string> ariaObjectProps = objectSpace.GetObjectByKey<AriaObject>(AriaObjectShelveViewController.CurrentAriaObject).AriaObjectProperties.Select(r => r.PropertyName).Distinct().ToList();

                List<string> ariaObjectShelveProps = objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveViewController.CurrentAriaObjectShelve).AriaObjectShelveProperties.Select(r => r.PropertyName).ToList();

                List<string> props = ariaObjectProps.Except(ariaObjectShelveProps).ToList();

                foreach (var propName in props)
                {
                    var objectProp = objectSpace.CreateObject<AriaObjectBrowseProperty>();

                    var porpObject = objectSpace.GetObjectByKey<AriaObject>(AriaObjectShelveViewController.CurrentAriaObject).AriaObjectProperties.OrderByDescending(r => r.ObjectRevision).FirstOrDefault(r => r.PropertyName == propName);

                    objectProp.PropertyName = propName;
                    objectProp.PropertyOid = porpObject.Oid.ToString();
                    objectProp.PropertyDescription = porpObject.PropertyDescription;
                    objectProp.KeyProperty = Guid.NewGuid().ToString();
                    objectProp.PropertyType = porpObject.PropertyType == null ? null : porpObject.Session.GetObjectByKey<PropertyType>(porpObject.PropertyType.Oid);

                    newCollectionSource.Add(objectProp);
                }
            }
            ListView vwAriaObject = Application.CreateListView(Application.FindListViewId(typeof(AriaObjectBrowseProperty)), newCollectionSource, false);

            e.View = vwAriaObject;
        }

        private void SelectPropertiesToDelete_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();
            foreach (AriaObjectBrowseProperty selectedAriaObjectBrowse in e.PopupWindow.View.SelectedObjects)
            {
                AriaObjectProperty selectedAriaObject = objectSpace.GetObjectByKey<AriaObjectProperty>(Guid.Parse(selectedAriaObjectBrowse.PropertyOid));

                var AriaObjectShelve = objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveViewController.CurrentAriaObjectShelve);

                var prop = new AriaObjectShelveProperty(AriaObjectShelve.Session);
                prop.PropertyName = selectedAriaObject.PropertyName;
                prop.PropertyDescription = selectedAriaObject.PropertyDescription;

                prop.PropertyType = selectedAriaObject.PropertyType == null ? null : prop.Session.GetObjectByKey<PropertyType>(selectedAriaObject.PropertyType.Oid);
                prop.Required = selectedAriaObject.Required;

                prop.ModificationType = Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveProperty.ModificationTypes.Delete;
                prop.AriaObjectShelve = AriaObjectShelve;

                AriaObjectShelve.AriaObjectShelveProperties.Add(prop);

                AriaObjectShelve.Save();

                AriaObjectShelve.Session.CommitTransaction();
            }

            ((NestedFrame)Frame).ViewItem.View.ObjectSpace.Refresh();
        }

        private void AriaObjectShelvePropertyViewController_Activated(object sender, EventArgs e)
        {
            if ((View is DetailView))
            {
                if (((AriaObjectShelveProperty)(View as DetailView).CurrentObject).AriaObjectShelve.TrackingEntry != null &&
                ((AriaObjectShelveProperty)(View as DetailView).CurrentObject).AriaObjectShelve.TrackingEntry.Status == TrackingEntry.TrackingStatus.Complete)
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

                        if (((AriaObjectShelveProperty)editor.CurrentObject).AriaObjectShelve.TrackingEntry != null && ((AriaObjectShelveProperty)editor.CurrentObject).AriaObjectShelve.TrackingEntry.Status == TrackingEntry.TrackingStatus.Complete)
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
                    if (editor.PropertyName == "AriaObjectShelvePropertySettings")
                    {
                        editor.Frame.GetController<ListViewController>().Actions["Edit"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<WebModificationsController>().Actions["SwitchToEditMode"].Enabled.SetItemValue("Disable", false);
                    }                  
                }
            }
        }

        private void AriaObjectShelvePropertyViewController_AfterConstruction(object sender, EventArgs e)
        {
        }

        private void AriaObjectShelvePropertyViewController_ViewControlsCreated(object sender, EventArgs e)
        {

        }
    }
}
