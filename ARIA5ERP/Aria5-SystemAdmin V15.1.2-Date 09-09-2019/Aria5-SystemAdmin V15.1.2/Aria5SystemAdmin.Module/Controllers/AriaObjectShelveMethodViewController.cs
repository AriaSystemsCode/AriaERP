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
    public partial class AriaObjectShelveMethodViewController : ViewController
    {
        public AriaObjectShelveMethodViewController()
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

        public static Guid CurrentAriaObjectShelveMethod;
        public static Guid CurrentAriaObjectShelve;

       
        private void AriaObjectShelveMethodViewController_Activated(object sender, EventArgs e)
        {
            if (View is DetailView)
            {
                if (((AriaObjectShelveMethod)(View as DetailView).CurrentObject).AriaObjectShelve.TrackingEntry != null &&
                ((AriaObjectShelveMethod)(View as DetailView).CurrentObject).AriaObjectShelve.TrackingEntry.Status == TrackingEntry.TrackingStatus.Complete)
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

                        if (((AriaObjectShelveMethod)editor.CurrentObject).AriaObjectShelve.TrackingEntry != null && ((AriaObjectShelveMethod)editor.CurrentObject).AriaObjectShelve.TrackingEntry.Status == TrackingEntry.TrackingStatus.Complete)
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
                CurrentAriaObjectShelveMethod = ((AriaObjectShelveMethod)this.View.CurrentObject).Oid;
                if (((AriaObjectShelveMethod)this.View.CurrentObject).AriaObjectShelve != null)
                {
                    CurrentAriaObjectShelve = ((AriaObjectShelveMethod)this.View.CurrentObject).AriaObjectShelve.Oid;
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
                    if (editor.PropertyName == "AriaObjectShelveMethodParameters")
                    {
                        editor.Frame.GetController<ListViewController>().Actions["Edit"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<WebModificationsController>().Actions["SwitchToEditMode"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<NewObjectViewController>().Actions["New"].Enabled.SetItemValue("Disable", false);                      
                        editor.Frame.GetController<LinkUnlinkController>().Actions["Link"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<AriaObjectShelveMethodParameterViewController>().Actions["AriaObjectShelveMethodParameterSelectMethodParametersToModify"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<AriaObjectShelveMethodParameterViewController>().Actions["AriaObjectShelveMethodParameterSelectMethodParametersToDelete"].Enabled.SetItemValue("Disable", false);

                    }
                }
            }
        }
        private void SelectMethodsToDelete_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {
           
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            var AriaObjectShelve = objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveViewController.CurrentAriaObjectShelve);

            foreach (AriaObjectBrowseMethod selectedAriaObjectBrowse in e.PopupWindow.View.SelectedObjects)
            {
                AriaObjectMethod selectedAriaObject = objectSpace.GetObjectByKey<AriaObjectMethod>(Guid.Parse(selectedAriaObjectBrowse.PropertyOid));

                var method = new AriaObjectShelveMethod(AriaObjectShelve.Session);

                method.MethodName = selectedAriaObject.MethodName;
                method.MethodDescription = selectedAriaObject.MethodDescription;
                method.ModificationType = Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveMethod.ModificationTypes.Delete;
                method.AriaObjectShelve = AriaObjectShelve;


                AriaObjectShelve.AriaObjectShelveMethods.Add(method);

                AriaObjectShelve.Save();

                AriaObjectShelve.Session.CommitTransaction();
            }

            ((NestedFrame)Frame).ViewItem.View.ObjectSpace.Refresh();       
        
        }

        private void SelectMethodsToDelete_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            CollectionSource newCollectionSource = new CollectionSource(objectSpace, typeof(AriaObjectBrowseMethod));
            if (AriaObjectShelveViewController.CurrentAriaObject != Guid.Empty)
            {
                List<string> ariaObjectMethods = objectSpace.GetObjectByKey<AriaObject>(AriaObjectShelveViewController.CurrentAriaObject).AriaObjectMethods.Select(r => r.MethodName).Distinct().ToList();

                List<string> ariaObjectShelveMethods = objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveViewController.CurrentAriaObjectShelve).AriaObjectShelveMethods.Select(r => r.MethodName).ToList();

                List<string> methods = ariaObjectMethods.Except(ariaObjectShelveMethods).ToList();
                methods.Sort();

                foreach (var methodName in methods)
                {
                    var objectmethod = objectSpace.CreateObject<AriaObjectBrowseMethod>();

                    var methodObject = objectSpace.GetObjectByKey<AriaObject>(AriaObjectShelveViewController.CurrentAriaObject).AriaObjectMethods.OrderByDescending(r => r.ObjectRevision).FirstOrDefault(r => r.MethodName == methodName);

                    objectmethod.MethodName = methodName;
                    objectmethod.PropertyOid = methodObject.Oid.ToString();
                    objectmethod.MethodDescription = methodObject.MethodDescription;
                    objectmethod.KeyProperty = Guid.NewGuid().ToString();
                    newCollectionSource.Add(objectmethod);
                }
            }
            ListView vwAriaObject = Application.CreateListView(Application.FindListViewId(typeof(AriaObjectBrowseMethod)), newCollectionSource, false);

            e.View = vwAriaObject;

        }

        private void SelectMethodsToModify_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            CollectionSource newCollectionSource = new CollectionSource(objectSpace, typeof(AriaObjectBrowseMethod));
            if (AriaObjectShelveViewController.CurrentAriaObject != Guid.Empty)
            {
                List<string> ariaObjectMethods = objectSpace.GetObjectByKey<AriaObject>(AriaObjectShelveViewController.CurrentAriaObject).AriaObjectMethods.Select(r => r.MethodName).Distinct().ToList();



                List<string> ariaObjectShelveMethods = objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveViewController.CurrentAriaObjectShelve).AriaObjectShelveMethods.Select(r => r.MethodName).ToList();

                List<string> methods = ariaObjectMethods.Except(ariaObjectShelveMethods).ToList();
                methods.Sort();

                foreach (var methodName in methods)
                {
                    var objectmethod = objectSpace.CreateObject<AriaObjectBrowseMethod>();

                    var methodObject = objectSpace.GetObjectByKey<AriaObject>(AriaObjectShelveViewController.CurrentAriaObject).AriaObjectMethods.OrderByDescending(r => r.ObjectRevision).FirstOrDefault(r => r.MethodName == methodName);

                    objectmethod.MethodName = methodName;
                    objectmethod.PropertyOid = methodObject.Oid.ToString();
                    objectmethod.MethodDescription = methodObject.MethodDescription;
                    objectmethod.KeyProperty = Guid.NewGuid().ToString();
                    newCollectionSource.Add(objectmethod);
                }
            }
            ListView vwAriaObject = Application.CreateListView(Application.FindListViewId(typeof(AriaObjectBrowseMethod)), newCollectionSource, false);

            e.View = vwAriaObject;



        }

        private void SelectMethodsToModify_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {

            IObjectSpace objectSpace = Application.CreateObjectSpace();

            var AriaObjectShelve = objectSpace.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveViewController.CurrentAriaObjectShelve);

            foreach (AriaObjectBrowseMethod selectedAriaObjectBrowse in e.PopupWindow.View.SelectedObjects)
            {
                AriaObjectMethod selectedAriaObject = objectSpace.GetObjectByKey<AriaObjectMethod>(Guid.Parse(selectedAriaObjectBrowse.PropertyOid));

                var method = new AriaObjectShelveMethod(AriaObjectShelve.Session);

                method.MethodName = selectedAriaObject.MethodName;
                method.MethodDescription = selectedAriaObject.MethodDescription;
                method.ModificationType = Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveMethod.ModificationTypes.Modify;
                method.AriaObjectShelve = AriaObjectShelve;


                AriaObjectShelve.AriaObjectShelveMethods.Add(method);

                AriaObjectShelve.Save();

                AriaObjectShelve.Session.CommitTransaction();
            }

            ((NestedFrame)Frame).ViewItem.View.ObjectSpace.Refresh();
            
        }

        
    }
}
