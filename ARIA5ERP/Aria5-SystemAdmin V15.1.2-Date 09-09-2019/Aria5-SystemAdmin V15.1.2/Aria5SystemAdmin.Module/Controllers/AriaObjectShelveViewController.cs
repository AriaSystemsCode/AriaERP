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
using Aria5SystemAdmin.Module.Managers;
using DevExpress.ExpressApp.Web.SystemModule;
using DevExpress.ExpressApp.Xpo;

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class AriaObjectShelveViewController : ViewController
    {
        public AriaObjectShelveViewController()
        {
            InitializeComponent();
            RegisterActions(components);
            // Target required Views (via the TargetXXX properties) and create their Actions.
        }
        protected override void OnActivated()
        {
            // Perform various tasks depending on the target View.

            //if (View is DetailView)
            //{
            //    if (this.View.CurrentObject != null)
            //    {
            //        this.View.Model.AllowEdit = ((AriaObjectShelve)this.View.CurrentObject).TrackingEntry.Status != TrackingEntry.TrackingStatus.Complete;
            //        Frame.View.ObjectSpace.Refresh();
            //    }
                
            //}
            base.OnActivated();
            

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


        public static Guid CurrentAriaObjectShelve;
        public static Guid CurrentAriaObject;
        public static AriaObjectShelve CAriaObjectShelve;

        private void AriaObjectShelveController_Activated(object sender, EventArgs e)
        {
            if ((View is DetailView))
            {
                if (((AriaObjectShelve)(View as DetailView).CurrentObject).TrackingEntry != null &&
                ((AriaObjectShelve)(View as DetailView).CurrentObject).TrackingEntry.Status == TrackingEntry.TrackingStatus.Complete)
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

                        if (((AriaObjectShelve)editor.CurrentObject).TrackingEntry != null && ((AriaObjectShelve)editor.CurrentObject).TrackingEntry.Status == TrackingEntry.TrackingStatus.Complete)
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
                    Frame.GetController<WebModificationsController>().Actions["SwitchToEditMode"].Enabled.SetItemValue("Disable", false); Frame.GetController<WebModificationsController>().Actions["SwitchToEditMode"].Enabled.SetItemValue("Disable", true);
                    Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Enabled.SetItemValue("Disable", true);

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
            if (this.View != null && this.View.CurrentObject != null)
            {
                CurrentAriaObjectShelve = ((AriaObjectShelve)this.View.CurrentObject).Oid;
                CAriaObjectShelve = ((AriaObjectShelve)this.View.CurrentObject);
                if (((AriaObjectShelve)this.View.CurrentObject).AriaObject != null)
                {
                    CurrentAriaObject = ((AriaObjectShelve)this.View.CurrentObject).AriaObject.Oid;
                }
                else
                {
                    CurrentAriaObject = Guid.Empty;
                }
            }
        }

        void editor_ControlCreated(object sender, EventArgs e)
        {
            foreach (ListPropertyEditor editor in (View as DetailView).GetItems<ListPropertyEditor>())
            {
                if (editor.Frame != null)
                {
                    if (editor.PropertyName == "AriaObjectShelveProperties")
                    {
                        editor.Frame.GetController<NewObjectViewController>().Actions["New"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<AriaObjectShelvePropertyViewController>().Actions["AriaObjectShelvePropertySelectPropertiesToModify"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<AriaObjectShelvePropertyViewController>().Actions["AriaObjectShelvePropertySelectPropertiesToDelete"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<LinkUnlinkController>().Actions["Link"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<ListViewController>().Actions["Edit"].Enabled.SetItemValue("Disable", false);
                    }

                    else if (editor.PropertyName == "AriaObjectShelveEvents")
                    {
                        editor.Frame.GetController<ListViewController>().Actions["Edit"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<NewObjectViewController>().Actions["New"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<AriaObjectShelveEventViewController>().Actions["AriaObjectShelveEventSelectEventsToModify"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<AriaObjectShelveEventViewController>().Actions["AriaObjectShelveEventSelectEventsToDelete"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<LinkUnlinkController>().Actions["Link"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Enabled.SetItemValue("Disable", false);
                    }

                    else if (editor.PropertyName == "AriaObjectShelveMethods")
                    {
                        editor.Frame.GetController<ListViewController>().Actions["Edit"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<NewObjectViewController>().Actions["New"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<AriaObjectShelveMethodViewController>().Actions["AriaObjectShelveMethodSelectMethodsToModify"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<AriaObjectShelveMethodViewController>().Actions["AriaObjectShelveMethodSelectMethodsToDelete"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<LinkUnlinkController>().Actions["Link"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Enabled.SetItemValue("Disable", false);
                    }
                    else if (editor.PropertyName == "AriaObjectShelveSettings")
                    {
                        editor.Frame.GetController<ListViewController>().Actions["Edit"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<NewObjectViewController>().Actions["New"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<LinkUnlinkController>().Actions["Link"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Enabled.SetItemValue("Disable", false);
                    }
                }
            }
        }

        private void AriaObjectShelveViewController_ViewControlsCreated(object sender, EventArgs e)
        {
            if (this.View != null && this.View.CurrentObject != null)
            {
                CurrentAriaObjectShelve = ((AriaObjectShelve)this.View.CurrentObject).Oid;
                if (((AriaObjectShelve)this.View.CurrentObject).AriaObject != null) CurrentAriaObject = ((AriaObjectShelve)this.View.CurrentObject).AriaObject.Oid;
            }
        }

        //private void CheckOut_Execute(object sender, SimpleActionExecuteEventArgs e)
        //{
        //    //IObjectSpace objectSpace = Application.CreateObjectSpace();

        //    //string filepath = string.Empty;
        //    //string AssignedResource = string.Empty;
        //    //TrackingEntry trackingEntry = objectSpace.GetObjectByKey<TrackingEntry>(TrackingEntryViewController.currentriaTrakingEntry);

        //    //// loop on all shelve files to get the files which have been selected from the list

        //    //foreach (AriaObjectShelve selectedFile in e.SelectedObjects)
        //    //{
        //    //    string FilePath = selectedFile.AriaObject.AriaObjectSettings.Where(r => r.SettingType.Name == "FilePath").Count() <= 0 ? "" :
        //    //                        selectedFile.AriaObject.AriaObjectSettings.First(r => r.SettingType.Name == "FilePath").Value;

        //    //    if (!string.IsNullOrEmpty(FilePath))
        //    //    {


        //    //        TfsManager tfsHandler = new TfsManager();


        //    //        filepath = FilePath;
        //    //        TrackingEntry trackingEntry1 = objectSpace.GetObjectByKey<TrackingEntry>(TrackingEntryViewController.currentriaTrakingEntry);
        //    //        foreach (TrackingTask task in trackingEntry1.TrackingTasks)
        //    //        {
        //    //            if (task.Task.CheckOut == true)
        //    //            {
        //    //                //check for the resources not empty
        //    //                AssignedResource = task.Resources.Name;

        //    //                // tfsHandler.GrantCheckoutFilePermission(filepath, AssignedResource);
        //    //            }
        //    //        }
        //    //        selectedFile.State = "CheckedOut";

        //    //        selectedFile.Save();

        //    //        selectedFile.Session.CommitTransaction();
        //    //    }
        //    //}
        //}

        private void SelectObjectsToModify_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            #region Old code that present an ariaobject browse 
            //IObjectSpace objectSpace = Application.CreateObjectSpace();

            //TrackingEntry trackingEntry = objectSpace.GetObjectByKey<TrackingEntry>(TrackingEntryViewController.currentriaTrakingEntry);

            //CollectionSource newCollectionSource = new CollectionSource(objectSpace, typeof(AriaObjectBrowse));
            //bool Exist = false;
            //if (trackingEntry.Entity != null)
            //{
            //    var myList = objectSpace.GetObjectByKey<AriaObject>(trackingEntry.Entity.Oid).GetChildren.ToList();
            //    foreach (AriaObject objectchild in objectSpace.GetObjectByKey<AriaObject>(trackingEntry.Entity.Oid).GetChildren.ToList())
            //    {
            //        if (objectchild.GetChildren.ToList().Count > 0)
            //        {
            //            foreach (AriaObject child in objectchild.GetChildren)
            //            {
            //                myList.Add(child);
            //            }
            //        }
            //    }

            //    myList.Add(trackingEntry.Entity);

          
            //    foreach (var relatedObject in myList)
            //    {

            //        var ariaObject = objectSpace.CreateObject<AriaObjectBrowse>();

            //        ariaObject.ObjectName = relatedObject.ObjectName;
            //        ariaObject.ObjectID = (int)relatedObject.ObjectID;
            //        ariaObject.ObjectDescription = relatedObject.ObjectDescription;
            //        ariaObject.ActiveRevision = relatedObject.ActiveRevision;
            //        ariaObject.Key = Guid.NewGuid().ToString();
            //        ariaObject.ObjectOid = relatedObject.Oid.ToString();

            //        foreach (AriaObjectShelve shelveObject in trackingEntry.AriaObjectShelves)
            //        {
            //            if (shelveObject.AriaObject != null && shelveObject.AriaObject.Oid == relatedObject.Oid)
            //            {
            //                Exist = true;
            //            }

            //        }

            //        if (Exist == false)
            //        {
            //            newCollectionSource.Add(ariaObject);
            //        }
            //        Exist = false;
            //    }
            //}
            //else if (trackingEntry.ModificationType == TrackingEntry.ModificationTypes.Add)
            //{
            //    var ariaObject = objectSpace.CreateObject<AriaObjectBrowse>();

            //    ariaObject.ObjectName = trackingEntry.ObjectName;

            //    ariaObject.Key = Guid.NewGuid().ToString();
            //    ariaObject.ObjectOid = trackingEntry.Oid.ToString();

            //    newCollectionSource.Add(ariaObject);
            //}



            //ListView vwAriaObject = Application.CreateListView(Application.FindListViewId(typeof(AriaObjectBrowse)),
            //newCollectionSource, false);

            //e.View = vwAriaObject;
            #endregion

            //ATA add the list of ariaobject to choose on of them 
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            TrackingEntry trackingEntry = objectSpace.GetObjectByKey<TrackingEntry>(TrackingEntryViewController.currentriaTrakingEntry);
            CollectionSource newCollectionSource = new CollectionSource(objectSpace, typeof(AriaObject));
            bool Exist = false;
            if (trackingEntry.Entity != null)
            {
                newCollectionSource.SetCriteria("entitiy relatedobject", "[ParentObjectID]= '" + trackingEntry.Entity.Oid + "' Or [ParentObjectID].[ParentObjectID] = '" + trackingEntry.Entity.Oid + "'");

               // var myList = objectSpace.GetObjectByKey<AriaObject>(trackingEntry.Entity.Oid).GetChildren.ToList();
                //foreach (AriaObject objectchild in objectSpace.GetObjectByKey<AriaObject>(trackingEntry.Entity.Oid).GetChildren.ToList())
                //{
                //    if (objectchild.GetChildren.ToList().Count > 0)
                //    {
                        
                //        newCollectionSource.Add(objectchild);
                //        foreach (AriaObject child in objectchild.GetChildren)
                //        {
                //            newCollectionSource.Add(child);
                //          //  myList.Add(child);
                //        }
                //    }
                //}
                ListView vwAriaObject = Application.CreateListView("AriaObject_LookupListView", newCollectionSource, false);
                vwAriaObject.AllowNew["onlyviewlistview"] = false;
                e.View = vwAriaObject;

                //myList.Add(trackingEntry.Entity);


                //foreach (var relatedObject in myList)
                //{
                //    foreach (AriaObjectShelve shelveObject in trackingEntry.AriaObjectShelves)
                //    {
                //        if (shelveObject.AriaObject != null && shelveObject.AriaObject.Oid == relatedObject.Oid)
                //        {
                //            Exist = true;
                //        }

                //    }

                //    //if (Exist == false)
                //    //{
                //    //    mylist.Add(ariaObject);
                //    //}
                //    Exist = false;
                //}
            }
            //else if (trackingEntry.ModificationType == TrackingEntry.ModificationTypes.Add)
            //{
            //    var ariaObject = objectSpace.CreateObject<>();

            //    ariaObject.ObjectName = trackingEntry.ObjectName;

            //    ariaObject.Key = Guid.NewGuid().ToString();
            //    ariaObject.ObjectOid = trackingEntry.Oid.ToString();

            //    newCollectionSource.Add(ariaObject);
            //}




        }

        private void SelectObjectsToModify_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();
            TrackingEntry trackingEntry = objectSpace.GetObjectByKey<TrackingEntry>(TrackingEntryViewController.currentriaTrakingEntry);
            foreach (AriaObject selectAriaObject in e.PopupWindow.View.SelectedObjects)
            {
                AriaObject selectedAriaObject = objectSpace.GetObjectByKey<AriaObject>(selectAriaObject.Oid);
                //ATA  exclude Ariaobjectshelves that related to another open tracking entry 
                IList<AriaObjectShelve> ExistsShelve = objectSpace.GetObjects<AriaObjectShelve>(CriteriaOperator.Parse("[AriaObject] ='" + selectedAriaObject.Oid + "'"));

                int indexofte = ExistsShelve.FindIndex(x => x.TrackingEntry != null && (x.TrackingEntry.Status != TrackingEntry.TrackingStatus.New && x.TrackingEntry.Status != TrackingEntry.TrackingStatus.Complete && x.TrackingEntry.Status != TrackingEntry.TrackingStatus.Cancelled));
                if (indexofte >= 0)
                {
                    throw new Exception("The '" + selectedAriaObject.ObjectName + "' is already linked to a Traking Entry Number'" + ExistsShelve[indexofte].TrackingEntry.ID + "' and it's status is " + ExistsShelve[indexofte].TrackingEntry.Status + "' So you can't include in your tracking Entry");
                }
                //ATA  exclude Ariaobjectshelves that related to another open tracking entry 
                if (selectedAriaObject != null)
                {
                    AriaObjectShelve newObjectShelve = new AriaObjectShelve(selectedAriaObject.Session);
                    var objectExist = newObjectShelve.Session.FindObject<AriaObjectShelve>(CriteriaOperator.Parse("[TrackingEntry] ='" + TrackingEntryViewController.currentriaTrakingEntry + "' AND [AriaObject] ='" + selectedAriaObject.Oid + "'"));

                    if (objectExist == null)
                    {
                        newObjectShelve.AriaObject = newObjectShelve.Session.FindObject<AriaObject>(CriteriaOperator.Parse("Oid ='" + selectedAriaObject.Oid + "'"));
                        newObjectShelve.ObjectDescription = selectedAriaObject.ObjectDescription;
                        newObjectShelve.ObjectID = (int)selectedAriaObject.ObjectID;
                        newObjectShelve.ObjectName = selectedAriaObject.ObjectName;
                        newObjectShelve.ModificationType = AriaObjectShelve.ModificationTypes.Modify;
                        newObjectShelve.ObjectType = newObjectShelve.Session.FindObject<ObjectType>(CriteriaOperator.Parse("Oid ='" + selectedAriaObject.ObjectType.Oid + "'"));
                        newObjectShelve.TrackingEntry = newObjectShelve.Session.GetObjectByKey<TrackingEntry>(TrackingEntryViewController.currentriaTrakingEntry);
                        //newObjectShelve.
                        trackingEntry.AriaObjectShelves.Add(newObjectShelve);
                    }
                }
                //ATA commented that is no need 
                //else
                //{
                //    AriaObjectShelve newObjectShelve = new AriaObjectShelve(trackingEntry.Session);


                //    var objectExist = newObjectShelve.Session.FindObject<AriaObjectShelve>(CriteriaOperator.Parse("[TrackingEntry] ='" + TrackingEntryViewController.currentriaTrakingEntry + "' AND [ObjectType.Name] ='Entity'"));

                //    if (objectExist == null)
                //    {
                //       // newObjectShelve.AriaObject = newObjectShelve.Session.FindObject<AriaObject>(CriteriaOperator.Parse("Oid ='" + selectedAriaObject.Oid + "'"));
                //        newObjectShelve.ObjectDescription = selectedAriaObject.ObjectDescription;
                //        newObjectShelve.ObjectID = selectAriaObject.Oid;
                //        newObjectShelve.ObjectName = selectedAriaObject.ObjectName;
                //        newObjectShelve.ModificationType = AriaObjectShelve.ModificationTypes.Modify;
                //        newObjectShelve.ObjectType = newObjectShelve.Session.FindObject<ObjectType>(CriteriaOperator.Parse("Name ='Entity'"));
                //        newObjectShelve.TrackingEntry = newObjectShelve.Session.GetObjectByKey<TrackingEntry>(TrackingEntryViewController.currentriaTrakingEntry);

                //        trackingEntry.AriaObjectShelves.Add(newObjectShelve);
                //    }
                //}

            }

            trackingEntry.Save();
            trackingEntry.Session.CommitTransaction();

            ((NestedFrame)Frame).ViewItem.View.ObjectSpace.Refresh();
        }

        private void SelectObjectsToDelete_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            TrackingEntry trackingEntry = objectSpace.GetObjectByKey<TrackingEntry>(TrackingEntryViewController.currentriaTrakingEntry);

            CollectionSource newCollectionSource = new CollectionSource(objectSpace, typeof(AriaObjectBrowse));
            bool Exist = false;
            if (trackingEntry.Entity != null)
            {
                var myList = objectSpace.GetObjectByKey<AriaObject>(trackingEntry.Entity.Oid).GetChildren.ToList();

                myList.Add(trackingEntry.Entity);

                if (trackingEntry.ModificationType == TrackingEntry.ModificationTypes.Add)
                {
                    var ariaObject = objectSpace.CreateObject<AriaObjectBrowse>();

                    ariaObject.ObjectName = trackingEntry.ObjectName;

                    ariaObject.Key = Guid.NewGuid().ToString();
                    ariaObject.ObjectOid = trackingEntry.Oid.ToString();

                    newCollectionSource.Add(ariaObject);
                }

                foreach (var relatedObject in myList)
                {

                    var ariaObject = objectSpace.CreateObject<AriaObjectBrowse>();

                    ariaObject.ObjectName = relatedObject.ObjectName;
                    ariaObject.ObjectID = (int)relatedObject.ObjectID;
                    ariaObject.ObjectDescription = relatedObject.ObjectDescription;
                    ariaObject.ActiveRevision = relatedObject.ActiveRevision;
                    ariaObject.Key = Guid.NewGuid().ToString();
                    ariaObject.ObjectOid = relatedObject.Oid.ToString();

                    foreach (AriaObjectShelve shelveObject in trackingEntry.AriaObjectShelves)
                    {
                        if (shelveObject.AriaObject != null && shelveObject.AriaObject.Oid == relatedObject.Oid)
                        {
                            Exist = true;
                        }

                    }

                    if (Exist == false)
                    {
                        newCollectionSource.Add(ariaObject);
                    }
                    Exist = false;
                }
            }

            ListView vwAriaObject = Application.CreateListView(Application.FindListViewId(typeof(AriaObjectBrowse)),
            newCollectionSource, false);

            e.View = vwAriaObject;
        }

        private void SelectObjectsToDelete_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {

           var session=  ((XPObjectSpace)this.ObjectSpace).Session;




            TrackingEntry trackingEntry = session.GetObjectByKey<TrackingEntry>(TrackingEntryViewController.currentriaTrakingEntry);

            foreach (AriaObjectBrowse selectedAriaObjectBrowse in e.PopupWindow.View.SelectedObjects)
            {
                AriaObject selectedAriaObject = session.GetObjectByKey<AriaObject>(Guid.Parse(selectedAriaObjectBrowse.ObjectOid));
                if (selectedAriaObject != null)
                {
                    AriaObjectShelve newObjectShelve = new AriaObjectShelve(selectedAriaObject.Session);


                    var objectExist = session.FindObject<AriaObjectShelve>(CriteriaOperator.Parse("[TrackingEntry] ='" + TrackingEntryViewController.currentriaTrakingEntry + "' AND [AriaObject] ='" + selectedAriaObject.Oid + "'"));

                    if (objectExist == null)
                    {
                        newObjectShelve.AriaObject = newObjectShelve.Session.FindObject<AriaObject>(CriteriaOperator.Parse("Oid ='" + selectedAriaObject.Oid + "'"));
                        newObjectShelve.ObjectDescription = selectedAriaObject.ObjectDescription;
                        newObjectShelve.ObjectID = (int)selectedAriaObject.ObjectID;
                        newObjectShelve.ObjectName = selectedAriaObject.ObjectName;
                        newObjectShelve.ModificationType = AriaObjectShelve.ModificationTypes.Delete;
                        newObjectShelve.ObjectType = newObjectShelve.Session.FindObject<ObjectType>(CriteriaOperator.Parse("Oid ='" + selectedAriaObject.ObjectType.Oid + "'"));
                        newObjectShelve.TrackingEntry = session.GetObjectByKey<TrackingEntry>(TrackingEntryViewController.currentriaTrakingEntry);

                        trackingEntry.AriaObjectShelves.Add(newObjectShelve);
                    }
                }
                else
                {
                    AriaObjectShelve newObjectShelve = new AriaObjectShelve(trackingEntry.Session);


                    var objectExist = session.FindObject<AriaObjectShelve>(CriteriaOperator.Parse("[TrackingEntry] ='" + TrackingEntryViewController.currentriaTrakingEntry + "'"));

                    if (objectExist == null)
                    {
                        // newObjectShelve.AriaObject = newObjectShelve.Session.FindObject<AriaObject>(CriteriaOperator.Parse("Oid ='" + selectedAriaObject.Oid + "'"));
                        newObjectShelve.ObjectDescription = selectedAriaObjectBrowse.ObjectDescription;
                        newObjectShelve.ObjectID = selectedAriaObjectBrowse.ObjectID;
                        newObjectShelve.ObjectName = selectedAriaObjectBrowse.ObjectName;
                        newObjectShelve.ModificationType = AriaObjectShelve.ModificationTypes.Delete;
                        newObjectShelve.ObjectType = newObjectShelve.Session.FindObject<ObjectType>(CriteriaOperator.Parse("Name ='Entity'"));
                        newObjectShelve.TrackingEntry = session.GetObjectByKey<TrackingEntry>(TrackingEntryViewController.currentriaTrakingEntry);

                        trackingEntry.AriaObjectShelves.Add(newObjectShelve);
                    }
                }

            }

            trackingEntry.Save();
            session.CommitTransaction();

            ((NestedFrame)Frame).ViewItem.View.ObjectSpace.Refresh();
        }

       
    }
}
