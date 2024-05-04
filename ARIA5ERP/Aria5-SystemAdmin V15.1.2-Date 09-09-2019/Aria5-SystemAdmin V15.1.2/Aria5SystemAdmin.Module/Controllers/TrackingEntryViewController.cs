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
using DevExpress.Xpo;
using Aria5SystemAdmin.Module.Managers;
using DevExpress.ExpressApp.Web.SystemModule;
using DevExpress.ExpressApp.Xpo;
using DevExpress.ExpressApp.Security;
using System.IO;
using ArrayToMemo;
using AriaDevExpress.Module.BusinessObjects.SysFiles;
using System.Data;
using System.Data.OleDb;
using Aria5SystemAdmin.Module.ServiceReferenceTfsManager;
//using Aria5SystemAdmin.Module.ServiceReferenceTfsManager;

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class TrackingEntryViewController : ViewController
    {

        public static Guid currentriaTrakingEntry;
        private Guid AiraObjectOid;
        public static TrackingEntry currentTrakingEntryObject;

        public TrackingEntryViewController()
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


        private void TrackingEntryViewController_Activated(object sender, EventArgs e)
        {
            if ((View is DetailView))
            {
                //if (((TrackingEntry)(View as DetailView).CurrentObject) != null &&
                //((TrackingEntry)(View as DetailView).CurrentObject).ModificationType == TrackingEntry.ModificationTypes.Delete)
                //{

                //    foreach (ListPropertyEditor editor in (View as DetailView).GetItems<ListPropertyEditor>())
                //    {

                //        if (((TrackingEntry)editor.CurrentObject) != null &&
                //            ((TrackingEntry)editor.CurrentObject).ModificationType == TrackingEntry.ModificationTypes.Delete)
                //        {
                //            editor.ControlCreated += editor_ControlCreated;
                //        }
                //    }
                //}

                if (((TrackingEntry)(View as DetailView).CurrentObject) != null &&
                (((TrackingEntry)(View as DetailView).CurrentObject).Status == TrackingEntry.TrackingStatus.Complete) || ((TrackingEntry)(View as DetailView).CurrentObject).ApplicationBuild != null)
                {
                    Frame.GetController<TrackingEntryViewController>().Actions["CompleteController"].Enabled.SetItemValue("Disable", false);
                    Frame.GetController<TrackingEntryViewController>().Actions["CancelAction"].Enabled.SetItemValue("Disable", false);

                    foreach (ListPropertyEditor editor in (View as DetailView).GetItems<ListPropertyEditor>())
                    {

                        if (((TrackingEntry)editor.CurrentObject) != null &&
                            ((TrackingEntry)editor.CurrentObject).Status == TrackingEntry.TrackingStatus.Complete)
                        {
                            editor.ControlCreated += editor_ControlCreated1;
                        }
                    }
                }
                else
                {
                    Frame.GetController<TrackingEntryViewController>().Actions["CompleteController"].Enabled.SetItemValue("Disable", true);
                    Frame.GetController<TrackingEntryViewController>().Actions["CancelAction"].Enabled.SetItemValue("Disable", true);


                }
            }
            else
            {
                Frame.GetController<TrackingEntryViewController>().Actions["CompleteController"].Enabled.SetItemValue("Disable", true);
                Frame.GetController<TrackingEntryViewController>().Actions["CancelAction"].Enabled.SetItemValue("Disable", true);
                Frame.GetController<ModificationsController>().Actions["Save"].Enabled.SetItemValue("Disable", true);
                Frame.GetController<ModificationsController>().Actions["SaveAndClose"].Enabled.SetItemValue("Disable", true);
                Frame.GetController<ModificationsController>().Actions["SaveAndNew"].Enabled.SetItemValue("Disable", true);
                Frame.GetController<NewObjectViewController>().Actions["New"].Enabled.SetItemValue("Disable", true);
                Frame.GetController<LinkUnlinkController>().Actions["Link"].Enabled.SetItemValue("Disable", true);
                Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", true);
                Frame.GetController<WebModificationsController>().Actions["SwitchToEditMode"].Enabled.SetItemValue("Disable", true);
                Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Enabled.SetItemValue("Disable", true);
            }
            if (this.View.CurrentObject != null && this.View.CurrentObject.GetType() == typeof(TrackingEntry))
            {
                currentriaTrakingEntry = ((TrackingEntry)this.View.CurrentObject).Oid;
            }

            Frame.View.Refresh();
        }


        void editor_ControlCreated(object sender, EventArgs e)
        {
            foreach (ListPropertyEditor editor in (View as DetailView).GetItems<ListPropertyEditor>())
            {
                if (editor.Frame != null)
                {
                    if (editor.PropertyName == "AriaObjectShelves")
                    {
                        editor.Frame.GetController<NewObjectViewController>().Actions["New"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<AriaObjectShelveViewController>().Actions["SelectObjectsToModify"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<AriaObjectShelveViewController>().Actions["AriaObjectShelveSelectObjectsToDelete"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<AriaObjectShelveViewController>().Actions["CheckOut"].Enabled.SetItemValue("Disable", false);
                        Frame.GetController<WebModificationsController>().Actions["SwitchToEditMode"].Enabled.SetItemValue("Disable", true);

                        editor.Frame.GetController<LinkUnlinkController>().Actions["Link"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<ListViewController>().Actions["Edit"].Enabled.SetItemValue("Disable", false);
                    }

                    if (editor.PropertyName == "TrackingTasks")
                    {
                        editor.Frame.GetController<NewObjectViewController>().Actions["New"].Enabled.SetItemValue("Disable", false);
                        Frame.GetController<WebModificationsController>().Actions["SwitchToEditMode"].Enabled.SetItemValue("Disable", true);

                        editor.Frame.GetController<LinkUnlinkController>().Actions["Link"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<ListViewController>().Actions["Edit"].Enabled.SetItemValue("Disable", false);
                    }

                }
            }
        }
        void editor_ControlCreated1(object sender, EventArgs e)
        {
            foreach (ListPropertyEditor editor in (View as DetailView).GetItems<ListPropertyEditor>())
            {
                if (editor.Frame != null)
                {
                    if (editor.PropertyName == "TrackingTasks")
                    {
                        editor.Frame.GetController<NewObjectViewController>().Actions["New"].Enabled.SetItemValue("Disable", false);
                        Frame.GetController<WebModificationsController>().Actions["SwitchToEditMode"].Enabled.SetItemValue("Disable", true);

                        editor.Frame.GetController<LinkUnlinkController>().Actions["Link"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<LinkUnlinkController>().Actions["Unlink"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<DeleteObjectsViewController>().Actions["Delete"].Enabled.SetItemValue("Disable", false);
                        editor.Frame.GetController<ListViewController>().Actions["Edit"].Enabled.SetItemValue("Disable", false);
                    }

                }
            }
        }

        private void TrackingEntryViewController_ViewControlsCreated(object sender, EventArgs e)
        {
            if (this.View.CurrentObject != null && this.View.CurrentObject.GetType() == typeof(TrackingEntry))
            {
                currentriaTrakingEntry = ((TrackingEntry)this.View.CurrentObject).Oid;
                currentTrakingEntryObject = ((TrackingEntry)this.View.CurrentObject);
            }
        }

        #region complete controller

        public void AddNewProperties(AriaObjectShelve objectShelve)
        {
            AriaObject ariaObject = objectShelve.AriaObject;
            //Add Properties
            foreach (var shelveProperty in objectShelve.AriaObjectShelveProperties)
            {
                var prop = new AriaObjectProperty(objectShelve.Session);

                prop.PropertyName = shelveProperty.PropertyName;
                prop.PropertyDescription = shelveProperty.PropertyDescription;
                if (shelveProperty.PropertyType != null) prop.PropertyType = objectShelve.Session.GetObjectByKey<PropertyType>(shelveProperty.PropertyType.Oid);
                prop.Required = shelveProperty.Required;
                prop.ModificationType = ((AriaObjectProperty.modificationType)shelveProperty.ModificationType);

                //Add settings of the Property
                foreach (var shelvePropSetting in shelveProperty.AriaObjectShelvePropertySettings)
                {
                    var PropSetting = new AriaObjectPropertySetting(objectShelve.Session);

                    PropSetting.DataType = shelvePropSetting.DataType;
                    PropSetting.DecimalPlaces = shelvePropSetting.DecimalPlaces;
                    PropSetting.Modified = shelvePropSetting.Modified;
                    if (shelvePropSetting.SettingType != null) PropSetting.SettingType = objectShelve.Session.GetObjectByKey<SettingType>(shelvePropSetting.SettingType.Oid);
                    PropSetting.Value = shelvePropSetting.Value;
                    PropSetting.Width = shelvePropSetting.Width;

                    prop.AriaObjectPropertiesSettings.Add(PropSetting);
                }

                ariaObject.AriaObjectProperties.Add(prop);
            }

            ariaObject.Save();
            //mmt
            // ariaObject.Session.CommitTransaction();
            //MMT

        }

        public void AddSetSettings(AriaObjectShelve objectShelve)
        {
            AriaObject ariaObject = objectShelve.AriaObject;

            if (ariaObject == null && objectShelve.ModificationType == AriaObjectShelve.ModificationTypes.Add)
            {
                ariaObject = new AriaObject(objectShelve.Session);

                var rev = new AriaObjectRevision(objectShelve.Session);



                foreach (var settingType in objectShelve.ObjectType.SettingTypes)
                {
                    AriaObjectShelveSetting setting = new AriaObjectShelveSetting(objectShelve.Session);

                    setting.SettingType = settingType;
                    setting.DataType = settingType.DataType.ToString();
                    setting.DecimalPlaces = settingType.Decimal;
                    setting.Width = settingType.Width;

                    if (rev != null)
                    {

                        var objSettings = rev.AriaObjectSettings.Where(r => r.SettingType.Oid == settingType.Oid);

                        if (objSettings.Count() > 0)
                        {
                            setting.Value = objSettings.First().Value;
                        }
                    }
                }




                ariaObject.ObjectName = objectShelve.ObjectName;

                ariaObject.ActiveRevision = rev.ObjectRevision;

                ariaObject.ObjectDescription = objectShelve.ObjectDescription;
                ariaObject.ObjectID = objectShelve.ObjectID;

                ariaObject.ObjectType = objectShelve.ObjectType;
                if (ariaObject.ParentObjectID != null) ariaObject.ParentObjectID = objectShelve.ParentObject;
                if (ariaObject.ParentObjectID != null && objectShelve.ParentObject.ParentObjectType != null) ariaObject.ParentObjectType = objectShelve.ParentObject.ParentObjectType;
                if (((TrackingEntry)this.View.CurrentObject).Application != null) ariaObject.Applications.Add(objectShelve.Session.GetObjectByKey<TrackingEntry>(currentriaTrakingEntry).Application);


                // ariaObject.ObjectID
                ariaObject.Save();
                ariaObject.Session.CommitTransaction();
                objectShelve.AriaObject = ariaObject;
                objectShelve.AriaObject.AriaObjectRevisions.Add(rev);
                objectShelve.Save();
            }



            if (objectShelve.AriaObject.AriaObjectRevisions.Where(r => string.IsNullOrWhiteSpace(r.ObjectRevision)).Count() > 0)
            {
                var oblRev = objectShelve.AriaObject.AriaObjectRevisions.First(r => string.IsNullOrWhiteSpace(r.ObjectRevision));

                foreach (var objectShelveSetting in objectShelve.AriaObjectShelveSettings)
                {
                    if (oblRev.AriaObjectSettings.Where(r => r.SettingType.Oid == objectShelveSetting.SettingType.Oid).Count() > 0)
                    {
                        var objectSetting = oblRev.AriaObjectSettings.Where(r => r.SettingType.Oid == objectShelveSetting.SettingType.Oid).First();
                        objectSetting.Value = objectShelveSetting.Value;
                        objectSetting.Save();
                        //MMT
                        //objectSetting.Session.CommitTransaction();
                        //MMT
                    }
                    else
                    {
                        AriaObjectSetting setting = new AriaObjectSetting(objectShelve.Session);

                        if (objectShelveSetting.SettingType != null) setting.SettingType = objectShelve.Session.GetObjectByKey<SettingType>(objectShelveSetting.SettingType.Oid);
                        setting.DataType = objectShelveSetting.DataType;
                        setting.DecimalPlaces = objectShelveSetting.DecimalPlaces;
                        setting.Value = objectShelveSetting.Value;

                        oblRev.AriaObjectSettings.Add(setting);
                        //MMT
                        // setting.Session.CommitTransaction();
                        //MMT
                    }
                }
            }
        }


        private void Complete_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            TrackingEntry trackingEntry = objectSpace.GetObjectByKey<TrackingEntry>(currentriaTrakingEntry);

            #region Check Child Entites
            bool notCompleteFlag = false;
            if (trackingEntry.ChildTrackingEntries.Count > 0)
            {
                foreach (var childTrackingEntry in trackingEntry.ChildTrackingEntries)
                {

                    if (childTrackingEntry.Status != TrackingEntry.TrackingStatus.Complete)
                    {
                        notCompleteFlag = true;

                        throw new Exception("Cannot complete tracking entry before complete children tracking entries.");
                    }
                    else
                    {
                        notCompleteFlag = false;
                    }
                }
            }
            #endregion


            if (notCompleteFlag == false)
            {
                #region TrackingEntry Entity New
                switch (((TrackingEntry)this.View.CurrentObject).ModificationType)
                {
                    case TrackingEntry.ModificationTypes.Add:
                        AiraObjectOid = AddNewObject(trackingEntry);

                        foreach (AriaObjectShelve item in trackingEntry.AriaObjectShelves)
                        {
                            if (((TrackingEntry)this.View.CurrentObject).ModificationType == TrackingEntry.ModificationTypes.Add &&
                               item.ModificationType == AriaObjectShelve.ModificationTypes.Modify)
                            {

                                // ((XPObjectSpace)this.ObjectSpace).Session.Connection.State == System.Data.ConnectionState.

                                item.AriaObject = objectSpace.GetObjectByKey<AriaObject>(AiraObjectOid);

                                AddSetSettings(item);
                                AddNewProperties(item);
                                AddNewMethods(item);
                                AddNewEvents(item);
                            }
                        }


                        trackingEntry.Entity.AssignNewRevision(trackingEntry);

                        break;

                    //case TrackingEntry.ModificationTypes.Delete:
                    //    AriaObject ariaObject = trackingEntry.Session.FindObject<AriaObject>(CriteriaOperator.Parse("Oid = '" + trackingEntry.Entity.Oid + "'"));
                    //    ariaObject.Applications.Remove(trackingEntry.Application);
                    //    ariaObject.Save();
                    //    trackingEntry.Application = null;
                    //    break;

                    case TrackingEntry.ModificationTypes.Modify:
                        ((TrackingEntry)this.View.CurrentObject).Entity.AssignNewRevision(trackingEntry);
                        break;
                }
                #endregion


                foreach (AriaObjectShelve item in trackingEntry.AriaObjectShelves)
                {
                    switch (((TrackingEntry)this.View.CurrentObject).ModificationType)
                    {
                        case TrackingEntry.ModificationTypes.Add:

                            switch (item.ModificationType)
                            {
                                case AriaObjectShelve.ModificationTypes.Add:
                                    AddNewAttachment(item);
                                    AddSetSettings(item);
                                    AddNewProperties(item);
                                    AddNewMethods(item);
                                    AddNewEvents(item);

                                    item.AriaObject.AssignNewRevision(trackingEntry);
                                    item.Save();
                                    item.Session.CommitTransaction();
                                    break;

                                case AriaObjectShelve.ModificationTypes.Delete:
                                    // Doesn't occurs
                                    break;

                                case AriaObjectShelve.ModificationTypes.Modify:
                                    item.AriaObject = item.Session.GetObjectByKey<AriaObject>(AiraObjectOid);
                                    item.Save();
                                    break;
                            }

                            break;

                        //case TrackingEntry.ModificationTypes.Delete:
                        //    switch (item.ModificationType)
                        //    {
                        //        case AriaObjectShelve.ModificationTypes.Add:
                        //            // Doesn't occurs
                        //            break;

                        //        case AriaObjectShelve.ModificationTypes.Delete:
                        //            // Doesn't occurs
                        //            break;

                        //        case AriaObjectShelve.ModificationTypes.Modify:
                        //            // Doesn't occurs
                        //            break;
                        //    }

                        //    break;

                        case TrackingEntry.ModificationTypes.Modify:
                            switch (item.ModificationType)
                            {
                                case AriaObjectShelve.ModificationTypes.Add:
                                    AddNewAttachment(item);
                                    AddSetSettings(item);
                                    AddNewProperties(item);
                                    AddNewMethods(item);
                                    AddNewEvents(item);

                                    item.AriaObject.AssignNewRevision(trackingEntry);
                                    item.Save();
                                    item.Session.CommitTransaction();
                                    break;

                                case AriaObjectShelve.ModificationTypes.Delete:
                                    item.Session.GetObjectByKey<AriaObject>(item.AriaObject.Oid).ParentObjectID = null;
                                    break;

                                case AriaObjectShelve.ModificationTypes.Modify:
                                    AddSetSettings(item);
                                    AddNewProperties(item);
                                    AddNewMethods(item);
                                    AddNewEvents(item);

                                    item.AriaObject.AssignNewRevision(trackingEntry);
                                    item.Save();
                                    item.Session.CommitTransaction();
                                    break;
                            }

                            break;
                    }
                }

                // CheckIn
                string filepath = string.Empty;
                string AssignedResource = string.Empty;

                TfsManager tfsHandler = new TfsManager();

                AssignedResource = "Khaled Mayhoub";

                // loop on all shelve files to get the files which have been selected from the list

                foreach (AriaObjectShelve selectedFile in trackingEntry.AriaObjectShelves)
                {
                    //"$/New Test/BricksStyle/SearchResultsPage.XAML";


                    string FilePath = selectedFile.AriaObject.AriaObjectSettings.Where(r => r.SettingType.SettingTypeId == "FilePath").Count() <= 0 ? "" :
                                    selectedFile.AriaObject.AriaObjectSettings.First(r => r.SettingType.SettingTypeId == "FilePath").Value;



                    if (!string.IsNullOrEmpty(FilePath))
                    {
                        //  int changSet = tfsHandler.CommentCheckin(FilePath, AssignedResource, "[" + AssignedResource + "]" + " Tracking #" + trackingEntry.ID + " [" + trackingEntry.RequestedDate.ToShortDateString() + "] " + selectedFile.Description);

                        selectedFile.State = "CheckedIn";

                        //selectedFile.ChangeSet = changSet.ToString();

                        selectedFile.Save();

                        trackingEntry.Session.CommitTransaction();

                        // tfsHandler.DenyCheckoutFilePermission(FilePath, AssignedResource);


                    }
                }

                Frame.View.ObjectSpace.Refresh();

                //MMT
                CommentCheckIn_Execute(sender, e);
                //MMT

                trackingEntry.Status = TrackingEntry.TrackingStatus.Complete;
                trackingEntry.CompleteDate = DateTime.Today.Date;

                trackingEntry.Save();
                trackingEntry.Session.CommitTransaction();


                Frame.View.ObjectSpace.Refresh();
            }


        }

        public Guid AddNewObject(TrackingEntry trackingEntry)
        {

            AriaObject ariaObject = new AriaObject(trackingEntry.Session);
            ariaObject.ObjectName = ((TrackingEntry)this.View.CurrentObject).ObjectName;

            if (trackingEntry.Entity != null)
            {
                ariaObject.ObjectDescription = trackingEntry.Entity.ObjectDescription;
                ariaObject.ObjectID = trackingEntry.Entity.ObjectID;
            }

            ariaObject.Save();
            ariaObject.ObjectType = trackingEntry.Session.FindObject<ObjectType>(CriteriaOperator.Parse("[ObjectTypeID] = 'Entity'"));

            if (((TrackingEntry)this.View.CurrentObject).Application != null)
            {

                var app = trackingEntry.Session.GetObjectByKey<TrackingEntry>(currentriaTrakingEntry).Application;
                ariaObject.Applications.Add(app);
            }

            trackingEntry.Entity = ariaObject;
            trackingEntry.Save();
            //mmt
            //trackingEntry.Session.CommitTransaction();
            //mmt
            Frame.View.ObjectSpace.Refresh();

            return ariaObject.Oid;

        }

        public Guid AddNewAttachment(AriaObjectShelve shelve)
        {
            AriaObject ariaObject = new AriaObject(shelve.Session);

            ariaObject.ObjectName = shelve.ObjectName;
            ariaObject.ObjectDescription = shelve.ObjectDescription;
            ariaObject.ObjectID = shelve.ObjectID;

            ariaObject.Save();

            ariaObject.ObjectType = ariaObject.Session.GetObjectByKey<ObjectType>(shelve.ObjectType.Oid);
            if (((TrackingEntry)this.View.CurrentObject).Application != null)
            {
                var app = shelve.Session.GetObjectByKey<TrackingEntry>(currentriaTrakingEntry).Application;
                ariaObject.Applications.Add(app);
            }
            shelve.AriaObject = ariaObject;
            ariaObject.ParentObjectID = shelve.Session.GetObjectByKey<TrackingEntry>(currentriaTrakingEntry).Entity;
            ariaObject.Session.CommitTransaction();

            Frame.View.ObjectSpace.Refresh();

            return ariaObject.Oid;

        }

        public void AddNewMethods(AriaObjectShelve objectShelve)
        {
            AriaObject ariaObject = objectShelve.AriaObject;

            //Add Methods
            foreach (var shelveMethod in objectShelve.AriaObjectShelveMethods)
            {
                var method = new AriaObjectMethod(objectShelve.Session);

                method.MethodName = shelveMethod.MethodName;
                method.MethodDescription = shelveMethod.MethodDescription;
                method.ModificationType = ((AriaObjectMethod.modificationType)shelveMethod.ModificationType);
                method.AriaObject = objectShelve.Session.FindObject<AriaObject>(CriteriaOperator.Parse("Oid ='" + ariaObject.Oid + "'"));
                //Add Paramerer of Method
                foreach (var shelveMethodParam in shelveMethod.AriaObjectShelveMethodParameters)
                {
                    var Param = new AriaObjectMethodParameter(objectShelve.Session);

                    Param.ParameterName = shelveMethodParam.ParameterName;
                    Param.ParameterNo = shelveMethodParam.ParameterNo;
                    Param.ParameterType = shelveMethodParam.ParameterType;
                    Param.ModificationType = AriaObjectMethodParameter.modificationType.Add;

                    method.AriaObjectMethodParameters.Add(Param);
                    //Add Settings of the Parameter
                    foreach (var shelveMethodParamSettings in shelveMethodParam.AriaObjectShelveMethodParameterSettings)
                    {
                        var ParamSetting = new AriaObjectMethodParameterSetting(objectShelve.Session);

                        ParamSetting.DataType = shelveMethodParamSettings.DataType;
                        ParamSetting.DecimalPlaces = shelveMethodParamSettings.DecimalPlaces;
                        ParamSetting.SettingType = shelveMethodParamSettings.SettingType;
                        ParamSetting.Value = shelveMethodParamSettings.Value;
                        ParamSetting.Width = shelveMethodParamSettings.Width;

                        Param.AriaObjectMethodParameterSettings.Add(ParamSetting);
                    }
                }

                ariaObject.AriaObjectMethods.Add(method);
            }

            ariaObject.Save();
            //mmt
            // ariaObject.Session.CommitTransaction();
            //mmt
        }

        public void AddNewEvents(AriaObjectShelve objectShelve)
        {
            AriaObject ariaObject = objectShelve.AriaObject;

            //Add Event
            foreach (var shelveEvent in objectShelve.AriaObjectShelveEvents)
            {
                var objectEvent = new AriaObjectEvent(objectShelve.Session);

                objectEvent.EventDescription = shelveEvent.EventDescription;
                objectEvent.EventName = shelveEvent.EventName;
                objectEvent.ModificationType = ((AriaObjectEvent.modificationType)shelveEvent.ModificationType);
                objectEvent.AriaObject = ariaObject.Session.FindObject<AriaObject>(CriteriaOperator.Parse("Oid ='" + ariaObject.Oid + "'"));

                //Add Event's Parameter
                foreach (var shelveEventParam in objectEvent.AriaObjectEventParameters)
                {
                    var shelveParam = new AriaObjectEventParameter(objectShelve.Session);

                    shelveParam.ParameterName = shelveEventParam.ParameterName;
                    shelveParam.ParameterNo = shelveEventParam.ParameterNo;
                    shelveParam.ParameterType = shelveEventParam.ParameterType;
                    shelveParam.ModificationType = shelveEventParam.ModificationType;

                    objectEvent.AriaObjectEventParameters.Add(shelveParam);
                    //Add Parameter's Setting
                    foreach (var shelveEventParamSettings in shelveEventParam.AriaObjectEventParameterSettings)
                    {
                        var shelveParamSetting = new AriaObjectEventParameterSetting(objectShelve.Session);

                        shelveParamSetting.DataType = shelveEventParamSettings.DataType;
                        shelveParamSetting.DecimalPlaces = shelveEventParamSettings.DecimalPlaces;
                        shelveParamSetting.Format = shelveEventParamSettings.Format;
                        shelveParamSetting.SettingType = shelveEventParamSettings.SettingType;
                        shelveParamSetting.Value = shelveEventParamSettings.Value;
                        shelveParamSetting.Width = shelveEventParamSettings.Width;

                        shelveParam.AriaObjectEventParameterSettings.Add(shelveParamSetting);
                    }
                }


                ariaObject.AriaObjectEvents.Add(objectEvent);

            }


            ariaObject.Save();
            //MMT
            //ariaObject.Session.CommitTransaction();
            //MMT
        }


        #endregion

        #region Check In
        private void CheckIn_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            string filepath = string.Empty;
            string AssignedResource = string.Empty;
            TrackingEntry trackingEntry = (TrackingEntry)e.CurrentObject;

            TfsManager tfsHandler = new TfsManager();

            AssignedResource = "Khaled Mayhoub";

            // loop on all shelve files to get the files which have been selected from the list

            foreach (AriaObjectShelve selectedFile in trackingEntry.AriaObjectShelves)
            {
                string FilePath = selectedFile.AriaObject.AriaObjectSettings.Where(r => r.SettingType.SettingTypeId == "FilePath").Count() <= 0 ? "" :
                                    selectedFile.AriaObject.AriaObjectSettings.First(r => r.SettingType.SettingTypeId == "FilePath").Value;

                if (!string.IsNullOrEmpty(FilePath))
                {
                    // int changSet = tfsHandler.CommentCheckin(FilePath, AssignedResource, "[" + AssignedResource + "]" + " Tracking #" + trackingEntry.ID + " [" + trackingEntry.RequestedDate.ToShortDateString() + "] " + selectedFile.Description);

                    selectedFile.State = "CheckedIn";

                    //  selectedFile.ChangeSet = changSet.ToString();

                    selectedFile.Save();

                    selectedFile.Session.CommitTransaction();

                    //    tfsHandler.DenyCheckoutFilePermission(FilePath, AssignedResource);


                }
            }

            Frame.View.ObjectSpace.Refresh();
        }
        #endregion

        private void Cancel_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            string AssignedResource = string.Empty;

            TrackingEntry trackingEntry = (TrackingEntry)e.CurrentObject;
            AriaObjectShelve ariaObjectShelve = new AriaObjectShelve(trackingEntry.Session);

            XPCollection allTrackingAriaObjectShelve = new XPCollection(trackingEntry.Session, typeof(AriaObjectShelve), CriteriaOperator.Parse("[TrackingEntry] = '" + currentriaTrakingEntry + "'"));

            foreach (AriaObjectShelve selectedFile in allTrackingAriaObjectShelve)
            {
                string TfsDomain = selectedFile.AriaObjectShelveSettings.Where(r => r.SettingType.Name == "TfsDomain").Count() <= 0 ? "" :
                    selectedFile.AriaObjectShelveSettings.First(r => r.SettingType.Name == "TfsDomain").Value;

                string TfsProjectName = selectedFile.AriaObjectShelveSettings.Where(r => r.SettingType.Name == "TfsProjectName").Count() <= 0 ? "" :
                    selectedFile.AriaObjectShelveSettings.First(r => r.SettingType.Name == "TfsProjectName").Value;

                string TfsGroupName = selectedFile.AriaObjectShelveSettings.Where(r => r.SettingType.Name == "TfsGroupName").Count() <= 0 ? "" :
                    selectedFile.AriaObjectShelveSettings.First(r => r.SettingType.Name == "TfsGroupName").Value;

                string FilePath = selectedFile.AriaObject.AriaObjectSettings.Where(r => r.SettingType.SettingTypeId == "Filepath").Count() <= 0 ? "" :
                                    selectedFile.AriaObject.AriaObjectSettings.First(r => r.SettingType.SettingTypeId == "Filepath").Value;



                TfsManager tfsHandler = new TfsManager();

                foreach (TrackingTask task in trackingEntry.TrackingTasks)
                {
                    if (task.Task.CheckOut == true)
                    {
                        AssignedResource = task.Resources.Name; //"Khaled Mayhoub";
                        // tfsHandler.DenyCheckoutFilePermission(FilePath, AssignedResource);
                    }
                }

                selectedFile.State = "Cancelled";

                selectedFile.Save();
                trackingEntry.Status = BusinessObjects.TrackingEntry.TrackingStatus.Cancelled;

                trackingEntry.Save();

                trackingEntry.Session.CommitTransaction();
            }
        }

        //private void GrantCheckOut_Execute(object sender, SimpleActionExecuteEventArgs e)
        //{
        //    TfsManager tfs = new TfsManager();
        //    string fileName = "", filePath = "", fullPath = "";
        //    if (this.View.CurrentObject != null && View.CurrentObject is TrackingEntry)
        //    {
        //       // TrackingTask task = ((TrackingEntry)this.View.CurrentObject).TrackingTasks.Where(x => x.Task.CheckOut == true && x.Status == TrackingTask.StatusTypes.New).FirstOrDefault();
        //        foreach (var item in ((TrackingEntry)this.View.CurrentObject).TrackingTasks)
        //        {
        //            if (((TrackingTask)item).Task.CheckOut == true && ((TrackingTask)item).Status == TrackingTask.StatusTypes.New)
        //            {
        //                string assignedSource = ((TrackingTask)item).Resources.Name;
        //                foreach (AriaObjectShelve objectShe in ((TrackingEntry)View.CurrentObject).AriaObjectShelves)
        //                {
        //                    for (int i = 0; i < objectShe.AriaObjectShelveSettings.Count; i++)
        //                    {
        //                        if (objectShe.AriaObjectShelveSettings[i].SettingType.SettingTypeId == "StorageFileName")
        //                        {
        //                            fileName = objectShe.AriaObjectShelveSettings[i].Value;
        //                        }
        //                        if (objectShe.AriaObjectShelveSettings[i].SettingType.SettingTypeId == "SubFolder")
        //                        {
        //                            filePath = objectShe.AriaObjectShelveSettings[i].Value;
        //                        }
        //                        fullPath = filePath + "\\" + fileName;
        //                    }
        //                    tfs.GrantCheckoutFilePermission(fullPath, assignedSource);
        //                }

        //              //  tfs.GrantCheckoutFilePermission(fullPath, assignedSource);
        //               ((TrackingEntry)this.View.CurrentObject).CheckInComment = ((TrackingEntry)this.View.CurrentObject).TicketNumber + " - " + ((TrackingEntry)this.View.CurrentObject).ID.ToString() + " - " + assignedSource;

        //            }
        //            else
        //            {
        //              //  DevExpress.XtraEditors.XtraMessageBox.Show("Please Change Checkout task Status to New!");
        //            }
        //        }
        //       // DevExpress.XtraEditors.XtraMessageBox.Show("Resource can Check out the Files!");
        //    }
        //}

        //ATA 8/11/2016 enhance grantcheckout code [Start]
        private void GrantCheckOut_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            TfsManager tfs = new TfsManager();
            string fileName = "", filePath = "", fullPath = "";
            if (this.View.CurrentObject != null && View.CurrentObject is TrackingEntry)
            {
                TrackingTask task = ((TrackingEntry)this.View.CurrentObject).TrackingTasks.Where(x => x.Task.CheckOut == true && x.Status == TrackingTask.StatusTypes.New).FirstOrDefault();
                if (task != null)
                {
                    string assignedSource = task.Resources.Name;
                    //ATA convert the status to inwork 
                    task.Status = TrackingTask.StatusTypes.InWork;
                    foreach (AriaObjectShelve objectShe in ((TrackingEntry)View.CurrentObject).AriaObjectShelves)
                    {
                        if (objectShe.AriaObjectShelveSettings.Count > 0)
                        {
                            AriaObjectShelveSetting Storagefilenametype = objectShe.AriaObjectShelveSettings.Where(x => x.SettingType.SettingTypeId == "StorageFileName").FirstOrDefault();
                            if (Storagefilenametype != null)
                            {
                                fileName = Storagefilenametype.Value;
                            }
                            AriaObjectShelveSetting Subfoldertype = objectShe.AriaObjectShelveSettings.Where(x => x.SettingType.SettingTypeId == "SubFolder").FirstOrDefault();
                            if (Subfoldertype != null)
                            {
                                filePath = Subfoldertype.Value;

                            }
                            fullPath = filePath + "\\" + fileName;
                        }
                        //ATA start 8/14/2017 old check out method 
                        // tfs.GrantCheckoutFilePermission(fullPath, assignedSource);
                        //ATA new method to handle aria4xp files type 
                        if (!string.IsNullOrEmpty(filePath))
                            Grantcheckoutfile(fullPath, assignedSource, tfs);
                        //ATA end  8/14/2017  
                        objectShe.CodeStatus = AriaObjectShelve.Status.CheckedOut;
                        objectShe.Save();
                    }

                    //  tfs.GrantCheckoutFilePermission(fullPath, assignedSource);
                    ((TrackingEntry)this.View.CurrentObject).CheckInComment = ((TrackingEntry)this.View.CurrentObject).ReferenceNo + " - " + ((TrackingEntry)this.View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString() + " - " + assignedSource;
                    Application.ShowViewStrategy.ShowMessage("The Permission is Granted for the user " + assignedSource + "", InformationType.Success);
                }
            }
        }

        //ATA check out the second files for aria4xp 8/14/2017[start]
        public void Grantcheckoutfile(string fullPath, string assignedSource, TfsManager tfs)
        {

            tfs.GrantCheckoutFilePermission(fullPath, assignedSource);
            switch (Path.GetExtension(fullPath).ToUpper())
            {
                case ".SCX":
                    string sct = fullPath.Replace(".SCX", ".SCT");
                    tfs.GrantCheckoutFilePermission(sct, assignedSource);
                    break;
                case ".FRX":
                    string frt = fullPath.Replace(".FRX", ".FRT");
                    tfs.GrantCheckoutFilePermission(frt, assignedSource);
                    break;
                case ".VCX":
                    string vct = fullPath.Replace(".VCX", ".VCT");
                    tfs.GrantCheckoutFilePermission(vct, assignedSource);
                    break;
                case ".DBF":
                    string fpt = fullPath.Replace(".DBF", ".FPT");
                    tfs.GrantCheckoutFilePermission(fpt, assignedSource);

                    string cdx = fullPath.Replace(".DBF", ".CDX");
                    tfs.GrantCheckoutFilePermission(cdx, assignedSource);
                    break;
                case ".LBX":
                    string LBT = fullPath.Replace(".LBX", ".LBT");
                    tfs.GrantCheckoutFilePermission(LBT, assignedSource);
                    break;
            }
        }

        public void Denycheckoutfile(string fullPath, string assignedSource, TfsManager tfs)
        {

            tfs.DenyCheckoutFilePermission(fullPath, assignedSource);
            switch (Path.GetExtension(fullPath))
            {
                case ".SCX":
                    string sct = fullPath.Replace(".SCX", ".SCT");
                    tfs.DenyCheckoutFilePermission(sct, assignedSource);
                    break;
                case ".FRX":
                    string frt = fullPath.Replace(".FRX", ".FRT");
                    tfs.DenyCheckoutFilePermission(frt, assignedSource);
                    break;
                case ".VCX":
                    string vct = fullPath.Replace(".VCX", ".VCT");
                    tfs.DenyCheckoutFilePermission(vct, assignedSource);
                    break;
                case ".DBF":
                    string fpt = fullPath.Replace(".DBF", ".FPT");
                    tfs.DenyCheckoutFilePermission(fpt, assignedSource);

                    string cdx = fullPath.Replace(".DBF", ".CDX");
                    tfs.DenyCheckoutFilePermission(cdx, assignedSource);
                    break;
                case ".LBX":
                    string LBT = fullPath.Replace(".LBX", ".LBT");
                    tfs.DenyCheckoutFilePermission(LBT, assignedSource);
                    break;
            }
        }
        //ATA check out the second files for aria4xp 8/14/2017[End]
        #region commented code for grantcheck out 
        //ATA 8/11/2016 enhance grantcheckout code [END]
        //public void denyCheckout()
        //{
        //    //ATA 
        //    ///TfsManager tfs = new TfsManager("http://tf_server:8080/tfs/Aria_Dev-2015", "Aria 5 System Admin");
        //    TfsManager tfs = new TfsManager();
        //    //ATA 
        //    if (this.View.CurrentObject != null && View.CurrentObject is TrackingEntry)
        //    {
        //        foreach (var item in ((TrackingEntry)this.View.CurrentObject).TrackingTasks)
        //        {
        //            if (((TrackingTask)item).Task.CheckOut == true && ((TrackingTask)item).Status == TrackingTask.StatusTypes.New)
        //            {
        //                string assignedSource = ((TrackingTask)item).Resources.Name;
        //                foreach (AriaObjectShelve objectShe in ((TrackingEntry)View.CurrentObject).AriaObjectShelves)
        //                {
        //                    string fileName = "", filePath = "", fullPath = "";

        //                    for (int i = 0; i < objectShe.AriaObjectShelveSettings.Count; i++)
        //                    {
        //                        if (objectShe.AriaObjectShelveSettings[i].SettingType.SettingTypeId == "StorageFileName")
        //                        {
        //                            fileName = objectShe.AriaObjectShelveSettings[i].Value;
        //                        }
        //                        if (objectShe.AriaObjectShelveSettings[i].SettingType.SettingTypeId == "SubFolder")
        //                        {
        //                            filePath = objectShe.AriaObjectShelveSettings[i].Value;
        //                        }
        //                        fullPath = filePath + "\\" + fileName;
        //                    }

        //                    if (fullPath != "") tfs.DenyCheckoutFilePermission(fullPath, assignedSource);

        //                }

        //            }
        //        }
        //        //DevExpress.XtraEditors.XtraMessageBox.Show("Resource denied to checkout the Files!");
        //    }
        //}
        #endregion
        //ATA enhance chek in code 11/8/2016 [Start]
        public void denyCheckout()
        {
            //ATA 
            ///TfsManager tfs = new TfsManager("http://tf_server:8080/tfs/Aria_Dev-2015", "Aria 5 System Admin");
            TfsManager tfs = new TfsManager();
            //ATA 
            if (this.View.CurrentObject != null && View.CurrentObject is TrackingEntry)
            {
                TrackingTask task = ((TrackingEntry)this.View.CurrentObject).TrackingTasks.Where(x => x.Task.CheckOut == true).FirstOrDefault();
                if (task != null)
                {
                    string assignedSource = task.Resources.Name;
                    foreach (AriaObjectShelve objectShe in ((TrackingEntry)View.CurrentObject).AriaObjectShelves)
                    {
                        string fileName = "", filePath = "", fullPath = "";

                        if (objectShe.AriaObjectShelveSettings.Count > 0)
                        {
                            AriaObjectShelveSetting Storagefilenametype = objectShe.AriaObjectShelveSettings.Where(x => x.SettingType.SettingTypeId == "StorageFileName").FirstOrDefault();
                            if (Storagefilenametype != null)
                            {
                                fileName = Storagefilenametype.Value;
                            }
                            AriaObjectShelveSetting Subfoldertype = objectShe.AriaObjectShelveSettings.Where(x => x.SettingType.SettingTypeId == "SubFolder").FirstOrDefault();
                            if (Subfoldertype != null)
                            {
                                filePath = Subfoldertype.Value;

                            }
                            fullPath = filePath + "\\" + fileName;
                        }

                        if (fullPath != "")
                            //ATA new function to deny check out 
                            Denycheckoutfile(fullPath, assignedSource, tfs);
                        //ATA old denycheckout function 
                        //tfs.DenyCheckoutFilePermission(fullPath, assignedSource);

                    }

                }
            }
            //DevExpress.XtraEditors.XtraMessageBox.Show("Resource denied to checkout the Files!");
        }
        //ATA Deny Check out code 11/8/2016 [End]

        private void DenyCheckOut_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            denyCheckout();
        }



        //ATA enhance chek in code 11/8/2016 [Start]
        public void CommentCheckIn_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            string TFSCollection = "", TFSProjectName = "", TFSProjectPath = "";
            string fileName = "", filePath = "", fullPath = "", ShelveSet = "", CheckInComment = "";

            if (this.View.CurrentObject != null && View.CurrentObject is TrackingEntry)
            {
                Application_T x = ((TrackingEntry)this.View.CurrentObject).Application;

                if (x.ApplicationSettings.Count > 0)
                {
                    ApplicationSetting Tfscollectionsetting = x.ApplicationSettings.Where(y => y.SettingType.SettingTypeId == "Tfscollection").FirstOrDefault();
                    if (Tfscollectionsetting != null)
                    {
                        TFSCollection = Tfscollectionsetting.SettingValue;
                    }
                    ApplicationSetting Tfsprjectsetting = x.ApplicationSettings.Where(y => y.SettingType.SettingTypeId == "Tfsproject").FirstOrDefault();
                    if (Tfsprjectsetting != null)
                    {
                        TFSProjectName = Tfsprjectsetting.SettingValue;
                    }
                    TFSProjectPath = "$/" + TFSProjectName;
                }
                TfsManager tfs = new TfsManager(TFSCollection, TFSProjectName);

                TrackingTask task = ((TrackingEntry)this.View.CurrentObject).TrackingTasks.Where(i => i.Task.CheckOut == true && i.Status == TrackingTask.StatusTypes.Complete).FirstOrDefault();
                if (task != null)
                {
                    string assignedSource = task.Resources.Name;
                    //MMT
                    //CheckInComment = ((TrackingEntry)this.View.CurrentObject).TicketNumber + " - " + ((TrackingEntry)this.View.CurrentObject).ID.ToString() + " - " + assignedSource;
                    CheckInComment = ((TrackingEntry)this.View.CurrentObject).ReferenceNo + " - " + ((TrackingEntry)this.View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString() + " - " + assignedSource;
                    //
                    ShelveSet = ((TrackingEntry)this.View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString();
                    //MMT
                    TFSProjectPath = x.Name;
                    //MMT
                    tfs.CommentCheckin(TFSProjectPath, ShelveSet, assignedSource, CheckInComment);

                    foreach (AriaObjectShelve objectShe in ((TrackingEntry)View.CurrentObject).AriaObjectShelves)
                    {
                        fileName = ""; filePath = ""; fullPath = "";
                        // objectShe.ChangeSet = changeset.ToString();

                        if (objectShe.AriaObjectShelveSettings.Count > 0)
                        {
                            AriaObjectShelveSetting Storagefilenametype = objectShe.AriaObjectShelveSettings.Where(z => z.SettingType.SettingTypeId == "StorageFileName").FirstOrDefault();
                            if (Storagefilenametype != null)
                            {
                                fileName = Storagefilenametype.Value;
                            }
                            AriaObjectShelveSetting Subfoldertype = objectShe.AriaObjectShelveSettings.Where(z => z.SettingType.SettingTypeId == "SubFolder").FirstOrDefault();
                            if (Subfoldertype != null)
                            {
                                filePath = Subfoldertype.Value;
                                //MMT
                                if (filePath.ToUpper().Contains("\\Aria4XP\\Aria4XP\\".ToUpper()))
                                {
                                    filePath = filePath.ToUpper().Replace("\\Aria4XP\\Aria4XP\\".ToUpper(), "\\Aria4XP\\".ToUpper());
                                }
                                //MMT
                            }
                            fullPath = filePath + "\\" + fileName;
                        }
                        //ATA 2/9/2017 
                        objectShe.CodeStatus = AriaObjectShelve.Status.CheckedIn;
                    }
                    //   tfs.DenyCheckoutFilePermission(fullPath, assignedSource);
                    denyCheckout();
                    // ((TrackingEntry)this.View.CurrentObject).Status = TrackingEntry.TrackingStatus.Complete;

                }

            }
        }
        //ATA enhance chek in code 11/8/2016 [End]

        private void CreateTestRunForTracking_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            if (this.View.CurrentObject != null && this.View.CurrentObject is TrackingEntry)
            {
                if (((TrackingEntry)this.View.CurrentObject).Entity != null)
                {
                    //ATA change the code to get the test cases from the tracking entry not the project 11/15/2017 [Start]

                    //string userName = ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters).UserName;
                    //TestRun NewTestRun = new TestRun(((TrackingEntry)this.View.CurrentObject).Session);
                    //NewTestRun.Name = "TR- " + ((TrackingEntry)this.View.CurrentObject).ProjectTemplate.Name + "- " + userName + "- " + DateTime.Now.ToString();
                    //XPCollection<TestCase> EntityTestCases = new XPCollection<TestCase>(((TrackingEntry)this.View.CurrentObject).Session, 
                    //                CriteriaOperator.Parse("ProjectTemplate='" + ((TrackingEntry)this.View.CurrentObject).ProjectTemplate.Oid + 
                    //                                       "' and Application ='" + ((TrackingEntry)this.View.CurrentObject).Application.Oid + 
                    //                                       "'and Entity='" + ((TrackingEntry)this.View.CurrentObject).Entity.Oid + "'"));
                    //EntityTestCases.Load();
                    //if (EntityTestCases.Count > 0) 
                    //{
                    //    NewTestRun.TestCases.AddRange(EntityTestCases);
                    //    NewTestRun.Save();
                    //    ((TrackingEntry)this.View.CurrentObject).TestRuns.Add(NewTestRun);

                    //    ((TrackingEntry)this.View.CurrentObject).Session.CommitTransaction();
                    //    this.View.Refresh();
                    //}
                    IObjectSpace ospace = Application.CreateObjectSpace();
                    TrackingEntry currenttrackingentryentry = ospace.FindObject<TrackingEntry>(CriteriaOperator.Parse("[Oid] == '" + ((TrackingEntry)this.View.CurrentObject).Oid + "'"));
                    string userName = ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters).UserName;
                    // Doaa 05/05/2019 {start}
                    TestRun NewTestRunUnit = ospace.CreateObject<TestRun>();//new TestRun(((TrackingEntry)this.View.CurrentObject).Session);
                    TestRun NewTestRunCross = ospace.CreateObject<TestRun>();//new TestRun(((TrackingEntry)this.View.CurrentObject).Session);
                                                                             // Doaa 05/05/2019 {end}

                    // Doaa 05/01/2019 {start}
                    if (((TrackingEntry)this.View.CurrentObject).ProjectTemplate == null)
                    {
                        throw new Exception("Can't create test runs without adding a project");
                    }
                    // Doaa 05/01/2019 {End}
                    NewTestRunUnit.Name = "TR- " + ((TrackingEntry)this.View.CurrentObject).ProjectTemplate.Name + "- " + userName + "(Unit Test) - " + DateTime.Now.ToString();
                    NewTestRunCross.Name = "TR- " + ((TrackingEntry)this.View.CurrentObject).ProjectTemplate.Name + "- " + userName + "(Cross Test) - " + DateTime.Now.ToString();

                    foreach (TestCase Testcase in currenttrackingentryentry.TestCases.Where(t => t.IsOriginal == true))
                    {
                        TestCase ClonedTestCase = new TestCase(Testcase.Session, Testcase, currenttrackingentryentry);
                        // Doaa 05/05/2019 {start}
                        NewTestRunUnit.TestCases.Add(ClonedTestCase);
                        NewTestRunCross.TestCases.Add(ClonedTestCase);
                        // Doaa 05/05/2019 {End}

                    }
                    if (currenttrackingentryentry.HasChildren == true)
                    {
                        foreach (TrackingEntry Childtracking in currenttrackingentryentry.ChildTrackingEntries)
                        {
                            foreach (TestCase Testcase in Childtracking.TestCases.Where(t => t.IsOriginal == true))
                            {
                                TestCase ClonedTestCase = new TestCase(Testcase.Session, Testcase, currenttrackingentryentry);
                                // Doaa 05/05/2019 {start}
                                NewTestRunUnit.TestCases.Add(ClonedTestCase);
                                NewTestRunCross.TestCases.Add(ClonedTestCase);
                                // Doaa 05/05/2019 {end}
                            }
                        }
                    }
                    if ((currentTrakingEntryObject.HasChildren == false && currentTrakingEntryObject.TestPlans.FirstOrDefault() != null) || currenttrackingentryentry.HasChildren == true)
                    {
                        // Doaa 05/05/2019 {start}
                        NewTestRunUnit.TestPlan = currenttrackingentryentry.TestPlans.FirstOrDefault();
                        NewTestRunCross.TestPlan = currenttrackingentryentry.TestPlans.FirstOrDefault();
                        // Doaa 05/05/2019 {end}

                    }
                    else if (currentTrakingEntryObject.HasChildren == false && currentTrakingEntryObject.TestPlans.FirstOrDefault() == null && currenttrackingentryentry.ParentTrackingEntry != null)
                    {
                        // Doaa 05/05/2019 {start}
                        NewTestRunUnit.TestPlan = currenttrackingentryentry.ParentTrackingEntry.TestPlans.FirstOrDefault();
                        NewTestRunCross.TestPlan = currenttrackingentryentry.ParentTrackingEntry.TestPlans.FirstOrDefault();
                        // Doaa 05/05/2019 {end}

                    }
                    // Doaa 05/05/2019 {start}
                    NewTestRunUnit.Save();
                    NewTestRunCross.Save();

                    currenttrackingentryentry.TestRuns.Add(NewTestRunUnit);
                    currenttrackingentryentry.TestRuns.Add(NewTestRunCross);
                    // Doaa 05/05/2019 {end}
                    ospace.CommitChanges();
                    this.View.Refresh();
                    //ATA change the code to get the test cases from the tracking entry not the project 11/15/2017 [End]

                }

            }
        }
        //ATA orginal create fix code 
        //MMT
        private void ApproveTracking_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            IObjectSpace ospace = Application.CreateObjectSpace();
            TrackingEntry TrackingEntry = ospace.FindObject<TrackingEntry>(CriteriaOperator.Parse("[Oid] = '" + ((TrackingEntry)View.CurrentObject).Oid + "'"));
            if (TrackingEntry.Status != TrackingEntry.TrackingStatus.InWork  )
            {
                throw new Exception("This Tracking entry cannot be approved because its current status is: " + TrackingEntry.Status+", it must be In Work.");
            }
            else {
                
                TrackingEntry.Status = TrackingEntry.TrackingStatus.ReleaseCandidate;
                TrackingEntry.ApproveDate = DateTime.Today;
                TrackingEntry.ApprovedBy = SecuritySystem.CurrentUserName;
                TrackingEntry.Save();
                TrackingEntry.Session.CommitTransaction();
            }
    }
        //MMT
        //ATA add new action that to handle creating tracking fixes 7/13/2017  [strat]
        private void CreateFix_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            Aria5SystemAdmin.Module.ServiceReferenceTfsManager.WebService1SoapClient CreateFixWebServiceClient = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.WebService1SoapClient();
            CreateFixWebServiceClient.InnerChannel.OperationTimeout = new TimeSpan(1, 0, 0);
            IObjectSpace ospace = Application.CreateObjectSpace();

            TrackingEntry TrackingEntry = ospace.FindObject<TrackingEntry>(CriteriaOperator.Parse("[Oid] = '" + ((TrackingEntry)View.CurrentObject).Oid + "'"));

            bool calltfsService = true;

            //foreach (var item in TrackingEntry.AriaObjectShelves)
            //{
            //    if (item.AriaObject.ObjectType.Name.ToUpper() == "ariareport".ToUpper())
            //    {
            //        calltfsService = false;
            //        break;
            //    }
            //}
            //Doaa 05/01/2019 Start
            foreach (var item in TrackingEntry.AriaObjectShelves)
            {
                if (item.AriaObject.ObjectType.ObjectTypeID.ToUpper() == "Ariareport".ToUpper()
                    || item.AriaObject.ObjectType.ObjectTypeID.ToUpper() == "Ariaobject".ToUpper()
                    || item.AriaObject.ObjectType.ObjectTypeID.ToUpper() == "Ariafile".ToUpper()
                    || item.AriaObject.ObjectType.ObjectTypeID.ToUpper() == "Ariatrigger".ToUpper()
                    || item.AriaObject.ObjectType.ObjectTypeID.ToUpper() == "Menu".ToUpper()
                    || item.AriaObject.ObjectType.ObjectTypeID.ToUpper() == "Ariaindex".ToUpper()
                    || item.AriaObject.ObjectType.ObjectTypeID.ToUpper() == "AriaField".ToUpper())
                {
                    calltfsService = false;



                    //break;
                }
                else { calltfsService = true; }

            }

            //Doaa 05/01/2019 End

            string exceptionmsg = "";
            if (CreateEnvironment(CreateFixWebServiceClient, TrackingEntry, calltfsService, out exceptionmsg) == false)
            {
                //DM -MMT
                //throw new Exception("Can't Access the shelve with this tracking number so we can't proceed in creating fix");
                throw new Exception(exceptionmsg);
                //DM - MMT
            }

            string PName = TrackingEntry.Application.Id;
            if (!PName.Contains("4"))
            {
                PName = "EDI";
            }
            if (CraeteAttachementfolder(CreateFixWebServiceClient, TrackingEntry, PName))
            {


                // Sara.n Change Create fix to take fix Type not Bug only [Start]
                // if (CreateFixWebServiceClient.createfix("B" + ((TrackingEntry)this.View.CurrentObject).ID.ToString(), PName))
                if (CreateFixWebServiceClient.createfix(((TrackingEntry)this.View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString(), PName))
                {
                    //string fixname = "B" + ((TrackingEntry)this.View.CurrentObject).ID.ToString();
                    string fixname = ((TrackingEntry)this.View.CurrentObject).Type.ToString()[0] + ((TrackingEntry)this.View.CurrentObject).ID.ToString().PadLeft(6, '0');
                    TrackingEntry.TrackingFix = "ftp://192.168.1.171/tracking/Fixes/" + fixname + "/" + fixname + ".EXE";
                    TrackingEntry.Save();
                    ospace.CommitChanges();
                    // Sara.n , Take fix Type [Start]
                    //Application.ShowViewStrategy.ShowMessage("Fix Number 'B" + ((TrackingEntry)this.View.CurrentObject).ID.ToString() + "' is succefully created");
                    //MMT
                    // Application.ShowViewStrategy.ShowMessage("Fix Number '" + ((TrackingEntry)View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString() + "' is succefully created");
                    MessageBox.Show(Application, "Fix Number '" + ((TrackingEntry)View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString() + "' is succefully created",
                            new Action(delegate
                            {
                                ObjectSpace.Refresh();
                            }));
                    //MMT
                    // Sara.n , Take fix Type [End]
                }
            }
            else
            {
                throw new Exception("Faild to create attachment folder ");
            }
        }

        public bool CreateEnvironment(WebService1SoapClient createfixwebserviceclient, TrackingEntry TrackingEntry, bool CallTFSService,out string exceptionmsg)
        {

          
            TrackingTask TrackingTask = TrackingEntry.TrackingTasks.Where(t => t.Task.CheckOut == true).FirstOrDefault();
            Boolean isCreated = true;
            exceptionmsg = "";
            if (TrackingTask != null && TrackingTask.Resources != null)
            {
                if (TrackingEntry.Application.Id.ToUpper().Contains("ARIA4XP"))
                {
                    // PName = "Aria4xp";   
                    // Sara.n , Take fix Type [Start]
                    // isCreated = createfixwebserviceclient.Createenvironment("B" + ((TrackingEntry)View.CurrentObject).ID.ToString(), "Aria4xp", TrackingTask.Resources.Name);
                    
                    isCreated = createfixwebserviceclient.Createenvironment(((TrackingEntry)View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)View.CurrentObject).ID.ToString().PadLeft (6,'0'), "Aria4xp", TrackingTask.Resources.Name, CallTFSService, out exceptionmsg);
                    // Sara.n , Take fix Type [End]
                }
                else
                {
                    //PName = "EDI";
                    // Sara.n , Take fix Type [Start]
                    // isCreated = createfixwebserviceclient.Createenvironment("B" + ((TrackingEntry)View.CurrentObject).ID.ToString(), "EDI", TrackingTask.Resources.Name);
                    isCreated = createfixwebserviceclient.Createenvironment(((TrackingEntry)View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)View.CurrentObject).ID.ToString(), "EDI", TrackingTask.Resources.Name, CallTFSService,out exceptionmsg);
                    // Sara.n , Take fix Type [End]
                }
            }
            else
            {
                throw new Exception("Access source code file is not exist or it's user is not exist");
            }
            return isCreated;
        }

        public bool CraeteAttachementfolder(WebService1SoapClient createfixwebserviceclient, TrackingEntry TrackingEntry, string PName)
        {
            //MMT
            bool llRetValue = false;
            //IList<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments> allattachments = new List<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments>();
            //MMT
            XPCollection<AriaObjectShelve> Shelves = TrackingEntry.AriaObjectShelves;
            var belal = View;
            if (TrackingEntry.HasChildren)
            {
                foreach (TrackingEntry Child in TrackingEntry.ChildTrackingEntries)
                {
                    Shelves.AddRange(Child.AriaObjectShelves.ToList());
                }
            }
            foreach (AriaObjectShelve objectShe in Shelves)
            {
                //MMT
                IList<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments> allattachments = new List<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments>();
                //MMT
                if (objectShe.ObjectType.ObjectTypeID.ToUpper () == "AriaFile".ToUpper ())
                {
                    // allattachments.Add(getthefilefieldsysfile(createfixwebserviceclient, objectShe));
                    Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments NewFields = getthefilefieldsysfile(createfixwebserviceclient, objectShe);
                    if (NewFields != null)
                    allattachments.Add(NewFields);
                }
                // Sara.n , Take fix Type [Start]
                //Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments[] attachments = CreateAttachmentRecordforsys(objectShe, "B" + ((TrackingEntry)View.CurrentObject).ID.ToString());
                Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments[] attachments = CreateAttachmentRecordforsys(objectShe, ((TrackingEntry)View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)View.CurrentObject).ID.ToString());
                // Sara.n , Take fix Type [End]
                if (attachments.Count() > 0)
                {
                    foreach (Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments sysfileattachement in attachments)
                    {
                        if (allattachments.FirstOrDefault(x => x.Name == sysfileattachement.Name && x.Type == sysfileattachement.Type) == null)
                        {
                            ////MMT
                            //if (objectShe.ObjectType.ObjectTypeID == "AriaField")
                            //{
                            //    sysfileattachement.Key = objectShe.AriaObject.ObjectName ;
                            //}
                            //MMT
                            allattachments.Add(sysfileattachement);
                        }
                    }
                    Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Testarray[] arrayofobjects = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Testarray[objectShe.AriaObjectShelveSettings.Where(c => !string.IsNullOrEmpty(c.ActualColumnName)).Count()];
                    if (objectShe.AriaObjectShelveSettings.Count > 0)
                    {
                        int rows = 0;
                        foreach (AriaObjectShelveSetting setting in objectShe.AriaObjectShelveSettings)
                        {
                            if (setting.ActualColumnName != null && !string.IsNullOrEmpty(setting.ActualColumnName))
                            {

                                Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Testarray oneobject = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Testarray();
                                oneobject.columnname = setting.ActualColumnName.ToLower().Trim();
                                oneobject.valuename = setting.Value == null ? "" : setting.Value.Trim();
                                arrayofobjects[rows] = oneobject;
                                rows++;
                            }
                        }
                    }
                    string modificationtype = objectShe.ModificationType.ToString().Substring(0, 1);
                    // Sara.n , Take fix Type [Start]
                    // createfixwebserviceclient.Createsysfiletable(arrayofobjects, "B" + ((TrackingEntry)this.View.CurrentObject).ID.ToString(), attachments[0].Name.ToUpper(), attachments[0].Tag, modificationtype);
                    //belal ragab 
                    createfixwebserviceclient.Createsysfiletable(arrayofobjects, ((TrackingEntry)View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString(), attachments[0].Name.ToUpper(), attachments[0].Tag, modificationtype, PName);
                    // BBBBBBBBBBBB
                    //if (createfixwebserviceclient.Createsysfiletable(arrayofobjects, ((TrackingEntry)View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString(), attachments[0].Name.ToUpper(), attachments[0].Tag, modificationtype, PName))
                    //{
                    //    throw new Exception("test system file tble");
                    //}
                    // Sara.n , Take fix Type [End]
                }
                else
                {
                    attachments = getattachmentsnotsystem(objectShe);
                    foreach (Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments item in attachments)
                    {
                        allattachments.Add(item);
                    }
                }
                llRetValue = createfixwebserviceclient.Createattachmenttable(allattachments.ToArray<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments>(), ((TrackingEntry)this.View.CurrentObject).Type.ToString().Trim().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString().PadLeft(6, '0'));
            }
            //return createfixwebserviceclient.Createattachmenttable(allattachments.ToArray<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments>(), "B" + ((TrackingEntry)this.View.CurrentObject).ID.ToString());
            //MMT
            //return createfixwebserviceclient.Createattachmenttable(allattachments.ToArray<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments>(), ((TrackingEntry)this.View.CurrentObject).Type.ToString().Trim().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString().PadLeft(6,'0'));
            return llRetValue;
            //MMT

        }
        //ATA add functon that handle the filefield sysfiles
        public Module.ServiceReferenceTfsManager.Attachments getthefilefieldsysfile(WebService1SoapClient createfixwebserviceclient, AriaObjectShelve ariaObjectShelve)
        {
            if (ariaObjectShelve.AriaObjectShelveProperties.Where(p => (p.IsNew == true) && p.PropertyType.Name == "Aria Field").Count() > 0)
            {
                foreach (AriaObjectShelveProperty shelveprop in ariaObjectShelve.AriaObjectShelveProperties.Where(p => (p.IsNew == true) && p.PropertyType.Name == "Aria Field"))
                {
                    Module.ServiceReferenceTfsManager.Testarray[] arrayofobjects = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Testarray[3];
                    Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Testarray oneobject = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Testarray();
                    oneobject.columnname = "cfile_nam";
                    oneobject.valuename = ariaObjectShelve.ObjectName;
                    arrayofobjects[0] = oneobject;
                    //MMT
                    //oneobject.columnname = "cfld_name";
                    //oneobject.valuename = shelveprop.PropertyName;
                    //arrayofobjects[1] = oneobject;
                    Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Testarray oneobjectFld = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Testarray();
                    oneobjectFld.columnname = "cfld_name";
                    oneobjectFld.valuename = shelveprop.PropertyName;
                    arrayofobjects[1] = oneobjectFld;
                    //MMT
                    // string modificationtype = ariaObjectShelve.ModificationType.ToString().Substring(0, 1);
                    // Sara.n , Take fix Type [Start]
                    //createfixwebserviceclient.Createsysfiletable(arrayofobjects, "B" + ((TrackingEntry)this.View.CurrentObject).ID.ToString(), "SYDFLFLD.DBF", "CFLFLD", "A");
                    createfixwebserviceclient.Createsysfiletable(arrayofobjects, ((TrackingEntry)View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString(), "SYDFLFLD.DBF", "CFLFLD", "A", ((TrackingEntry)View.CurrentObject).Application.Name);

                    // Sara.n , Take fix Type [Start]
                }
                Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments atta = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments();
                atta.Source = "D:\\Aria4xp\\ARIA4XP\\ARIA4XP (R13)\\sysfiles\\";
                // Sara.n , Take fix Type [Start]
                //atta.Dest = "D:\\Tracking\\Fixes\\" + "B" + ((TrackingEntry)this.View.CurrentObject).ID.ToString() + "\\Attachments\\Sysfiles\\";
                atta.Dest = "D:\\Tracking\\Fixes\\" + ((TrackingEntry)View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString() + "\\Attachments\\Sysfiles\\";
                // Sara.n , Take fix Type [End]
                atta.Type = "System";
                atta.Name = "SYDFLFLD.DBF";
                atta.Tag = "CFLFLD";
                //MMT
                atta.cKeyExpression = "CFILE_NAM+CFLD_NAME";
                //MMT
                return atta;
            }
            else
            {
                return null;
            }
        }

        #region testcreatefix functions as web service 
        //private void CreateFix_Execute(object sender, SimpleActionExecuteEventArgs e)
        //{

        //    ServiceReferenceTfsManager.WebService1SoapClient testclient = new ServiceReferenceTfsManager.WebService1SoapClient();
        //    testclient.InnerChannel.OperationTimeout = new TimeSpan(1, 0, 0);
        //    //testclient.Createenvironment("B" + ((TrackingEntry)View.CurrentObject).ID.ToString());
        //    if (this.View.CurrentObject != null && View.CurrentObject is TrackingEntry)
        //    {
        //        IList<ServiceReferenceTfsManager.Attachments> allattachments = new List<ServiceReferenceTfsManager.Attachments>();
        //        // object[][] attachments = new object[30][];
        //        foreach (AriaObjectShelve objectShe in ((TrackingEntry)View.CurrentObject).AriaObjectShelves)
        //        {
        //            // attachments[x] = new object[10];
        //            ServiceReferenceTfsManager.Attachments[] attachments = CreateAttachmentRecordforsys(objectShe, "B" + ((TrackingEntry)View.CurrentObject).ID.ToString());
        //            if (attachments.Count() > 0)
        //            {

        //                foreach (ServiceReferenceTfsManager.Attachments item in attachments)
        //                {
        //                    if (allattachments.FirstOrDefault(x => x.Name == item.Name && x.Type == item.Type) == null)
        //                        allattachments.Add(item);
        //                }
        //             Testarray[] arrayofobjects = new Testarray[objectShe.AriaObjectShelveSettings.Where(c => !string.IsNullOrEmpty(c.ActualColumnName)).Count()];

        //                if (objectShe.AriaObjectShelveSettings.Count > 0)
        //                {
        //                    int rows = 0;
        //                    foreach (AriaObjectShelveSetting setting in objectShe.AriaObjectShelveSettings)
        //                    {
        //                        if (setting.ActualColumnName != null && !string.IsNullOrEmpty(setting.ActualColumnName))
        //                        {

        //                           Testarray oneobject = new Testarray();
        //                            oneobject.columnname = setting.ActualColumnName;
        //                            oneobject.valuename = setting.Value;
        //                            arrayofobjects[rows] = oneobject;
        //                            rows++;

        //                            //setting.Value = listofobj.ToString();
        //                        }



        //                    }
        //                }

        //                string modificationtype = objectShe.ModificationType.ToString().Substring(0, 1);
        //                Createsysfiletable(arrayofobjects, "B" + ((TrackingEntry)this.View.CurrentObject).ID.ToString(), attachments[0].Name.ToUpper(), attachments[0].Tag, modificationtype);
        //            }
        //            else
        //            {
        //                attachments = getattachmentsnotsystem(objectShe);
        //                foreach (ServiceReferenceTfsManager.Attachments item in attachments)
        //                {
        //                    allattachments.Add(item);
        //                }
        //            }
        //        }
        //        testclient.Createattachmenttable(allattachments.ToArray<ServiceReferenceTfsManager.Attachments>(), "B" + ((TrackingEntry)this.View.CurrentObject).ID.ToString());
        //        if (testclient.createfix("B" + ((TrackingEntry)this.View.CurrentObject).ID.ToString()))
        //        {
        //            Application.ShowViewStrategy.ShowMessage("Fix Number 'B"+((TrackingEntry)this.View.CurrentObject).ID.ToString()+"' is succefully created");
        //        }

        //    }

        //}
        ////ATA test function 

        //public bool Createsysfiletable(Testarray[] ObjectData, string TNUM, string filename, string tag, string modificationtype)
        //{
        //    System.IO.File.WriteAllText("D:\\Testsysfileswithmemofields.txt", "Get this function succefully");
        //    DataTable sqlqueries = new DataTable();
        //    OleDbConnection yourConnectionHandler = new OleDbConnection(
        //       @"Provider=VFPOLEDB.1;Data Source=D:\\sysfiles\\");
        //    yourConnectionHandler.Open();
        //    if (yourConnectionHandler.State == ConnectionState.Open)
        //    {
        //        string mySQL = "select * from " + filename.ToUpper() + " where 1 = 2";  // dbf table name
        //        OleDbCommand MyQuery = new OleDbCommand(mySQL, yourConnectionHandler);
        //        OleDbDataAdapter DA = new OleDbDataAdapter(MyQuery);
        //        DA.Fill(sqlqueries);
        //        DataRow row = sqlqueries.NewRow();
        //        sqlqueries.Rows.Add(row);
        //        for (int i = 0; i < ObjectData.GetLength(0) - 1; i++)
        //        {
        //            if (ObjectData[i] != null && ObjectData[i].columnname != null)
        //            {

        //                try
        //                {
        //                    ConvertingArraytoMemo arrtomem = new ConvertingArraytoMemo();
        //                    //ATA start convert from array to memo 
        //                    if (ObjectData[i].columnname.ToUpper().StartsWith("M"))
        //                    {
        //                        if (ObjectData[i].columnname.ToUpper().Contains("LT"))
        //                        {
        //                            List<Filter> filters = new List<Filter>();
        //                            FilterConverter fltconvertorc = new FilterConverter();
        //                            filters = (List<Filter>)fltconvertorc.ConvertFromStorageType(ObjectData[i].valuename.ToString());
        //                            if (filters.Count > 0)
        //                            {
        //                                if (ObjectData[i].columnname.ToUpper().Contains("FX"))
        //                                {
        //                                    //System.IO.File.WriteAllText("D:\\Temptableformemofields\\" + ObjectData[i].columnname.ToString() + ".txt", ObjectData[i].valuename.ToString());
        //                                    //System.IO.File.WriteAllText("D:\\Temptableformemofields\\3" + ObjectData[i].columnname.ToString() + ".txt", filters[0].DataType + filters[0].Name + filters[0].Operator + filters[0].ValueType);
        //                                    row[ObjectData[i].columnname.ToString()] = Filter.Convertor(filters);// arrtomem.ArrayToMemo(filters.ToArray(), "LAOGFXFLT");
        //                                    //System.IO.File.WriteAllText("D:\\Temptableformemofields\\2" + ObjectData[i].columnname.ToString() + ".txt", Filter.Convertor(filters));

        //                                }
        //                                else if (ObjectData[i].columnname.ToUpper().Contains("VR"))
        //                                {
        //                                    // System.IO.File.WriteAllText("D:\\Temptableformemofields\\" + ObjectData[i].columnname.ToString() + ".txt", ObjectData[i].valuename.ToString());
        //                                    // System.IO.File.WriteAllText("D:\\Temptableformemofields\\3" + ObjectData[i].columnname.ToString() + ".txt", filters[0].DataType + filters[0].Name + filters[0].Operator+filters[0].ValueType);
        //                                    row[ObjectData[i].columnname.ToString()] = Filter.Convertor(filters);//arrtomem.ArrayToMemo(filters.ToArray(), "LAOGVRFLT");
        //                                    // System.IO.File.WriteAllText("D:\\Temptableformemofields\\2" + ObjectData[i].columnname.ToString() + ".txt", Filter.Convertor(filters));

        //                                }
        //                                else
        //                                {
        //                                    // System.IO.File.WriteAllText("D:\\Temptableformemofields\\" + ObjectData[i].columnname.ToString() + ".txt", ObjectData[i].valuename.ToString());
        //                                    // System.IO.File.WriteAllText("D:\\Temptableformemofields\\3" + ObjectData[i].columnname.ToString() + ".txt", filters.Count.ToString());
        //                                    row[ObjectData[i].columnname.ToString()] = Filter.Convertor(filters);//arrtomem.ArrayToMemo(filters.ToArray(), "LAOGHDFLT");
        //                                    //System.IO.File.WriteAllText("D:\\Temptableformemofields\\2" + ObjectData[i].columnname.ToString() + ".txt", Filter.Convertor(filters));

        //                                }
        //                            }

        //                        }
        //                        else if (ObjectData[i].columnname.ToUpper().Contains("LD"))
        //                        {
        //                            List<Fields> Fields = new List<Fields>();
        //                            FieldsConverter fldconvertor = new FieldsConverter();
        //                            Fields = (List<Fields>)fldconvertor.ConvertFromStorageType(ObjectData[i].valuename.ToString());


        //                            if (Fields.Count > 0)
        //                            {
        //                                if (ObjectData[i].columnname.ToUpper().Contains("AV"))
        //                                {
        //                                    //ATA will be handled as tow arrays not just one array 
        //                                    row[ObjectData[i].columnname.ToString()] = arrtomem.ArrayToMemo(Fields.ToArray(), "LAOGFIELDH");
        //                                }
        //                                else
        //                                {
        //                                    row[ObjectData[i].columnname.ToString()] = arrtomem.ArrayToMemo(Fields.ToArray(), "LASELFIELD");
        //                                }

        //                            }

        //                        }
        //                        else if (ObjectData[i].columnname.ToUpper().Contains("IL"))
        //                        {
        //                            List<Files> Files = new List<Files>();
        //                            FilesConverter filconvertor = new FilesConverter();
        //                            Files = (List<Files>)filconvertor.ConvertFromStorageType(ObjectData[i].valuename.ToString());
        //                            if (Files.Count > 0)
        //                            {
        //                                row[ObjectData[i].columnname.ToString()] = arrtomem.ArrayToMemo(Files.ToArray(), "LASELFILE");
        //                            }
        //                        }
        //                    }
        //                    else
        //                    {
        //                        row[ObjectData[i].columnname.ToString()] = ObjectData[i].valuename.ToString();
        //                    }

        //                }
        //                catch (Exception e)
        //                {
        //                    row[ObjectData[i].columnname.ToString()] = ObjectData[i].valuename.ToString();

        //                }

        //            }
        //        }
        //        yourConnectionHandler.Close();
        //    }
        //    //  createfixproject.createnewfix newinst = new createfixproject.createnewfix();
        //    // string destpath = newinst.createsystable(filename, "D:\\Aria4xp\\ARIA4XP\\ARIA4XP (R13)\\sysfiles", "D:\\Tracking\\Fixes\\" + TNUM + "\\Attachments\\Sysfiles");
        //    //ATA get the table all fields from table as a rray 
        //    //// object[,] array = newinst.getthefields(filename, "D:\\Aria4xp\\ARIA4XP\\ARIA4XP (R13)\\sysfiles");
        //    // System.Data.OleDb.OleDbConnection OleConn = new System.Data.OleDb.OleDbConnection(@"Provider=Microsoft.Jet.OLEDB.4.0;Data Source=D:\\;Extended Properties=DBASE IV;");
        //    // OleConn.Open();
        //    // //ATA create this array to contain only the name of fields that it's data type is memo 
        //    // string[] memofields = new string[20];
        //    // string[] noncharfields = new string[20];
        //    // string[] datetimevalues = new string[10];
        //    // int y = 0;
        //    // int c = 0;
        //    // int d = 0;
        //    // StringBuilder commandcolumns = new StringBuilder();
        //    // StringBuilder commandvalues = new StringBuilder();
        //    // //ATA start generate script for the sysfile 
        //    // commandcolumns.Append("insert into  " + destpath + "(");

        //    // commandvalues.Append(")Values(");
        //    // //ATA fill the array of memofields and create first part of query 
        //    // for (int i = 1; i <= array.GetLength(0); i++)
        //    // {


        //    //     if (array[i, 2].ToString().ToUpper() == "M")
        //    //     {
        //    //         memofields[y] = array[i, 1].ToString();
        //    //         y++;
        //    //     }
        //    //     else if (array[i, 2].ToString().ToUpper() == "D")
        //    //     {
        //    //         datetimevalues[d] = array[i, 1].ToString();
        //    //         d++;

        //    //     }
        //    //     else if (array[i, 2].ToString().ToUpper() != "C" && array[i, 2].ToString().ToUpper() != "M" && array[i, 2].ToString().ToUpper() != "D")
        //    //     {
        //    //         noncharfields[c] = array[i, 1].ToString();
        //    //         c++;
        //    //     }

        //    // }
        //    // //ATA loop over the datatable to get the memo fields from it  and create the insert query 

        //    // for (int row = 0; row < sqlqueries.Rows.Count; row++)
        //    // {

        //    //     for (int column = 0; column < sqlqueries.Columns.Count; column++)
        //    //     {
        //    //         if (!string.IsNullOrEmpty(sqlqueries.Rows[row][column].ToString()))
        //    //         {
        //    //             commandcolumns.Append("[" + sqlqueries.Columns[column].ColumnName.ToString() + "],");
        //    //             if (memofields.Contains(sqlqueries.Columns[column].ColumnName.ToUpper()))
        //    //             {
        //    //                 System.IO.File.WriteAllText("D:\\tablememofields\\" + sqlqueries.Columns[column].ColumnName.ToString() + ".txt", sqlqueries.Rows[row][column].ToString());
        //    //                 commandvalues.Append("FILETOSTR('D:\\tablememofields\\" + sqlqueries.Columns[column].ColumnName.ToString() + ".txt'),");
        //    //             }
        //    //             else if (noncharfields.Contains(sqlqueries.Columns[column].ColumnName.ToUpper()))
        //    //             {
        //    //                 commandvalues.Append(sqlqueries.Rows[row][column].ToString() + ",");

        //    //             }
        //    //             else if (datetimevalues.Contains(sqlqueries.Columns[column].ColumnName.ToUpper()))
        //    //             {
        //    //                 string te = sqlqueries.Rows[row][column].ToString();
        //    //                 if (!string.IsNullOrEmpty(sqlqueries.Rows[row][column].ToString()))
        //    //                 {
        //    //                     string testdate = DateTime.Parse(sqlqueries.Rows[row][column].ToString()).Date.ToString("d");
        //    //                     commandvalues.Append("CTOD(\"" + DateTime.Parse(sqlqueries.Rows[row][column].ToString()).Date.ToString("d") + "\"),");
        //    //                 }
        //    //                 else
        //    //                 {
        //    //                     commandvalues.Append("CTOD(''),");
        //    //                 }
        //    //             }
        //    //             else
        //    //             {
        //    //                 commandvalues.Append("'" + sqlqueries.Rows[row][column].ToString() + "',");

        //    //             }
        //    //         }

        //    //     }
        //    //     StringBuilder test = new StringBuilder();
        //    //     test.Append(commandcolumns.ToString(0, commandcolumns.Length - 1) + commandvalues.ToString(0, commandvalues.Length - 1).Replace("False", ".F.").Replace("True", ".T.") + ")");
        //    //     //ATA record into temp table that created under the fix folder attachement 

        //    //     var parse = new VisualFoxpro.FoxApplication();
        //    //     string foxCommand = "use 'D:\\Tracking\\Fixes\\" + TNUM + "\\Attachments\\Sysfiles\\" + filename + "' Shared";
        //    //     System.IO.File.WriteAllText("D:\\ErrorHappenedHere.txt", test.ToString());

        //    //     parse.DoCmd(foxCommand);
        //    //     parse.DoCmd(test.ToString());
        //    //     foxCommand = "Close All";
        //    //     parse.DoCmd(foxCommand);
        //    //     var task = System.Diagnostics.Process.GetProcessById(parse.ProcessId);
        //    //     task.Close();

        //    //     foreach (string fname in Directory.GetFiles("D:\\tablememofields"))
        //    //     {
        //    //         System.IO.File.Delete(fname);
        //    //     }
        //    //     newinst.updatesystemfile("D:\\Sysfiles\\" + filename, destpath, modificationtype, tag);
        //    // }
        //    return true;
        //}

        //ATA test function 
        #endregion
        public Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments[] CreateAttachmentRecordforsys(AriaObjectShelve Object, string Path)
        {
            IList<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments> attach = new List<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments>();
            Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments atta = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments();
            atta.Source = "D:\\Aria4xp\\ARIA4XP\\ARIA4XP (R13)\\sysfiles\\";
            atta.Dest = "D:\\Tracking\\Fixes\\" + Path + "\\Attachments\\Sysfiles\\";
            atta.Type = "System";

            switch (Object.ObjectType.ObjectTypeID.ToUpper())
            {
                case "ARIAFIELD":
                    atta.Name = "SYDFIELD.DBF";
                    atta.Tag = "CFLD_NAME";
                    atta.cKeyExpression = "CFLD_NAME";
                    break;
                case "ARIAFILE":
                    atta.Name = "SYDFILES.DBF";
                    atta.Tag = "CFILE_NAM";
                    atta.cKeyExpression = "CFILE_NAM";
                    break;
                case "ARIAINDEX":
                    atta.Name = "SYDINDEX.DBF";
                    atta.Tag = "CFILE_NAM";
                    atta.cKeyExpression = "CFILE_NAM+CFILE_TAG";
                    break;
                case "ARIAOBJECT":
                    atta.Name = "SYDOBJCT.DBF";
                    atta.Tag = "CAPP_ID";
                    atta.cKeyExpression = "CAPP_ID+CAPOBJNAM";
                    break;
                case "ARIAREPORT":
                    atta.Name = "SYDREPRT.DBF";
                    atta.Tag = "CREP_ID";
                    atta.cKeyExpression = "CREP_ID";
                    break;
                case "OPTIONGRID":
                    atta.Name = "SYREPUVR.DBF";
                    atta.Tag = "CREP_ID";
                    atta.cKeyExpression = "CREP_ID+PADR(SUBSTR(MFLD_NAME,1,100),100,' ')";
                    break;
                case "ARIATRIGGER":
                    atta.Name = "SYCTRIGG.DBF";
                    atta.Tag = "OBJEVENT";
                    atta.cKeyExpression = "CAPOBJNAM+CEVENT_ID+STR(NTRIGORDER,3)";
                    break;
                case "MENU":
                    atta.Name = "SYCMENU.DBF";
                    atta.Tag = "MENUUQ";
                    atta.cKeyExpression = "UPPER(ALLTRIM(CAPP_ID))+UPPER(ALLTRIM(CPROSS_ID))+UPPER(ALLTRIM(CPROCTYPE))+UPPER(ALLTRIM(CMENUPARAM))";
                    break;
                //MMT
                case "FLFLD":
                    atta.Name = "SYDFLFLD.DBF";
                    atta.Tag = "CFLFLD";
                    atta.cKeyExpression = "CFILE_NAM+CFLD_NAME";
                    break;
                //MMT
                default:
                    atta = null;
                    break;
            }
            if (atta != null)
                attach.Add(atta);
            return attach.ToArray<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments>();
        }
        public Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments[] getattachmentsnotsystem(AriaObjectShelve Obj)
        {
            IList<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments> listofattachments = new List<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments>();
            Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments atta = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments();
            atta.Name = Obj.ObjectName;
            string subfolder = Obj.AriaObjectShelveSettings.FirstOrDefault(x => x.SettingType.SettingTypeId.ToUpper() == "SUBFOLDER").Value;
            int lindx = subfolder.IndexOf(")");
            string source = "";

            if (Obj.TrackingEntry.Application.Id.Contains("4"))
            {
                if (subfolder.ToUpper().Contains("ARIA"))
                    //MMT
                    //source = subfolder.Remove(0, lindx + 2);
                    source = subfolder.Remove(0, lindx + 1);
                //MMT
                if (string.IsNullOrEmpty(source))
                {
                    source = @"D:\Aria4xp\ARIA4XP\Aria4xp (R13)\" ;
                }
                else
                {
                    source = subfolder.Remove(0, lindx + 2);
                    source = @"D:\Aria4xp\ARIA4XP\Aria4xp (R13)\" + source + "\\";
                }
            }
            else
            {
                source = Obj.AriaObjectShelveSettings.FirstOrDefault(x => x.SettingType.SettingTypeId.ToUpper() == "SUBFOLDER").Value.Replace("$", @"D:") + "\\";
            }

            atta.Source = source;// Obj.AriaObjectShelveSettings.FirstOrDefault(x => x.SettingType.SettingTypeId.ToUpper() == "SUBFOLDER").Value.Replace("$",@"D:")+"\\";
            atta.Dest = Obj.AriaObjectShelveSettings.FirstOrDefault(x => x.SettingType.SettingTypeId.ToUpper() == "SUBFOLDER").Value + "\\";

            atta.Type = Obj.ObjectType.Name;
            listofattachments.Add(atta);
            //switch (Path.GetExtension(Obj.ObjectName))
            //{
            //    case ".SCX":
            //        ServiceReferenceTfsManager.Attachments sct = new ServiceReferenceTfsManager.Attachments();
            //        sct.Name = atta.Name.Replace(".SCX", ".SCT");
            //        sct.Source = atta.Source;
            //        sct.Dest = atta.Dest;
            //        sct.Type = atta.Type;
            //        listofattachments.Add(sct);
            //        break;
            //    case ".FRX":
            //         ServiceReferenceTfsManager.Attachments frt = new ServiceReferenceTfsManager.Attachments();
            //         frt.Name = atta.Name.Replace(".FRX", ".FRT");
            //       frt.Source = atta.Source;
            //       frt.Dest = atta.Dest;
            //       frt.Type = atta.Type;
            //        listofattachments.Add(frt);
            //        break;
            //    case ".VCX":
            //        ServiceReferenceTfsManager.Attachments vct = new ServiceReferenceTfsManager.Attachments();
            //        vct.Name = atta.Name.Replace(".VCX", ".VCT");
            //        vct.Source = atta.Source;
            //        vct.Dest = atta.Dest;
            //        vct.Type = atta.Type;
            //        listofattachments.Add(vct);
            //        break;
            //    case ".DBF":
            //       ServiceReferenceTfsManager.Attachments fpt = new ServiceReferenceTfsManager.Attachments();
            //        fpt.Name = atta.Name.Replace(".DBF", ".FPT");
            //        fpt.Source = atta.Source;
            //        fpt.Dest = atta.Dest;
            //        fpt.Type = atta.Type;
            //        listofattachments.Add(fpt);
            //       ServiceReferenceTfsManager.Attachments cdx = new ServiceReferenceTfsManager.Attachments();
            //        cdx.Name = atta.Name.Replace(".DBF", ".CDX");
            //        cdx.Source = atta.Source;
            //        cdx.Dest = atta.Dest;
            //        cdx.Type = atta.Type;
            //        listofattachments.Add(cdx);
            //        break;
            //    case ".LBX":
            //        ServiceReferenceTfsManager.Attachments LBT = new ServiceReferenceTfsManager.Attachments();
            //        LBT.Name = atta.Name.Replace(".LBX", ".LBT");
            //        LBT.Source = atta.Source;
            //        LBT.Dest = atta.Dest;
            //        LBT.Type = atta.Type;
            //        listofattachments.Add(LBT);
            //        break;
            //}

            return listofattachments.ToArray<Aria5SystemAdmin.Module.ServiceReferenceTfsManager.Attachments>();
        }
        private void StartThisTracking_Execute(object sender, SimpleActionExecuteEventArgs e)
        {

            IObjectSpace ospace = Application.CreateObjectSpace();
            TrackingEntry TE = ospace.FindObject<TrackingEntry>(CriteriaOperator.Parse("[Oid] = '" + ((TrackingEntry)View.CurrentObject).Oid + "'"));
            //MMT
            //foreach (AriaObjectShelve selectAriaObject in TE.AriaObjectShelves)
            //{
            //    AriaObject selectedAriaObject = ospace.GetObjectByKey<AriaObject>(selectAriaObject.AriaObject.Oid);
            //    IList<AriaObjectShelve> ExistsShelve = ospace.GetObjects<AriaObjectShelve>(CriteriaOperator.Parse("[AriaObject] ='" + selectedAriaObject.Oid + "'"));

            //    int indexofte = ExistsShelve.FindIndex(x => x.TrackingEntry != null && x.TrackingEntry.ID != TE.ID && (x.TrackingEntry.Status != TrackingEntry.TrackingStatus.New && x.TrackingEntry.Status != TrackingEntry.TrackingStatus.Complete && x.TrackingEntry.Status != TrackingEntry.TrackingStatus.Cancelled));
            //    if (indexofte >= 0)
            //    {
            //        throw new Exception("The '" + selectedAriaObject.ObjectName + "' is already linked to a Traking Entry Number'" + ExistsShelve[indexofte].TrackingEntry.ID + "' and it's status is " + ExistsShelve[indexofte].TrackingEntry.Status + "' So you can't start this tracking Entry");
            //    }
            //}
            //MMT
            CheckTestplans(TE);
            CheckTestCases(TE);
            // Doaa 05/01/2019 {start}
            if (TE.TrackingTasks != null)
            {
                if (TE.TrackingTasks.Count > 0)
                {
                    if(TE.TrackingTasks.Where(t => t.Task.Name == "Access Source Files").Count() ==  0)
                    {
                        throw new Exception("Can't start this tracking without Access Source Files task");
                    }
                }
                else
                    throw new Exception("Can't start this tracking without scheduling");
            }
            else
                throw new Exception("Can't start this tracking without scheduling");
            // Doaa 05/01/2019 {End}
            Addfilestotfs(TE, CheckAriaobjectshelvesavilabilty(TE));
            TE.Status = TrackingEntry.TrackingStatus.InWork;
            TE.Save();
            ospace.CommitChanges();
            //MMT
            TfsManager tfs = new TfsManager();
            string fileName = "", filePath = "", fullPath = "";
            if (this.View.CurrentObject != null && View.CurrentObject is TrackingEntry)
            {
                TrackingTask task = ((TrackingEntry)this.View.CurrentObject).TrackingTasks.Where(x => x.Task.CheckOut == true && x.Status == TrackingTask.StatusTypes.New).FirstOrDefault();
                if (task != null)
                {
                    string assignedSource = task.Resources.Name;
                    //ATA convert the status to inwork 
                    task.Status = TrackingTask.StatusTypes.InWork;
                    foreach (AriaObjectShelve objectShe in ((TrackingEntry)View.CurrentObject).AriaObjectShelves)
                    {
                        if (objectShe.AriaObjectShelveSettings.Count > 0)
                        {
                            AriaObjectShelveSetting Storagefilenametype = objectShe.AriaObjectShelveSettings.Where(x => x.SettingType.SettingTypeId == "StorageFileName").FirstOrDefault();
                            if (Storagefilenametype != null)
                            {
                                fileName = Storagefilenametype.Value;
                            }
                            AriaObjectShelveSetting Subfoldertype = objectShe.AriaObjectShelveSettings.Where(x => x.SettingType.SettingTypeId == "SubFolder").FirstOrDefault();
                            if (Subfoldertype != null)
                            {
                                filePath = Subfoldertype.Value;
                                //MMT
                                if (filePath.ToUpper().Contains("\\Aria4XP\\Aria4XP\\".ToUpper()))
                                {
                                    filePath = filePath.ToUpper().Replace("\\Aria4XP\\Aria4XP\\".ToUpper (), "\\Aria4XP\\".ToUpper());
                                }
                                //MMT

                            }
                            fullPath = filePath + "\\" + fileName;
                        }
                        //ATA start 8/14/2017 old check out method 
                        // tfs.GrantCheckoutFilePermission(fullPath, assignedSource);
                        //ATA new method to handle aria4xp files type 
                        if (!string.IsNullOrEmpty(filePath))
                            Grantcheckoutfile(fullPath, assignedSource, tfs);
                        //ATA end  8/14/2017  
                        objectShe.CodeStatus = AriaObjectShelve.Status.CheckedOut;
                        objectShe.Save();
                    }

                    //  tfs.GrantCheckoutFilePermission(fullPath, assignedSource);
                    //MMT
                    //((TrackingEntry)this.View.CurrentObject).CheckInComment = ((TrackingEntry)this.View.CurrentObject).TicketNumber + " - " + ((TrackingEntry)this.View.CurrentObject).ID.ToString() + " - " + assignedSource;
                    ((TrackingEntry)this.View.CurrentObject).CheckInComment = ((TrackingEntry)this.View.CurrentObject).ReferenceNo + " - " + ((TrackingEntry)this.View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString() + " - " + assignedSource;
                    //MMT
                    Application.ShowViewStrategy.ShowMessage("The Permission is Granted for the user " + assignedSource + "", InformationType.Success);
                }
            }
            //MMT
        }

        private List<string> CheckAriaobjectshelvesavilabilty(TrackingEntry TE)
        {
            //MMT
            //(x.TrackingEntry.Status != TrackingEntry.TrackingStatus.New && x.TrackingEntry.Status != TrackingEntry.TrackingStatus.Complete && x.TrackingEntry.Status != TrackingEntry.TrackingStatus.Cancelled)
            List<string> AddedFilePathes = new List<string>();
            IObjectSpace ospace = Application.CreateObjectSpace();
            foreach (AriaObjectShelve Shelve in TE.AriaObjectShelves)
            {
                //ATA  exclude Ariaobjectshelves that related to another open tracking entry 
                if (Shelve.ModificationType != AriaObjectShelve.ModificationTypes.Add)
                {
                    IList<AriaObjectShelve> ExistsShelve = ospace.GetObjects<AriaObjectShelve>(CriteriaOperator.Parse("[AriaObject] ='" + Shelve.AriaObject.Oid + "' and TrackingEntry is not null and TrackingEntry != '" + TE.Oid + "'"));
                    //MMT 
                    //int indexofte = ExistsShelve.FindIndex(x => x.TrackingEntry.Status != TrackingEntry.TrackingStatus.ReleaseCandidate && x.TrackingEntry.Status != TrackingEntry.TrackingStatus.New);
                    int indexofte = ExistsShelve.FindIndex(x => x.TrackingEntry.Status != TrackingEntry.TrackingStatus.Complete  && x.TrackingEntry.Status != TrackingEntry.TrackingStatus.New && x.TrackingEntry.Status != TrackingEntry.TrackingStatus.Cancelled);
                    //MMT
                    if (indexofte >= 0)
                    {
                        throw new Exception("The '" + Shelve.AriaObject.ObjectName + "' is already linked to a Traking Entry Number'" + ExistsShelve[indexofte].TrackingEntry.ID + "' and it's status is " + ExistsShelve[indexofte].TrackingEntry.Status + "' So you can't start the tracking");
                    }
                }
                else
                {
                    string path = Shelve.AriaObjectShelveSettings.FirstOrDefault(s => s.SettingType.SettingTypeId == "SubFolder").Value.Replace("$", @"D:\") + "\\";

                    int lindx = path.IndexOf(")");
                    string source = "";
                    if (path.ToUpper().Contains("ARIA"))
                        source = path.Remove(0, lindx + 2);
                    source = @"D:\Aria4xp\ARIA4XP\Aria4xp (R13)\" + source + "\\" + Shelve.AriaObjectShelveSettings.FirstOrDefault(s => s.SettingType.SettingTypeId == "StorageFileName").Value;
                    AddedFilePathes.Add(source);
                }

            }
            return AddedFilePathes;
        }
        private void Addfilestotfs(TrackingEntry TE, List<string> AddedFilePathes)
        {
            TrackingTask sourcecodetask = TE.TrackingTasks.Where(T => T.Task.CheckOut == true).FirstOrDefault();
            string assignedSource = "";
            if (sourcecodetask != null && sourcecodetask.Resources != null)
            {
                assignedSource = sourcecodetask.Resources.Name;
            }
            Aria5SystemAdmin.Module.ServiceReferenceTfsManager.WebService1SoapClient tfs = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.WebService1SoapClient();
            tfs.InnerChannel.OperationTimeout = new TimeSpan(1, 0, 0);
            foreach (var fullPath in AddedFilePathes)
            {
                tfs.addnewfile(fullPath, TE.Application.Id, assignedSource);
                switch (Path.GetExtension(fullPath).ToUpper())
                {
                    case ".SCX":
                        string sct = fullPath.ToUpper().Replace(".SCX", ".SCT");
                        tfs.addnewfile(sct, TE.Application.Id, assignedSource);
                        break;
                    case ".FRX":
                        string frt = fullPath.ToUpper().Replace(".FRX", ".FRT");
                        tfs.addnewfile(frt, TE.Application.Id, assignedSource);
                        break;
                    case ".VCX":
                        string vct = fullPath.ToUpper().Replace(".VCX", ".VCT");
                        tfs.addnewfile(vct, TE.Application.Id, assignedSource);
                        break;
                    case ".DBF":
                        string fpt = fullPath.ToUpper().Replace(".DBF", ".FPT");
                        tfs.addnewfile(fpt, TE.Application.Id, assignedSource);

                        string cdx = fullPath.ToUpper().Replace(".DBF", ".CDX");
                        tfs.addnewfile(cdx, TE.Application.Id, assignedSource);
                        break;
                    case ".LBX":
                        string LBT = fullPath.ToUpper().Replace(".LBX", ".LBT");
                        tfs.addnewfile(LBT, TE.Application.Id, assignedSource);
                        break;
                }
            }
        }
        private void CheckTestplans(TrackingEntry TE)
        {
            if (TE.ProjectTemplate != null)
            {
                if ((TE.HasChildren == true && TE.TestPlans.Count > 0) || (TE.HasChildren == false && TE.ParentTrackingEntry == null && TE.TestPlans.Count > 0) || (TE.HasChildren == false && TE.ParentTrackingEntry != null && (TE.TestPlans.Count > 0 || TE.ParentTrackingEntry.TestPlans.Count > 0)))
                {

                }
                else
                {
                    throw new Exception("Can't start the tracking entry because there are no Test plan for it please refer to quality team to generate a test plan for it");
                }
            }
            else
            {
                throw new Exception("can't start this tracking because it is not related to any projects");
            }

        }
        private void CheckTestCases(TrackingEntry TE)
        {
            foreach (QATestPlan Tpl in TE.TestPlans)
            {
                var testtypes = Enum.GetNames(typeof(QATestPlan.Testtype));
                foreach (var testtype in testtypes)
                {
                    if ((bool)Tpl.GetMemberValue(testtype))
                    {
                        int tcscount = TE.TestCases.Where(t => t.IsOriginal == true && t.TestcaseType == ((QATestPlan.Testtype)Enum.Parse(typeof(QATestPlan.Testtype), testtype))).Count();
                        if (TE.HasChildren && tcscount == 0)
                        {
                            foreach (TrackingEntry ChildTE in TE.ChildTrackingEntries)
                            {
                                tcscount += ChildTE.TestCases.Where(t => t.IsOriginal == true && t.TestcaseType == ((QATestPlan.Testtype)Enum.Parse(typeof(QATestPlan.Testtype), testtype))).Count();
                            }
                        }
                        if (tcscount == 0)
                        {
                            throw new Exception("This tracking entry can't be started because the test Type '" + testtype + "' that mentioned in test plan is not coverd by test cases");
                        }
                    }
                }
                var testlevels = Enum.GetNames(typeof(QATestPlan.Testlevels));
                foreach (var testlevel in testlevels)
                {
                    if ((bool)Tpl.GetMemberValue(testlevel))
                    {
                        int tcscount = TE.TestCases.Where(t => t.IsOriginal == true && t.TestCaselevel == ((QATestPlan.Testlevels)Enum.Parse(typeof(QATestPlan.Testlevels), testlevel))).Count();
                        //ATA check that if the children of the tracking havee the test cases that required to match the test plan 
                        if (TE.HasChildren && tcscount == 0)
                        {
                            foreach (TrackingEntry ChildTE in TE.ChildTrackingEntries)
                            {
                                tcscount += TE.TestCases.Where(t => t.IsOriginal == true && t.TestCaselevel == ((QATestPlan.Testlevels)Enum.Parse(typeof(QATestPlan.Testlevels), testlevel))).Count();
                            }
                        }
                        if (tcscount == 0)
                        {
                            throw new Exception("This tracking entry can't be started because the test level '" + testlevel + "' that mentioned in test plan is not coverd by test cases");
                        }
                    }
                }
                if (!Tpl.SystemTest && !Tpl.ComponentTest && !Tpl.AcceptanceTest && !Tpl.IntegrationTest)
                {
                    throw new Exception("you can't save test plane without select one Level at least");
                }
            }
        }
        private void ScheduleThisTracking_Execute(object sender, SimpleActionExecuteEventArgs e)
        {

            IObjectSpace ospace = Application.CreateObjectSpace();
            TrackingEntry TE = ospace.FindObject<TrackingEntry>(CriteriaOperator.Parse("[Oid] = '" + ((TrackingEntry)View.CurrentObject).Oid + "'"));
            if (TE.HasChildren == true)
            {
                ScheduleTracking(TE, ospace);
                foreach (TrackingEntry Tracking in TE.ChildTrackingEntries)
                {
                    if (Tracking.ProjectTemplate != null)//&& Tracking.ProjectEntity != null)
                    {
                        if (Tracking.TrackingTasks.Count == 0)
                        {
                            ScheduleTracking(Tracking, ospace);
                        }
                        else
                        {
                            throw new Exception("Please delete all previous tasks to be able to use this template");
                        }
                    }
                    else
                    {
                        throw new Exception("The child tracking entry number '" + Tracking.ID + "' is not linked to project or project entity please check");
                    }
                }
            }
            else if (TE.ProjectTemplate != null) //&& TE.ProjectEntity != null)
            {
                if (TE.TrackingTasks.Count == 0)
                {
                    ScheduleTracking(TE, ospace);
                }
                else
                {
                    throw new Exception("Please delete all previous tasks to be able to use this template");
                }

            }
            else
            {
                throw new Exception("can't start this tracking because it is not related to any projects");
            }
        }

        private void ScheduleTracking(TrackingEntry TE, IObjectSpace ospace)
        {

            for (int i = 0; i < 4; i++)
            {
                TrackingTask task = ospace.CreateObject<TrackingTask>();
                switch (i)
                {
                    case 0:
                        task.Task = ospace.FindObject<Activity>(CriteriaOperator.Parse("[Name] = 'Testing Requirements'"));
                        break;
                    case 1:
                        task.Task = ospace.FindObject<Activity>(CriteriaOperator.Parse("[Name] = 'Access Source Files'"));
                        break;
                    case 2:
                        if (TE.HasChildren == true)
                            continue;
                        task.Task = ospace.FindObject<Activity>(CriteriaOperator.Parse("[Name] = 'Programming'"));
                        break;
                    case 3:
                        task.Task = ospace.FindObject<Activity>(CriteriaOperator.Parse("[Name] = 'Cross Testing'"));
                        break;
                }

                task.StartDate = TE.EnteredDate;
                task.EndDate = TE.RequestedDate;
                if (TE.ProjectTemplate.Type == ProjectTemplate.ProjectType.Key)
                {
                    if (TE.ProjectTemplate.ProjectWBS.Count > 0)
                    {
                        task.WBSMonth = TE.ProjectTemplate.ProjectWBS.Where(x => x.Month == 3).FirstOrDefault();
                    }
                    else if (TE.ProjectTemplate.UseCasePoints != null && TE.ProjectTemplate.UseCasePoints.WBS.Count > 0)
                    {
                        task.WBSMonth = TE.ProjectTemplate.ProjectWBS.Where(x => x.Month == 1).FirstOrDefault();
                    }
                    else
                    {
                        throw new Exception("the linked project didn't have any phases");
                    }
                }
                else
                {
                    //add parent and child mechanism 
                    if (TE.ParentTrackingEntry == null)
                    {
                        QAWBS wbs = TE.ProjectTemplate.ProjectWBS.Where(w => w.PhaseTitle == (TE.ID + " - " + TE.ReferenceNo)).FirstOrDefault();
                        if (wbs != null)
                        {
                            task.WBSMonth = wbs;
                        }
                        else
                        {
                            QAWBS qawbs = new QAWBS(TE.Session);
                            qawbs.Project = TE.ProjectTemplate;

                            AutoTaskIntegrationManger autoTaskIntegrationManger = new AutoTaskIntegrationManger();
                            autoTaskIntegrationManger.CreateNewPhasenew(TE.ProjectTemplate.AutoTaskID, qawbs);
                            throw new Exception("Please create new phase under this project with the name of " + TE.ID + " - " + TE.ReferenceNo + " to create tasks under it");
                        }
                    }
                    else
                    {
                        QAWBS wbs = TE.ProjectTemplate.ProjectWBS.Where(w => w.PhaseTitle == (TE.ParentTrackingEntry.ID + " - " + TE.ParentTrackingEntry.ReferenceNo)).FirstOrDefault();
                        if (wbs != null)
                        {
                            task.WBSMonth = wbs;
                            task.WBSActivity = wbs.QAActivities.Where(a => a.Activity == (TE.ID + " - " + TE.ReferenceNo)).FirstOrDefault();
                        }
                        else
                        {
                            throw new Exception("Please create new phase under this project with the name of " + TE.ID + " - " + TE.ReferenceNo + " to create tasks under it");
                        }
                    }

                }

                task.TrackingEntry = TE;
                task.Save();
                //ospace.CommitChanges();
            }
            TE.Save();
            

            ospace.CommitChanges();
        }

        private void CreateBuild_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            if (this.View.CurrentObject != null && this.View.CurrentObject is TrackingEntry && ((TrackingEntry)(this.View.CurrentObject)).Type == TrackingEntry.TrackingType.TBuild)
            {

                Aria5SystemAdmin.Module.ServiceReferenceTfsManager.WebService1SoapClient CreateFixWebServiceClient = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.WebService1SoapClient();
                CreateFixWebServiceClient.InnerChannel.OperationTimeout = new TimeSpan(1, 0, 0);
                IObjectSpace ospace = Application.CreateObjectSpace();
                TrackingEntry TrackingEntry = ospace.FindObject<TrackingEntry>(CriteriaOperator.Parse("[Oid] = '" + ((TrackingEntry)View.CurrentObject).Oid + "'"));
                //DM -MMT
                //if (CreateEnvironment(CreateFixWebServiceClient, TrackingEntry, true) == false)
                //{
                //    throw new Exception("Can't Access the shelve with this tracking number so we can't proceed in creating fix");
                //}
                string exceptionmsg = "";
                if (CreateEnvironment(CreateFixWebServiceClient, TrackingEntry, true, out exceptionmsg) == false)
                {
                    throw new Exception(exceptionmsg);
                }
                //DM - MMT
                string PName = TrackingEntry.Application.Id;
                if (!PName.Contains("4"))
                {
                    PName = "EDI";
                }

                if (CraeteAttachementfolder(CreateFixWebServiceClient, TrackingEntry, PName))
                {
                    if (CreateFixWebServiceClient.createfix(((TrackingEntry)this.View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString(), PName))
                    {
                        string fixname = ((TrackingEntry)this.View.CurrentObject).Type.ToString() + ((TrackingEntry)this.View.CurrentObject).BuildNo.ToString();
                        TrackingEntry.TrackingFix = "ftp://192.168.1.171/tracking/" + fixname + "/" + fixname + ".EXE";
                        TrackingEntry.Save();
                        ospace.CommitChanges();
                        Application.ShowViewStrategy.ShowMessage("Build Number '" + ((TrackingEntry)View.CurrentObject).Type.ToString().Substring(0, 1) + ((TrackingEntry)this.View.CurrentObject).ID.ToString() + "' is succefully created");
                    }
                }
                else
                {
                    throw new Exception("Faild to create attachment folder ");
                }
            }
        }

    }
}
