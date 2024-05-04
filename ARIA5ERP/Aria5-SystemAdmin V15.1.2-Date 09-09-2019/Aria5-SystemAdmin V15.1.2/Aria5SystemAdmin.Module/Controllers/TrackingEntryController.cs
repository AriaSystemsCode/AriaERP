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

namespace Aria5SystemAdmin.Module.Controllers
{
    // For more typical usage scenarios, be sure to check out http://documentation.devexpress.com/#Xaf/clsDevExpressExpressAppViewControllertopic.
    public partial class TrackingEntryController : ViewController
    {
        public static Guid currentriaTrakingEntry;

        public TrackingEntryController()
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

        private void TrackingEntryController_Activated(object sender, EventArgs e)
        {
            if (this.View.CurrentObject != null && this.View.CurrentObject.GetType() == typeof(TrackingEntry))
            {

                currentriaTrakingEntry = ((TrackingEntry)this.View.CurrentObject).Oid;
            }
        }

        private void TrackingEntry_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            TrackingEntry trackingEntry = objectSpace.GetObjectByKey<TrackingEntry>(currentriaTrakingEntry);
            AriaObjectShelve ariaObjectShelve = new AriaObjectShelve(trackingEntry.Session);

            XPCollection allTrackingAriaObjectShelve = new XPCollection(trackingEntry.Session, typeof(AriaObjectShelve), CriteriaOperator.Parse("[TrackingEntry] = '" + trackingEntry.Oid + "'"));

            foreach (AriaObjectShelve item in allTrackingAriaObjectShelve)
            {

                //     ariaObject.IncrementRevision();

            //    ariaObject.AriaObjectRevisions.Add(rev);
                AddNewProperties(item, item.AriaObject);
                //AddNewMethods(item, item.AriaObject);
                //AddNewEvents(item, item.AriaObject);
            }

            //change state to checked in or checked out
            trackingEntry.State= "CheckedIn ";

            //change status to compelete 
            trackingEntry.Status = ((BusinessObjects.TrackingEntry.TrackingStatus)Enum.Parse(typeof(BusinessObjects.TrackingEntry.TrackingStatus), BusinessObjects.TrackingEntry.TrackingStatus.Complete.ToString()));
            trackingEntry.CompleteDate = DateTime.Today;


            trackingEntry.ActiveRevision = (Convert.ToInt32(trackingEntry.Session.FindObject<AriaObject>(CriteriaOperator.Parse("Oid ='" + trackingEntry.Entity.Oid + "'")).ActiveRevision) + 1).ToString();

            trackingEntry.Save();

            trackingEntry.Session.CommitTransaction();
        }

        public void AddNewProperties(AriaObjectShelve objectShelve, AriaObject ariaObject)
        {
            IObjectSpace objectSpace = Application.CreateObjectSpace();

            TrackingEntry trackingEntry = objectSpace.GetObjectByKey<TrackingEntry>(currentriaTrakingEntry);

            foreach (var objectProperty in objectShelve.AriaObjectShelveProperties)
            {
                var prop = new AriaObjectProperties(trackingEntry.Session);

                prop.PropertyName = objectProperty.PropertyName;
                prop.PropertyDescription = objectProperty.PropertyDescription;
                prop.PropertyType = prop.Session.FindObject<AriaObjectShelveProperty>(CriteriaOperator.Parse("Oid ='" + objectProperty.Oid + "'")).PropertyType;
                prop.Required = objectProperty.Required;
                prop.ModificationType = ((AriaObjectProperties.modificationType)objectProperty.ModificationType);



                foreach (var shelvePropSetting in objectProperty.AriaObjectShelvePropertySettings)
                {
                    var PropSetting = new AriaObjectPropertiesSettings(trackingEntry.Session);

                    PropSetting.DataType = shelvePropSetting.DataType;
                    PropSetting.DecimalPlaces = shelvePropSetting.DecimalPlaces;
                    //PropSetting.Format = shelvePropSetting.Format;
                    //PropSetting.NowLocked = shelvePropSetting.NowLocked;
                    PropSetting.Modified = shelvePropSetting.Modified;
                    PropSetting.SettingType = shelvePropSetting.SettingType;
                    //PropSetting.Validvalues = shelvePropSetting.ValidValues;
                    PropSetting.Value = shelvePropSetting.Value;
                    PropSetting.Width = shelvePropSetting.Width;

                    prop.AriaObjectPropertiesSettingses.Add(PropSetting);
                }

                ariaObject.AriaObjectPropertieses.Add(prop);
            }


            ariaObject.Save();

        }


        //public void AddNewMethods(AriaObjectShelve objectShelve, AriaObject ariaObject)
        //{

        //    foreach (var shelveMethod in objectShelve.AriaObjectShelveMethods)
        //    {
        //        var method = new AriaObjectMethod(currentriaTrakingEntry.Session);

        //        method.MethodName = shelveMethod.MethodName;
        //        method.MethodDescription = shelveMethod.MethodDescription;
        //        method.ModificationType = ((AriaObjectMethod.modificationType)shelveMethod.ModificationType);
        //        method.NowLocked = shelveMethod.NowLocked;
        //        method.AriaObject = ariaObject.Session.FindObject<AriaObject>(CriteriaOperator.Parse("Oid ='" + shelveMethod.AriaObjectShelve.Oid + "'"));





        //        foreach (var shelveMethodParam in shelveMethod.AriaObjectShelveMethodParameters)
        //        {
        //            var shelveParam = new AriaObjectShelveMethodParameter(currentriaTrakingEntry.Session);

        //            shelveParam.ParameterName = shelveMethodParam.ParameterName;
        //            shelveParam.ParameterNo = shelveMethodParam.ParameterNo;
        //            shelveParam.ParameterType = shelveMethodParam.ParameterType;
        //            shelveParam.ModificationType = shelveMethodParam.ModificationType;
        //            shelveParam.NowLocked = shelveMethodParam.NowLocked;


        //            foreach (var shelveMethodParamSettings in shelveMethodParam.AriaObjectShelveMethodParameterSettings)
        //            {
        //                var shelveParamSetting = new AriaObjectShelveMethodParameterSettings(currentriaTrakingEntry.Session);

        //                shelveParamSetting.DataType = shelveMethodParamSettings.DataType;
        //                shelveParamSetting.DecimalPlaces = shelveMethodParamSettings.DecimalPlaces;
        //                shelveParamSetting.Format = shelveMethodParamSettings.Format;
        //                shelveParamSetting.ModifiedType = shelveMethodParamSettings.ModifiedType;
        //                shelveParamSetting.NowLocked = shelveMethodParamSettings.NowLocked;
        //                shelveParamSetting.SettingType = shelveMethodParamSettings.SettingType;
        //                shelveParamSetting.UpdatedToActual = shelveMethodParamSettings.UpdatedToActual;
        //                shelveParamSetting.ValidValues = shelveMethodParamSettings.ValidValues;
        //                shelveParamSetting.Value = shelveMethodParamSettings.Value;
        //                shelveParamSetting.Width = shelveMethodParamSettings.Width;
        //            }
        //        }


        //        ariaObject.AriaObjectRevisions.Last().AriaObjectMethods.Add(method);
        //    }

        //    ariaObject.Save();

        //}

        //public void AddNewEvents(AriaObjectShelve objectShelve, AriaObject ariaObject)
        //{

        //    foreach (var shelveEvent in objectShelve.AriaObjectShelveEvents)
        //    {
        //        var objectEvent = new AriaObjectEvent(currentriaTrakingEntry.Session);

        //        objectEvent.EventDescription = shelveEvent.EventDescription;
        //        objectEvent.EventName = shelveEvent.EventName;
        //        objectEvent.ModificationType = ((AriaObjectEvent.modificationType)shelveEvent.ModificationType);
        //        objectEvent.NowLocked = shelveEvent.NowLocked;
        //        objectEvent.AriaObject = ariaObject.Session.FindObject<AriaObject>(CriteriaOperator.Parse("Oid ='" + shelveEvent.AriaObjectShelve.Oid + "'"));


        //        foreach (var shelveEventParam in objectEvent.AriaObjectEventParameters)
        //        {
        //            var shelveParam = new AriaObjectEventParameter(currentriaTrakingEntry.Session);

        //            shelveParam.ParameterName = shelveEventParam.ParameterName;
        //            shelveParam.ParameterNo = shelveEventParam.ParameterNo;
        //            shelveParam.ParameterType = shelveEventParam.ParameterType;
        //            shelveParam.ModificationType = shelveEventParam.ModificationType;
        //            shelveParam.NowLocked = shelveEventParam.NowLocked;

        //            foreach (var shelveEventParamSettings in shelveEventParam.AriaObjectEventParameterSettingses)
        //            {
        //                var shelveParamSetting = new AriaObjectEventParameterSettings(currentriaTrakingEntry.Session);

        //                shelveParamSetting.DataType = shelveEventParamSettings.DataType;
        //                shelveParamSetting.DecimalPlaces = shelveEventParamSettings.DecimalPlaces;
        //                shelveParamSetting.Format = shelveEventParamSettings.Format;
        //                shelveParamSetting.NowLocked = shelveEventParamSettings.NowLocked;
        //                shelveParamSetting.SettingType = shelveEventParamSettings.SettingType;
        //                shelveParamSetting.Value = shelveEventParamSettings.Value;
        //                shelveParamSetting.Width = shelveEventParamSettings.Width;
        //            }
        //        }


        //        ariaObject.AriaObjectRevisions.Last().AriaObjectEvents.Add(objectEvent);
        //    }


        //    ariaObject.Save();

        //}

    }

}