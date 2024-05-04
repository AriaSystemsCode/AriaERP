using System;
using System.ComponentModel;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Actions;
using DevExpress.Persistent.Base;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Data.Filtering;

namespace Aria5SystemAdmin.Module.Controllers
{
    public partial class AriaObjectViewController : ViewController
    {
        public AriaObjectViewController()
        {
            InitializeComponent();
            RegisterActions(components);
        }

        private void AssignNewRevision_Execute(object sender, SimpleActionExecuteEventArgs e)
        {

            EntityOperationPermission x = new EntityOperationPermission(((AriaObject)e.CurrentObject).Session);
            x.Save();

            if (e.CurrentObject != null) ((AriaObject)e.CurrentObject).AssignNewRevision();
        }

        private void CreateTrackingEntry_Execute(object sender, SimpleActionExecuteEventArgs e)
        {
            IObjectSpace ospace = Application.CreateObjectSpace();
            TrackingEntry newentry = ospace.CreateObject<TrackingEntry>();
            foreach (var item in this.View.SelectedObjects)
            {
                AriaObject selectedAriaObject = ospace.FindObject<AriaObject>(CriteriaOperator.Parse("[Oid]= '"+((AriaObject)item).Oid+"'"));
                IList<AriaObjectShelve> ExistsShelve = ospace.GetObjects<AriaObjectShelve>(CriteriaOperator.Parse("[AriaObject] ='" + selectedAriaObject.Oid + "' and TrackingEntry is not null"));
                int indexofte = ExistsShelve.FindIndex(x => x.TrackingEntry.Status != TrackingEntry.TrackingStatus.ReleaseCandidate && x.TrackingEntry.Status != TrackingEntry.TrackingStatus.New);
                if (indexofte >= 0)
                {
                    throw new Exception("The '" + selectedAriaObject.ObjectName + "' is already linked to a Traking Entry Number'" + ExistsShelve[indexofte].TrackingEntry.ID + "' and it's status is " + ExistsShelve[indexofte].TrackingEntry.Status + "' So you can't include in your traking Entry");
                }
                newentry.Application = selectedAriaObject.ParentObjectID.Application;
                newentry.Entity = selectedAriaObject.ParentObjectID;
                AriaObjectShelve she = ospace.CreateObject<AriaObjectShelve>();
                she.AriaObject = she.Session.FindObject<AriaObject>(CriteriaOperator.Parse("Oid ='" + selectedAriaObject.Oid + "'"));
                she.ObjectDescription = selectedAriaObject.ObjectDescription;
                she.ObjectID = (int)selectedAriaObject.ObjectID;
                she.ObjectName = selectedAriaObject.ObjectName;
                she.ModificationType = AriaObjectShelve.ModificationTypes.Modify;
                she.ObjectType = she.Session.FindObject<ObjectType>(CriteriaOperator.Parse("Oid ='" + selectedAriaObject.ObjectType.Oid + "'"));
                she.TrackingEntry = she.Session.GetObjectByKey<TrackingEntry>(TrackingEntryViewController.currentriaTrakingEntry);

                newentry.AriaObjectShelves.Add(she);

            }
            newentry.Save();
            DetailView DV = Application.CreateDetailView(ospace, newentry);
           DV.ViewEditMode = DevExpress.ExpressApp.Editors.ViewEditMode.Edit;
           Application.ShowViewStrategy.ShowViewInPopupWindow(DV);
         //  Application.ShowViewStrategy.ShowMessage("created Succefully");
            //ObjectSpace.CommitChanges();
        }

        private void CreateChildTrackingEntry_Execute(object sender, ParametrizedActionExecuteEventArgs e)
        {
            IObjectSpace ospace = Application.CreateObjectSpace();
            TrackingEntry newentry = ospace.CreateObject<TrackingEntry>();
            newentry.ParentTrackingEntry = ospace.FindObject<TrackingEntry>(CriteriaOperator.Parse("[id] = '" + e.ParameterCurrentValue + "'"));
            if (newentry.ParentTrackingEntry == null)
            {
                throw new Exception("Please Enter a valid parent Tracking Entry ID");
            }
            foreach (var item in this.View.SelectedObjects)
            {
                AriaObject selectedAriaObject = ospace.FindObject<AriaObject>(CriteriaOperator.Parse("[Oid]= '" + ((AriaObject)item).Oid + "'"));
                IList<AriaObjectShelve> ExistsShelve = ospace.GetObjects<AriaObjectShelve>(CriteriaOperator.Parse("[AriaObject] ='" + selectedAriaObject.Oid + "'"));
                int indexofte = ExistsShelve.FindIndex(x => x.TrackingEntry != null && (x.TrackingEntry.Status != TrackingEntry.TrackingStatus.ReleaseCandidate || x.TrackingEntry.Status != TrackingEntry.TrackingStatus.New));
                if (indexofte >= 0)
                {
                    throw new Exception("The '" + selectedAriaObject.ObjectName + "' is already linked to a Traking Entry Number'" + ExistsShelve[indexofte].TrackingEntry.ID + "' and it's status is " + ExistsShelve[indexofte].TrackingEntry.Status + "' So you can't include in your traking Entry");
                }
                newentry.Application = selectedAriaObject.ParentObjectID.Application;
                newentry.Entity = selectedAriaObject.ParentObjectID;
                AriaObjectShelve she = ospace.CreateObject<AriaObjectShelve>();
                she.AriaObject = she.Session.FindObject<AriaObject>(CriteriaOperator.Parse("Oid ='" + selectedAriaObject.Oid + "'"));
                she.ObjectDescription = selectedAriaObject.ObjectDescription;
                she.ObjectID = (int)selectedAriaObject.ObjectID;
                she.ObjectName = selectedAriaObject.ObjectName;
                she.ModificationType = AriaObjectShelve.ModificationTypes.Modify;
                she.ObjectType = she.Session.FindObject<ObjectType>(CriteriaOperator.Parse("Oid ='" + selectedAriaObject.ObjectType.Oid + "'"));
                she.TrackingEntry = she.Session.GetObjectByKey<TrackingEntry>(TrackingEntryViewController.currentriaTrakingEntry);

                newentry.AriaObjectShelves.Add(she);

            }
            newentry.Save();
            DetailView DV = Application.CreateDetailView(ospace, newentry);
            DV.ViewEditMode = DevExpress.ExpressApp.Editors.ViewEditMode.Edit;
            Application.ShowViewStrategy.ShowViewInPopupWindow(DV);
        }

    }
}
