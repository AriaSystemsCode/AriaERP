using Aria5SystemAdmin.Module.Controllers;
using DevExpress.Persistent.Validation;
using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class AriaObjectShelveEvent
    {
        public enum ModificationTypes
        {
            Add,
            Modify,
            Delete
        }




        public override void AfterConstruction()
        {
            base.AfterConstruction();

            IsNew = Session.IsNewObject(this);
        }

        protected override void OnLoaded()
        {
            base.OnLoaded();

            IsNew = Session.IsNewObject(this);
        }

        protected override void OnDeleting()
        {
            base.OnDeleting();

            if (AriaObjectShelveViewController.CurrentAriaObjectShelve != Guid.Empty)
            {
                AriaObjectShelve shelve = Session.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveViewController.CurrentAriaObjectShelve);
                if (shelve.TrackingEntry != null && shelve.TrackingEntry.Status == TrackingEntry.TrackingStatus.Complete)
                {
                    throw new Exception("Can't Delete, TrackingEntry is marked as complete");
                }

            }

        }

        protected override void OnSaving()
        {
            base.OnSaving();
            if (AriaObjectShelveViewController.CurrentAriaObjectShelve != Guid.Empty)
            {
                AriaObjectShelve shelve = Session.GetObjectByKey<AriaObjectShelve>(AriaObjectShelveViewController.CurrentAriaObjectShelve);
                if (shelve.TrackingEntry != null && shelve.TrackingEntry.Status == TrackingEntry.TrackingStatus.Complete)
                {
                    throw new Exception("Can't Save, TrackingEntry is marked as complete");
                }

            }

        }

        [CodeRule]
        public class AriaObjectShelveEventCodeRule : RuleBase<AriaObjectShelveEvent>
        {
            protected override bool IsValidInternal(AriaObjectShelveEvent target, out string errorMessageTemplate)
            {
                AriaObjectShelveViewController e = new AriaObjectShelveViewController();

                if (target.IsNew)
                {
                    var AriaObjectShelve = target.AriaObjectShelve;

                    foreach (AriaObjectShelveEvent p in AriaObjectShelve.AriaObjectShelveEvents)
                    {
                        if (p.Oid != Guid.Empty && p.EventName.ToUpper().Trim() == target.EventName.ToUpper().Trim() && p.Oid!=target.Oid)
                        {
                            errorMessageTemplate = "Can't save, Event is already exists";
                            return false;
                        }
                    }

                    if (target.AriaObjectShelve.AriaObject != null)
                    {
                        var AriaObject = target.AriaObjectShelve.AriaObject;

                        foreach (AriaObjectEvent p in AriaObject.AriaObjectEvents)
                        {
                            if (p.Oid != Guid.Empty && p.EventName.ToUpper().Trim() == target.EventName.ToUpper().Trim())
                            {
                                errorMessageTemplate = "Can't save, Event is already exists";
                                return false;
                            }
                        }
                    }

                    errorMessageTemplate = "";
                    return true;
                }

                errorMessageTemplate = "";
                return true;
            }

            public AriaObjectShelveEventCodeRule() : base("", DefaultContexts.Save) { }
            public AriaObjectShelveEventCodeRule(IRuleBaseProperties properties) : base(properties) { }
        }




    }

   
}
