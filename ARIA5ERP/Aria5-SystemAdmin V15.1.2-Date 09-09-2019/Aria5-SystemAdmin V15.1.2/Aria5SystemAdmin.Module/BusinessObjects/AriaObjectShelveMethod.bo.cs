using Aria5SystemAdmin.Module.Controllers;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Validation;
using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class AriaObjectShelveMethod
    {
        public enum ModificationTypes
        {
            Add,
            Modify,
            Delete
        };


        

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
        public class AriaObjectShelveMethodCodeRule : RuleBase<AriaObjectShelveMethod>
        {
            protected override bool IsValidInternal(AriaObjectShelveMethod target, out string errorMessageTemplate)
            {
                AriaObjectShelveViewController e = new AriaObjectShelveViewController();

                if (target.IsNew)
                {
                    var AriaObjectShelve = target.AriaObjectShelve;

                    foreach (AriaObjectShelveMethod p in AriaObjectShelve.AriaObjectShelveMethods)
                    {
                        if (p.Oid != Guid.Empty && p.Oid != target.Oid && p.MethodName.ToUpper().Trim() == target.MethodName.ToUpper().Trim())
                        {
                            errorMessageTemplate = "Can't save, Method is already exists";
                            return false;
                        }
                    }

                    if (target.AriaObjectShelve.AriaObject != null)
                    {
                        var AriaObject = target.AriaObjectShelve.AriaObject;

                        foreach (AriaObjectMethod p in AriaObject.AriaObjectMethods)
                        {
                            if (p.Oid != Guid.Empty && p.MethodName.ToUpper().Trim() == target.MethodName.ToUpper().Trim())
                            {
                                errorMessageTemplate = "Can't save, Method is already exists";
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

            public AriaObjectShelveMethodCodeRule() : base("", DefaultContexts.Save) { }
            public AriaObjectShelveMethodCodeRule(IRuleBaseProperties properties) : base(properties) { }
        }
    }
   
}
