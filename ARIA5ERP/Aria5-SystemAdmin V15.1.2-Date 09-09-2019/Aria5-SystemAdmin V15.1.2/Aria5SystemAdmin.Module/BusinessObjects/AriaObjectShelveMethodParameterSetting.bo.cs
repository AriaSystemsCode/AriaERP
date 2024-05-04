using Aria5SystemAdmin.Module.Controllers;
using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class AriaObjectShelveMethodParameterSetting
    {


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

            if (_valueIsChanged == true)
                Modified = true;

        }
    }



}
