using Aria5SystemAdmin.Module.Managers;
using DevExpress.Data.Filtering;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class Account
    {  //SARA.N,1 [Tracking# + Aria5-DevExpress-Account]_Programming, 2-08-2015 [START]
        protected override void OnDeleting()
        {
            if (AccountDevices.Count>0 ) Session.Delete(AccountDevices);
            if (ConfigurationItems.Count > 0) Session.Delete(ConfigurationItems);
            if (Persons.Count > 0) Session.Delete(Persons);
            if (EntityAddresses.Count > 0) Session.Delete(EntityAddresses);
            if (EntityAttachments.Count > 0) Session.Delete(EntityAttachments);
            if (EntityCategory != null) Session.Delete(EntityCategory);
            if (Divisions.Count > 0) Session.Delete(Divisions);
            if (ContactPhones.Count > 0) Session.Delete(ContactPhones);
            if (ContactAddresses.Count>0)  Session.Delete(ContactAddresses);

            //sara.N,1 28-02-2016  Deleting Account doesn't delete all Data from data base [start]
            if (EntityRelationships.Count > 0) { Session.Delete(EntityRelationships); }
            if (Locations.Count > 0) { Session.Delete(Locations); }
            if (ADS.Count > 0) { Session.Delete(ADS); }
            if (News.Count > 0) { Session.Delete(News); }
            //ATA , 2 , Can't delet account issue [begin]
           // if (Notes != null) { Session.Delete(Notes); }
            //if (ParentContact != Guid.Empty) { Session.Delete(ParentContact); }
            //ATA , 2 , Can't delet account issue [end]

            //sara.N,1 28-02-2016  Deleting Account doesn't delete all Data from data base [End]

            // Sara.N,1/11/2016 Delete Account users and drop sql created schema [Start]
            XPCollection<AriaSecuritySystemUser> accountusers = new XPCollection<AriaSecuritySystemUser>(Session, CriteriaOperator.Parse("[Account]='" + Oid + "'"));

            for (int i = 0; i < accountusers.Count; i++)
            {
                Session.Delete(accountusers[i]);
            }
            //foreach (AriaSecuritySystemUser user in accountusers)
            //{
            //    Session.Delete(user);
            //  //  user.Delete();
            //}
           
            // Sara.N,1/11/2016 Delete Account users and drop sql created schema [End]
            base.OnDeleting();
        }
        //SARA.N,1 [Tracking# + Aria5-DevExpress-Account]_Programming, 2-08-2015 [END]
     
        
    }
}
