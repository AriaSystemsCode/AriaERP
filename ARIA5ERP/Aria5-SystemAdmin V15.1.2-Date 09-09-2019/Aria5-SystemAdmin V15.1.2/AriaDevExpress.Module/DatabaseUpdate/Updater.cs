using System;

using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Updating;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.Persistent.BaseImpl;
using DevExpress.ExpressApp.Security;

namespace AriaDevExpress.Module.DatabaseUpdate
{
    public class Updater : ModuleUpdater
    {
        public Updater(IObjectSpace objectSpace, Version currentDBVersion) : base(objectSpace, currentDBVersion) { }
        public override void UpdateDatabaseAfterUpdateSchema()
        {
            base.UpdateDatabaseAfterUpdateSchema();
            
            //SecurityRole role = ObjectSpace.FindObject<SecurityRole>(new BinaryOperator("Name", SecurityStrategy.AdministratorRoleName));
            //if (role == null)
            //{
            //    role = ObjectSpace.CreateObject<SecurityRole>();
            //    role.Name = SecurityStrategy.AdministratorRoleName;
            //    role.CanEditModel = true;
            //    role.BeginUpdate();
            //    role.Permissions.GrantRecursive(typeof(object), SecurityOperations.Create);
            //    role.Permissions.GrantRecursive(typeof(object), SecurityOperations.Navigate);
            //    role.Permissions.GrantRecursive(typeof(object), SecurityOperations.Delete);
            //    role.Permissions.GrantRecursive(typeof(object), SecurityOperations.Read);
            //    role.Permissions.GrantRecursive(typeof(object), SecurityOperations.Write);
            //    role.EndUpdate();

            //    role.Save();
            //}

            //SecurityUser user = ObjectSpace.FindObject<SecurityUser>(new BinaryOperator("UserName", "Admin"));
            //if (user == null)
            //{
            //    user = ObjectSpace.CreateObject<SecurityUser>();
            //    user.UserName = "Admin";
            //    user.SetPassword("");
            //   // user.Roles.Add(role);
            //    user.Save();
            //}
            
            // look
            // As per Mostafa, used by DevExpress older version, now, no need, to it.
            //SecurityUser anonymousUser = ObjectSpace.FindObject<SecurityUser>(new BinaryOperator("UserName", SecurityStrategy.AnonymousUserName));

            //if (anonymousUser == null)
            //{
            //    anonymousUser = ObjectSpace.CreateObject<SecurityUser>();
            //   anonymousUser.UserName = SecurityStrategy.AnonymousUserName;
               
            //    anonymousUser.SetPassword("");
            //    anonymousUser.Save();
            //}
            ObjectSpace.CommitChanges();
        }
    }
}
