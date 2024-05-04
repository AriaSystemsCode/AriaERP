using Aria5SystemAdmin.Module.BusinessObjects;
using Aria5SystemAdmin.Module.DataTypes;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp.Security.Strategy;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aria5SystemAdmin.Module.Managers
{
    public class AriaRoleManager
    {
        public AriaUserPermissions GetUserPermissionsList(string userName, string applicationName)
        {
            AriaUserPermissions permission = new AriaUserPermissions();
            
            Session session = XpoDefault.Session;
            
            AriaSecuritySystemUser user = session.FindObject<AriaSecuritySystemUser>(BinaryOperator.Parse("UserName = '" + userName + "'"));
 
            if (user != null)
            {

                if (user.Roles != null)  foreach (AriaSecuritySystemRole role in user.Roles)
                {
                    
                     if (IsRoleAdmin(role))
                     {
                         permission.IsAdministrator = true;
                       break;
                     }
                 
                }
            }
            
            return permission;
        }
        public bool IsRoleAdmin(AriaSecuritySystemRole role)
        { 
            bool isAdmin = false;
            foreach (AriaSecuritySystemRole childRole in role.ChildRoles)
            {
                if (childRole.IsAdministrative)
                {
                    isAdmin = true;
                    break;
                }
                else
                {
                    if (IsRoleAdmin(childRole))
                    {
                        isAdmin = true;
                        break;
                    }
                }
            }

            if (role.IsAdministrative)
            {
                isAdmin = true;

            }
            return isAdmin;
        }
        
        public void AssignRoleToUser(Guid userId, Guid roleId)
        {
            Session session = XpoDefault.Session;

            AriaSecuritySystemRole userRole = session.FindObject<AriaSecuritySystemRole>(new BinaryOperator("Oid", roleId));
            AriaSecuritySystemUser user = session.FindObject<AriaSecuritySystemUser>(new BinaryOperator("Oid", userId));
            if (user != null & userRole != null)
            {
                user.Roles.Add(userRole);
                user.Save();
            }
        }
    }
}
