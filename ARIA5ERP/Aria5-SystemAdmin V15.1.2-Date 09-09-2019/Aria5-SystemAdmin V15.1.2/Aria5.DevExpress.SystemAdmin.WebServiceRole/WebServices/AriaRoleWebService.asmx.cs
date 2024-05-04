using Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling;
using Aria5SystemAdmin.Module.BusinessObjects;
using Aria5SystemAdmin.Module.DataTypes;
using Aria5SystemAdmin.Module.Managers;
using DevExpress.Data.Utils;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Configuration;
using System.Web.Services;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.WebServices
{
    /// <summary>
    /// Summary description for AriaRoleWebService
    /// </summary>
    [WebService(Namespace = "http://tempuri.org/")]
    [WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
    [System.ComponentModel.ToolboxItem(false)]
    // To allow this Web Service to be called from script, using ASP.NET AJAX, uncomment the following line. 
    // [System.Web.Script.Services.ScriptService]
    public class AriaRoleWebService : System.Web.Services.WebService
    {
        [WebMethod(EnableSession = true)]
        public void AddNewRole()
        {
           Helpers.SchemaHelper.SwitchSchema();

            Session session = XpoDefault.Session;
            if (session.IsConnected == true)
            {
                AriaSecuritySystemRole newRole = new AriaSecuritySystemRole(session);
                newRole.Save();
            }
        }

        [WebMethod(EnableSession = true)]
        public AriaUserPermissionsMarshalling GetUserPermissionsList(string userName, string applicationName)
        {
           Helpers.SchemaHelper.SwitchSchema();

            AriaRoleManager manager = new AriaRoleManager();
            AriaUserPermissions tempReturn = new AriaUserPermissions();
            tempReturn = manager.GetUserPermissionsList(userName, applicationName);
            AriaUserPermissionsMarshalling returnAriaUserPermissions = new AriaUserPermissionsMarshalling();
            returnAriaUserPermissions.IsAdministrator = tempReturn.IsAdministrator;

            if(tempReturn.UserTypePermissions != null) foreach (AriaUserTypePermissions oAriaUserTypePermissions in tempReturn.UserTypePermissions)
            {
                AriaUserTypePermissionsMarshalling tempAriaUserTypePermissions = new AriaUserTypePermissionsMarshalling ();
                tempAriaUserTypePermissions.TargetType = oAriaUserTypePermissions.TargetType ;
                tempAriaUserTypePermissions.ObjectType = oAriaUserTypePermissions.ObjectType;
                tempAriaUserTypePermissions.AllowRead = oAriaUserTypePermissions.AllowRead;
                tempAriaUserTypePermissions.AllowWrite = oAriaUserTypePermissions.AllowWrite;
                tempAriaUserTypePermissions.AllowCreate = oAriaUserTypePermissions.AllowCreate;
                tempAriaUserTypePermissions.AllowDelete = oAriaUserTypePermissions.AllowDelete;
                tempAriaUserTypePermissions.AllowNavigate = oAriaUserTypePermissions.AllowNavigate;
                foreach (AriaMethodPermission tempAriaMethodPermission in oAriaUserTypePermissions.MethodPermission)
                {
                    AriaMethodPermissionMarshalling tempMethodPermission = new AriaMethodPermissionMarshalling();
                    tempMethodPermission.MethodName =tempAriaMethodPermission.MethodName ;
                    tempMethodPermission.AllowExecute = tempAriaMethodPermission.AllowExecute;
                    tempAriaUserTypePermissions.MethodPermission.Add(tempMethodPermission);  
                }

                foreach (AriaPropertyPermission tempAriaPropertyPermission in oAriaUserTypePermissions.PropertyPermission)
                {
                    AriaPropertyPermissionMarshalling tempPropertyPermission = new AriaPropertyPermissionMarshalling();
                    tempPropertyPermission.PropertyName = tempAriaPropertyPermission.PropertyName;
                    tempPropertyPermission.AllowRead = tempAriaPropertyPermission.AllowRead;
                    tempPropertyPermission.AllowWrite = tempAriaPropertyPermission.AllowWrite;
                    tempAriaUserTypePermissions.PropertyPermission.Add(tempPropertyPermission);
                }

                foreach (AriaUserObjectPermissions tempAriaObjectPermission in oAriaUserTypePermissions.UserObjectPermissions)
                {
                    AriaUserObjectPermissionsMarshalling tempObjectPermission = new AriaUserObjectPermissionsMarshalling();
                    tempObjectPermission.TargetType = tempAriaObjectPermission.TargetType ;
                    tempObjectPermission. ObjectType =tempAriaObjectPermission.ObjectType ;
                    tempObjectPermission.AllowRead = tempAriaObjectPermission.AllowRead ;
                    tempObjectPermission.AllowWrite =tempAriaObjectPermission.AllowWrite ;
                    tempObjectPermission.AllowCreate = tempAriaObjectPermission.AllowCreate ;
                    tempObjectPermission.AllowDelete = tempAriaObjectPermission.AllowDelete ;
                    tempObjectPermission.AllowNavigate = tempAriaObjectPermission.AllowNavigate ;
                    tempObjectPermission.Criteria =  tempAriaObjectPermission.Criteria ;
                    foreach (AriaMethodPermission MethodPermission in tempAriaObjectPermission.MethodPermission)
                    {
                        AriaMethodPermissionMarshalling tmpMethodMatshalling =  new AriaMethodPermissionMarshalling ();
                        tmpMethodMatshalling.MethodName = MethodPermission.MethodName ;
                        tmpMethodMatshalling.AllowExecute = MethodPermission.AllowExecute ;
                        tempObjectPermission.MethodPermission.Add (tmpMethodMatshalling);
                    }

                    foreach (AriaPropertyPermission PropertyPermission in tempAriaObjectPermission.PropertyPermission)
                    {
                        AriaPropertyPermissionMarshalling tmppropMatshalling =  new AriaPropertyPermissionMarshalling ();
                        tmppropMatshalling.PropertyName = PropertyPermission.PropertyName ;
                        tmppropMatshalling.AllowRead =  PropertyPermission.AllowRead ;
                        tmppropMatshalling.AllowWrite =  PropertyPermission.AllowWrite ;
                        tempObjectPermission.PropertyPermission.Add (tmppropMatshalling);
                    }
                    
                    tempAriaUserTypePermissions.UserObjectPermissions.Add(tempObjectPermission);
                }
                returnAriaUserPermissions.UserTypePermissions.Add(tempAriaUserTypePermissions);
            }
            return returnAriaUserPermissions;
        }
    }
}
