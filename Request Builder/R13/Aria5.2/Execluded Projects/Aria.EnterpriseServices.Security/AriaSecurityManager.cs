using System;
using System.Data;
using Aria.Data;
using Aria.Environment;
using Aria.Data;
using System.EnterpriseServices;
using Aria.DataTypes.Security;


namespace Aria.EnterpriseServices.Security
{
    public class AriaSecurityManager : ServicedComponent 
    {
        private bool CheckAria27UserPrivilage(AriaDbConnection connection, string UserName, string password, 
                                                string processID, Aria27Privilege privilege)
        {
            string userSqlCommand = "SELECT * FROM syuuser WHERE Cuser_id = '@UserName'";
            AriaDbCommand userCommand = new AriaDbCommand(userSqlCommand, connection, AriaDatabaseTypes.Aria27SystemFiles);
            userCommand.Parameters.Add(new AriaDbParameter("UserName", UserName.PadRight(10)));

            DataTable userTable = userCommand.GetDataTable();

            if (userTable.Rows.Count == 0 || userTable.Rows[0]["cusr_pass"].ToString().TrimEnd() != password.TrimEnd())
            {
                return false;
            }
            else
            {   
                if (userTable.Rows[0]["cusr_levl"].ToString().TrimEnd() == "A")
                {
                    return true;
                }

                else
                {
                    string privilegeSqlCommand = "SELECT * FROM syuusrpr WHERE ((Cuser_id = '@UserID' and Cgrporuser = 'U') or " +
                                                    "(Cuser_id = '@GroupID' and Cgrporuser = 'G')) and " +
                                                        "Cpross_id = '@ProcessID' and ccomp_id = '@CompanyID'";
                    AriaDbCommand privilegeCommand = new AriaDbCommand(privilegeSqlCommand, connection, AriaDatabaseTypes.Aria27SystemFiles);
                    privilegeCommand.Parameters.Add(new AriaDbParameter("UserID", UserName.PadRight(10)));
                    privilegeCommand.Parameters.Add(new AriaDbParameter("UserID", userTable.Rows[0]["Cuser_id"]));
                    privilegeCommand.Parameters.Add(new AriaDbParameter("GroupID", userTable.Rows[0]["Cusr_grup"]));
                    privilegeCommand.Parameters.Add(new AriaDbParameter("ProcessID", processID.PadRight(10)));
                    privilegeCommand.Parameters.Add(new AriaDbParameter("CompanyID", connection.CompanyName.PadRight(2)));



                    DataTable privilegeTable = privilegeCommand.GetDataTable();

                    for (int index = 0; index < privilegeTable.Rows.Count; index++)
                    {
                        switch (privilege)
                        {
                            case Aria27Privilege.Add:
                                if (privilegeTable.Rows[index]["Laddrec"].Equals(true))
                                    return true;
                                break;

                            case Aria27Privilege.Delete:
                                if (privilegeTable.Rows[index]["Ldelerec"].Equals(true))
                                    return true;
                                break;

                            case Aria27Privilege.Edit:
                                if (privilegeTable.Rows[index]["Leditrec"].Equals(true))
                                    return true;
                                break;

                            case Aria27Privilege.View:
                                    return true;
                                break;
                        }
                    }
                }
            }

            return false;
        }

        public bool CheckSecurity(AriaDbConnection connection, string userName, string password, string objectName, string methodName)
        {
            string sqlCommand = "SELECT AriaObjectSecurity27Mapping.ProcessID, AriaObjectMethodSecurity27Mapping.Privilege FROM AriaObjectMethodSecurity27Mapping LEFT JOIN AriaObjectSecurity27Mapping " +
                                    "ON (AriaObjectSecurity27Mapping.ObjectName = AriaObjectMethodSecurity27Mapping.ObjectName) " +
                                        "WHERE AriaObjectSecurity27Mapping.ObjectName = @ObjectName and AriaObjectMethodSecurity27Mapping.MethodName = @MethodName";

            AriaDbCommand command = new AriaDbCommand(sqlCommand, connection, AriaDatabaseTypes.Aria50SystemFiles);

            command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));
            command.Parameters.Add(new AriaDbParameter("MethodName", methodName));

            DataTable table = command.GetDataTable();

            if (table.Rows.Count == 0 || table.Rows[0]["Privilege"] is DBNull)
            {
                return false;
            }
            else
            {
                return CheckAria27UserPrivilage(connection, userName, password, table.Rows[0]["ProcessID"].ToString(),
                                                    (Aria27Privilege)Enum.Parse(typeof(Aria27Privilege), table.Rows[0]["Privilege"].ToString()));
            }
        }
    }
}
