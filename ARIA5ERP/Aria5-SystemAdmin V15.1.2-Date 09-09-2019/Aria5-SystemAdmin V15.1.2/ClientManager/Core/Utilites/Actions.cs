using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Core.Utilites
{
    public enum Actions
    {
        None,
        CreateClientAriaFolder,
        CreateClientSQLFolder,
        Copy_AriaSource_ClientFolder,
        CreateClientUser,
        Copy_AriaMaster_ClientSQLFolder,
        AttachClientMasterDB,
        CreateAllA4Databases,
        CreateCompanyDbfsFiles,
        AddAllSycCompanyInfo,
        UpdateSycinst,
        CreateClientGroup,
        CreateClientUsers,
        AdjustClientFolderSecurity,
        ShareClientSharedFolder,
        CreateClientSettingsXML,
        AddSystemMasterClientRow,
        AddSystemMasterAriaUsersRows,
        AddSystemMasterClientProductRows,
        DeleteActKey,
        CopyActKey,
        UpdateSydAppl,
        AddSystemMasterAriaUsersRoles,
    }
}
