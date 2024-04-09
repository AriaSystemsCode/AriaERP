namespace Aria.Environment
{
    public enum AriaDatabaseTypes
    {
        Aria27Data,              // FOX          ODBC
        Aria40Data,              // SQL 2000     SQL
        Aria27SystemFiles,       // FOX          ODBC
        Aria40SystemFiles,       // FOX          ODBC
        Aria50SystemFiles,       // SQL 2005     SQL
        Aria50ClientSystemFiles, // SQL 2005     SQL Client
        AriaOpenRowSet           // Open Rowset SQL Client
    }

    public enum DatabaseServerLoginTypes
    {
        // T20110803.0001 MAH June 13 2011
        NotSet,
        // T20110803.0001 MAH June 13 2011 End
        WindowAuthentication,
        SqlServerAuthentication
    }

}
