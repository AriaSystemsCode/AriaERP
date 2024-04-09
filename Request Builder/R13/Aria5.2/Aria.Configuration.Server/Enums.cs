namespace Aria.Configuration.Server
{
    public enum AriaConfigurationStatusTypes
    {
        Configured,
        NotConfigured,
        Warning,
        OptionalConfiguration
    }

    public enum ComActivationType
    {
        Library,
        Server
    }

    public enum ComSecurityLevel
    {
        Process,
        ProcessAndComponent
    }

    public enum DatabaseServerLoginTypes
    {
        WindowAuthentication,
        SqlServerAuthentication
    }
}