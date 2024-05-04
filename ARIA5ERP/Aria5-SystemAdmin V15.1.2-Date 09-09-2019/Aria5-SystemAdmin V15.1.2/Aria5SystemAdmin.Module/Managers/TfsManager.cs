using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Collections.ObjectModel;
using System.Net;
//using Microsoft.TeamFoundation.Server;
//using Microsoft.TeamFoundation.VersionControl.Client;
//using Microsoft.TeamFoundation.Client;
using System.Web.Configuration;
using Aria5SystemAdmin.Module;
using Aria5SystemAdmin.Module.BusinessObjects;
using Microsoft.TeamFoundation.Client;
using Microsoft.TeamFoundation.VersionControl.Client;
using Microsoft.TeamFoundation.Build.Client;
using Aria5SystemAdmin.Module.ServiceReferenceTfsManager;

/// <summary>
/// Summary description for TfsHandler
/// </summary>
/// 
namespace Aria5SystemAdmin.Module.Managers
{
    public class TfsManager
    {

        # region Declarations
        //public ProjectInfo[] projects;
        //VersionControlServer sourceControl;
        public string path = string.Empty;
        public string filepath = string.Empty;
        public string AssignedResource = string.Empty;
        //public Workspace workspace;
        public PendingChange[] PendingChanges;
        #endregion

        # region Properties
        public Uri TfsUri
        {
            set;
            get;

        }
        public string TfsUserName
        {
            set;
            get;

        }
        public string TfsPassWord
        {
            set;
            get;

        }
        public string TfsDomain
        {
            set;
            get;

        }
        public string TfsProjectName
        {
            set;
            get;

        }
        public string TfsGroupName
        {
            set;
            get;

        }

        public string TfsCollection
        {
            set;
            get;

        }

        public string BuildWorkSapce
        {
            set;
            get;

        }
        public string TfsWorkSpacePath
        {
            set;
            get;
        }
        public string TfsCheckinWorkSpace
        {
            set;
            get;
        }
        # endregion

        # region Constructor
        public TfsManager()
        {

            TfsUri = new Uri(WebConfigurationManager.AppSettings["tfsUri"].ToString());
            TfsUserName = WebConfigurationManager.AppSettings["tfsUserName"].ToString();
            TfsPassWord = WebConfigurationManager.AppSettings["tfsPassWord"].ToString();
            TfsDomain = WebConfigurationManager.AppSettings["tfsDomain"].ToString();
            TfsProjectName = WebConfigurationManager.AppSettings["tfsProjectName"].ToString();
            TfsGroupName = WebConfigurationManager.AppSettings["tfsGroupName"].ToString();
            TfsCollection = WebConfigurationManager.AppSettings["TFSCollection"].ToString();
            BuildWorkSapce = WebConfigurationManager.AppSettings["BuildWorkSpace"].ToString();
            TfsWorkSpacePath = WebConfigurationManager.AppSettings["tfsWorkSpacePath"].ToString();
            TfsCheckinWorkSpace = WebConfigurationManager.AppSettings["tfsCheckinWorkSpaceCreated"].ToString();

            //TfsUri = new Uri(tfsUri);
            //TfsUserName = tfsUserName;
            //TfsPassWord = tfsPassWord;
            //TfsDomain = tfsDomain;
            //TfsProjectName = tfsProjectName;
            //TfsGroupName = tfsGroupName;
        }

        public TfsManager(string TFSCollection, string TFSProject)
        {

            TfsUserName = WebConfigurationManager.AppSettings["tfsUserName"].ToString();
            TfsPassWord = WebConfigurationManager.AppSettings["tfsPassWord"].ToString();
            TfsDomain = WebConfigurationManager.AppSettings["tfsDomain"].ToString();
            TfsProjectName = TFSProject;
            TfsGroupName = WebConfigurationManager.AppSettings["tfsGroupName"].ToString();
            TfsCollection = TFSCollection;
            BuildWorkSapce = WebConfigurationManager.AppSettings["BuildWorkSpace"].ToString();
            TfsWorkSpacePath = WebConfigurationManager.AppSettings["tfsWorkSpacePath"].ToString();
            TfsCheckinWorkSpace = WebConfigurationManager.AppSettings["tfsCheckinWorkSpaceCreated"].ToString();

        }
        # endregion

        # region Tracking
        // Sara.N 19/09/2015 QA Control TFS [ChekOut and CheckIn] Using System Admin [Start]
        public void GrantCheckoutFilePermission(String filepath, String AssignedResource)
        {
            Aria5SystemAdmin.Module.ServiceReferenceTfsManager.WebService1SoapClient webService1SoapClient = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.WebService1SoapClient();
            webService1SoapClient.SetParameters(TfsUserName, TfsPassWord, TfsDomain, TfsProjectName, TfsGroupName, TfsCollection, TfsCheckinWorkSpace, "", "");
            webService1SoapClient.InnerChannel.OperationTimeout = new TimeSpan(1, 0, 0);
            webService1SoapClient.GrantCheckoutFilePermission(filepath, AssignedResource);

            # region Commented as webservice
            ////DevExpress.XtraEditors.XtraMessageBox.Show("Resource can Check out the Files!");
            //Uri tfsUri = new Uri(TfsCollection);
            //NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
            //TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(tfsUri, cre);
            //VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
            //List<SecurityChange> changes = new List<SecurityChange>();
            //changes.Add(new PermissionChange(filepath, AssignedResource, new string[] { "PendChange" }, null, null));
            //SecurityChange[] actualChanges = vcs.SetPermissions(changes.ToArray());
            # endregion

        }

        public void DenyCheckoutFilePermission(String filepath, String AssignedResource)
        {

            Aria5SystemAdmin.Module.ServiceReferenceTfsManager.WebService1SoapClient webService1SoapClient = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.WebService1SoapClient();
            webService1SoapClient.SetParameters(TfsUserName, TfsPassWord, TfsDomain, TfsProjectName, TfsGroupName, TfsCollection, TfsCheckinWorkSpace, "", "");
            webService1SoapClient.InnerChannel.OperationTimeout = new TimeSpan(1, 0, 0);
            webService1SoapClient.DenyCheckoutFilePermission(filepath, AssignedResource);

            # region Commented as webservice
            ////DevExpress.XtraEditors.XtraMessageBox.Show("Resource is denyed to Check out the Files!");
            //// Uri TfsUri = new Uri("http://tf_server:8080/tfs/Aria_Dev-2015");
            //Uri TfsUri = new Uri(TfsCollection);
            //NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
            //TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);
            //VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
            //List<SecurityChange> changes = new List<SecurityChange>();
            //changes.Add(new PermissionChange(filepath, AssignedResource, null, new string[] { "PendChange" }, null));
            //SecurityChange[] actualChanges = vcs.SetPermissions(changes.ToArray());
            # endregion

        }

        public void CommentCheckin(string ProjectPath, String Shelvset, String AssignedResource, String Comment)
        {
            Aria5SystemAdmin.Module.ServiceReferenceTfsManager.WebService1SoapClient webService1SoapClient = new Aria5SystemAdmin.Module.ServiceReferenceTfsManager.WebService1SoapClient();
            webService1SoapClient.SetParameters(TfsUserName, TfsPassWord, TfsDomain, TfsProjectName, TfsGroupName, TfsCollection, TfsCheckinWorkSpace, "", "");
            webService1SoapClient.InnerChannel.OperationTimeout = new TimeSpan(1, 0, 0);
           webService1SoapClient.CommentCheckin(ProjectPath, Shelvset, AssignedResource, Comment);

            # region Commented as webservice
            //// DevExpress.XtraEditors.XtraMessageBox.Show("CheckIn Files Completed Successfully!");
            //string NewWorkSpace = "";
            //Uri TfsUri = new Uri(TfsCollection);
            //TfsCheckinWorkSpace = "TestWorkSpace";
            //NewWorkSpace = @"D:\Dev\" + TfsCheckinWorkSpace;
            //NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
            //TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);
            //VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
            //if (vcs.GetWorkspace(TfsCheckinWorkSpace, TfsUserName) == null)
            //{
            //    vcs.CreateWorkspace(TfsCheckinWorkSpace, TfsUserName);
            //}
            //var workspace = vcs.GetWorkspace(TfsCheckinWorkSpace, TfsUserName);
            //workspace.Map(ProjectPath, NewWorkSpace);
            //var LatestRequest = new GetRequest(new ItemSpec(ProjectPath, RecursionType.Full), VersionSpec.Latest);
            //var results = workspace.Get(LatestRequest, GetOptions.GetAll | GetOptions.Overwrite);
            //workspace.Unshelve(Shelvset, AssignedResource);
            //PendingChanges = workspace.GetPendingChanges();
            //var returnval = workspace.CheckIn(PendingChanges, Comment);
            
            //vcs.DeleteWorkspace(TfsCheckinWorkSpace, TfsUserName);
            //return (int)returnval;
            # endregion

        }
        //Sara.N 19/09/2015 QA Control TFS [ChekOut and CheckIn] Using System Admin [End]
        # endregion

        # region Build
        // Sara.N 2-11-2015  QA Control TFS [Generate Build] Using System Admin [Start]   
        public void CreateWorkSpace(string tfsMappingPathValue, string initLabel, string lastLabel)
        {
            ServiceReferenceTfsManager.WebService1SoapClient webService1SoapClient = new ServiceReferenceTfsManager.WebService1SoapClient();
            webService1SoapClient.SetParameters(TfsUserName, TfsPassWord, TfsDomain, TfsProjectName, TfsGroupName, TfsCollection, TfsCheckinWorkSpace, BuildWorkSapce, TfsWorkSpacePath);
            webService1SoapClient.InnerChannel.OperationTimeout = new TimeSpan(1, 0, 0);
            webService1SoapClient.CreateWorkSpace(tfsMappingPathValue, initLabel, lastLabel);

            //var client = new ServiceReferenceTfsManager.WebService1SoapClient();
            //client.InnerChannel.OperationTimeout = TimeSpan.FromMinutes(3);

            # region Commented as webservice

           // NetworkCredential networkCredential = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
           // //sara
           // //// Uri TfsUri = new Uri("http://tf_server:8080/tfs/Aria_Dev-2015");
           // Uri TfsUri = new Uri(TfsCollection);
           // TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, networkCredential);
           //// TeamFoundationServer tfs = new TeamFoundationServer(TfsCollection, networkCredential);

           // VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
            
            
           // if (vcs.TryGetWorkspace(BuildWorkSapce) != null)
           // {
           //     vcs.DeleteWorkspace(BuildWorkSapce, TfsUserName);
           // }
           // // Creating TFS WorkSpace 
           // if (vcs.TryGetWorkspace(BuildWorkSapce) == null)
           // {
           //     vcs.CreateWorkspace(BuildWorkSapce, TfsUserName);
           // }
           // var workspace = vcs.GetWorkspace(BuildWorkSapce, TfsUserName);
           // // Mapping Source Code from ProjectPath to Physical Path at TFS WorkSpace 
           // workspace.Map(tfsMappingPathValue, TfsWorkSpacePath); //@"D:\Dev\BuildWorkSpace");  //@"D:\Dev\BuildWorkSpace"
           // //Get Latest Vr. of the Source Code 
           // var LatestRequest = new GetRequest(new ItemSpec(tfsMappingPathValue, RecursionType.Full), VersionSpec.Latest);
           // var results = workspace.Get(LatestRequest, GetOptions.GetAll | GetOptions.Overwrite);
           // if (lastLabel == null || lastLabel.Trim() == "")
           // {
           //     var LabelRequest = new GetRequest(new ItemSpec(tfsMappingPathValue, RecursionType.Full), new LabelVersionSpec(initLabel));// Mr. Mahmoud check where get the Old Label ????
           //     var LabelResults = workspace.Get(LabelRequest, GetOptions.GetAll | GetOptions.Overwrite);
           // }
           // else
           // {
           //     var LabelRequest = new GetRequest(new ItemSpec(tfsMappingPathValue, RecursionType.Full), new LabelVersionSpec(lastLabel));// Mr. Mahmoud check where get the Old Label ????
           //     var LabelResults = workspace.Get(LabelRequest, GetOptions.GetAll | GetOptions.Overwrite);
           // }
            # endregion
        }
        public void GetSpecificChangeset(string tfsMappingPathValue, int changeset, string StorfgefilePath)
        {

            //ServiceReferenceTfsManager.WebService1SoapClient webService1SoapClient = new ServiceReferenceTfsManager.WebService1SoapClient();
            //webService1SoapClient.SetParameters(TfsUserName, TfsPassWord, TfsDomain, TfsProjectName, TfsGroupName, TfsCollection, TfsCheckinWorkSpace, "BuildWorkSpace", @"D:\Dev\BuildWorkSpace");
            //webService1SoapClient.InnerChannel.OperationTimeout = new TimeSpan(1, 0, 0);
            //webService1SoapClient.GetSpecificChangeset(tfsMappingPathValue, changeset, StorfgefilePath);

            # region Commented as webservice
            ////Check to Remove The String.sln at the end of the tfsProjectPath as example "$/New Test/App5"
            // ApplicationSetting tfsMappingPath = build.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "TFSMappingPath".Trim().ToUpper());
            // string tfsMappingPathValue = tfsMappingPath.SettingValue;

            //NetworkCredential networkCredential = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);

            //TeamFoundationServer tfs = new TeamFoundationServer(TfsCollection, networkCredential);

            //VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;

            //var workspace = vcs.GetWorkspace(BuildWorkSapce, TfsUserName);

            ////Get Specific Change Set of the Source Code 
            //// Bug
            ////var ChangeSetRequest = new GetRequest(new ItemSpec("$/1TouchAway/Aria.WindowsStore.OneTouchAway.View/Login/Login_1.xaml", RecursionType.None), new ChangesetVersionSpec(changeset));
            //var ChangeSetRequest = new GetRequest(new ItemSpec(StorfgefilePath, RecursionType.None), new ChangesetVersionSpec(changeset));

            //var ChangeSetResults = workspace.Get(ChangeSetRequest, GetOptions.GetAll | GetOptions.Overwrite);
            # endregion

        }
        public void CreateWorkSpaceLabel(string tfsMappingPathValue, string currentBuildLablestring)
        {
            //ServiceReferenceTfsManager.WebService1SoapClient webService1SoapClient = new ServiceReferenceTfsManager.WebService1SoapClient();
            //webService1SoapClient.SetParameters(TfsUserName, TfsPassWord, TfsDomain, TfsProjectName, TfsGroupName, TfsCollection, TfsCheckinWorkSpace, "BuildWorkSpace", @"D:\Dev\BuildWorkSpace");
            //webService1SoapClient.InnerChannel.OperationTimeout = new TimeSpan(1, 0, 0);
            //webService1SoapClient.CreateWorkSpaceLabel(tfsMappingPathValue, currentBuildLablestring);

            # region Commented as webservice


            //NetworkCredential networkCredential = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);

            //TeamFoundationServer tfs = new TeamFoundationServer(TfsCollection, networkCredential);

            //VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
            ////Creating Label From Prepeared WorkSpace with Specific Vr.
            //VersionControlLabel labelToCreate = new VersionControlLabel(vcs, currentBuildLablestring, TfsUserName, "", ""); // Mr. Mahmoud check where get the New Label 
            //LabelItemSpec[] labelItemSpecs = new LabelItemSpec[1];
            //ItemSpec itemSpec = new ItemSpec(tfsMappingPathValue, RecursionType.Full);

            //VersionSpec a;
            //a = VersionSpec.ParseSingleSpec("W" + BuildWorkSapce, TfsUserName);

            //labelItemSpecs[0] = new LabelItemSpec(itemSpec, a, false);
            //vcs.CreateLabel(labelToCreate, labelItemSpecs, LabelChildOption.Replace);

            ////Delete WorkSpace After Creating the Need Label.
            //vcs.DeleteWorkspace(BuildWorkSapce, TfsUserName);
            //Check to Remove The String.sln at the end of the tfsProjectPath as example "$/New Test/App5"
            //   ApplicationSetting tfsMappingPath = build.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "TFSMappingPath".Trim().ToUpper());
            // string tfsMappingPathValue = tfsMappingPath.SettingValue;

            //  ApplicationSetting currentBuildLable = build.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "CurrentBuildLabel".Trim().ToUpper());
            //  string currentBuildLablestring = currentBuildLable.SettingValue;

            //NetworkCredential networkCredential = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);

            //TeamFoundationServer tfs = new TeamFoundationServer(TfsCollection, networkCredential);

            //VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
            ////Creating Label From Prepeared WorkSpace with Specific Vr.
            //VersionControlLabel labelToCreate = new VersionControlLabel(vcs, currentBuildLablestring, TfsUserName, "", ""); // Mr. Mahmoud check where get the New Label 
            //LabelItemSpec[] labelItemSpecs = new LabelItemSpec[1];
            //ItemSpec itemSpec = new ItemSpec(tfsMappingPathValue, RecursionType.Full);

            //VersionSpec a;
            //a = VersionSpec.ParseSingleSpec("W" + BuildWorkSapce, TfsUserName);

            //labelItemSpecs[0] = new LabelItemSpec(itemSpec, a, false);
            //vcs.CreateLabel(labelToCreate, labelItemSpecs, LabelChildOption.Replace);

            ////Delete WorkSpace After Creating the Need Label.
            //vcs.DeleteWorkspace(BuildWorkSapce, TfsUserName);
            # endregion


            NetworkCredential networkCredential = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);

            TeamFoundationServer tfs = new TeamFoundationServer(TfsCollection, networkCredential);

            VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
            //Creating Label From Prepeared WorkSpace with Specific Vr.
            VersionControlLabel labelToCreate = new VersionControlLabel(vcs, currentBuildLablestring, TfsUserName, "", ""); // Mr. Mahmoud check where get the New Label 
            LabelItemSpec[] labelItemSpecs = new LabelItemSpec[1];
            ItemSpec itemSpec = new ItemSpec(tfsMappingPathValue, RecursionType.Full);

            VersionSpec a;
            a = VersionSpec.ParseSingleSpec("W" + BuildWorkSapce, TfsUserName);

            labelItemSpecs[0] = new LabelItemSpec(itemSpec, a, false);
            vcs.CreateLabel(labelToCreate, labelItemSpecs, LabelChildOption.Replace);

            //Delete WorkSpace After Creating the Need Label.
            vcs.DeleteWorkspace(BuildWorkSapce, TfsUserName);

        }
        public void UpdateMetadataTFS(String xmlstring, String filename, ApplicationBuild_T build)
        {
            ////ApplicationSetting resourceFolder = build.Application_T.ApplicationSettings.First(r => r.SettingID == "Resouce Folder");
            ////string resourcepathValue = resourceFolder.SettingValue;

            ////  string filename = entityName + "_MetaData.XML";

            //NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
            //TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(new Uri(TfsCollection), cre);


            //sourceControl = (VersionControlServer)tfs.GetService(typeof(VersionControlServer));

            //var ws = sourceControl.GetWorkspace(BuildWorkSapce, TfsUserName);

            ////Checkout the Metadata.xml files
            ////ws.PendEdit(resourcepathValue + entityName + "_MetaData.XML");
            //ws.PendEdit(filename);


            ////Download on the same path ?????
            ////System.IO.File.WriteAllText(resourcepathValue + entityName + "_MetaData.XML", xmlstring);
            ////System.IO.File.WriteAllText(filename, xmlstring);
            //string physpth = TfsWorkSpacePath.Trim();
            //if (!TfsWorkSpacePath.EndsWith("\\")) physpth += "\\";
            //physpth += filename.Replace("$", "").Replace("/", "\\").Substring(TfsProjectName.Length + 2);

            //System.IO.File.WriteAllText(physpth, xmlstring);

            ////Get Pending Changes from Workspace
            //PendingChanges = ws.GetPendingChanges();

            //if (PendingChanges.Where(r => r.ServerItem.Trim().ToUpper() == filename.Trim().ToUpper()).Count() > 0)
            //{
            //    //Checkin the need files after If Condition for Validating the Metadata files.
            //    int changesetNumber = ws.CheckIn(PendingChanges.Where(r => r.ServerItem.Trim().ToUpper() == filename.Trim().ToUpper()).ToArray(), build.TFSLabel);
            //}


        }

        //public void CreateBuildDefinition(string TFSCollection, string DropLocation, string buildAgentName, string buildControllerName, string mappingPathValue, string tfsProjectPathValue, string projectname, string ApplicationBuildId, string applicationDescription)
        //{
        //    //string TFSCollection = WebConfigurationManager.AppSettings["TFSCollection"].ToString();
        //    //string DropLocation = WebConfigurationManager.AppSettings["BuildDropLocation"].ToString();
        //    //string buildAgentName = WebConfigurationManager.AppSettings["buildAgentName"].ToString();
        //    //string buildControllerName = WebConfigurationManager.AppSettings["buildControllerName"].ToString();


        //    //ApplicationSetting mappingPath = build.Application_T.ApplicationSettings.First(r => r.SettingID == "TFSMappingPath");
        //    //string mappingPathValue = mappingPath.SettingValue;
        //    string mappingDropPathValue = mappingPathValue + "\\Drops";

        //    //ApplicationSetting tfsProjectPath = build.Application_T.ApplicationSettings.First(r => r.SettingID == "TFS Project Path");
        //    //string tfsProjectPathValue = tfsProjectPath.SettingValue;

        //    //ApplicationSetting tfsProjectName = build.Application_T.ApplicationSettings.First(r => r.SettingID == "TFS Main Project name");
        //    //string projectname = tfsProjectName.SettingValue;


        //    TeamFoundationServer tfs = new TeamFoundationServer(TFSCollection);
        //    IBuildServer buildServer = (IBuildServer)tfs.GetService(typeof(IBuildServer));

        //    //Create build definition and give it a name and desription
        //    IBuildDefinition buildDef = buildServer.CreateBuildDefinition(projectname);
        //    buildDef.Name = "BuildDef# " + ApplicationBuildId;
        //    buildDef.Description = applicationDescription;
        //    buildDef.ContinuousIntegrationType = ContinuousIntegrationType.None;


        //    //Controller and default build process template
        //    buildDef.BuildController = buildServer.GetBuildController(buildControllerName);
        //    var defaultTemplate = buildServer.QueryProcessTemplates(projectname).First(p => p.TemplateType == ProcessTemplateType.Default);
        //    buildDef.Process = defaultTemplate;


        //    //Drop location
        //    buildDef.DefaultDropLocation = DropLocation;//"\\\\TF_SERVER\\Builds";    

        //    //Source Settings
        //    buildDef.Workspace.AddMapping(mappingPathValue, "$(SourceDir)", WorkspaceMappingType.Map);
        //    buildDef.Workspace.AddMapping(mappingDropPathValue, "", WorkspaceMappingType.Cloak);



        //    //Process params
        //    var process = WorkflowHelpers.DeserializeProcessParameters(buildDef.ProcessParameters);

        //    //What to build

        //    process.Add("ProjectsToBuild", new[] { tfsProjectPathValue });
        //    process.Add("ConfigurationsToBuild", new[] { "" });

        //    buildDef.ProcessParameters = WorkflowHelpers.SerializeProcessParameters(process);

        //    //Retention policy
        //    buildDef.RetentionPolicyList.Clear();
        //    buildDef.AddRetentionPolicy(BuildReason.Triggered, BuildStatus.Succeeded, 10, DeleteOptions.All);
        //    buildDef.AddRetentionPolicy(BuildReason.Triggered, BuildStatus.Failed, 10, DeleteOptions.All);
        //    buildDef.AddRetentionPolicy(BuildReason.Triggered, BuildStatus.Stopped, 1, DeleteOptions.All);
        //    buildDef.AddRetentionPolicy(BuildReason.Triggered, BuildStatus.PartiallySucceeded, 10, DeleteOptions.All);

        //    //Lets save it
        //    buildDef.Save();


        //}
        public void BuildDeploy(string currentLabel, string projectname, String buildDefinitionName)
        {
            //ServiceReferenceTfsManager.WebService1SoapClient webService1SoapClient = new ServiceReferenceTfsManager.WebService1SoapClient();
            //webService1SoapClient.SetParameters(TfsUserName, TfsPassWord, TfsDomain, TfsProjectName, TfsGroupName, TfsCollection, TfsCheckinWorkSpace, "BuildWorkSpace", @"D:\Dev\BuildWorkSpace");
            //webService1SoapClient.InnerChannel.OperationTimeout = new TimeSpan(1, 0, 0);
            //webService1SoapClient.BuildDeploy(currentLabel, projectname, buildDefinitionName);

            # region Commented as webservice
            ////ApplicationSetting currentLabelSetting = build.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "CurrentBuildLabel".Trim().ToUpper());
            ////string currentLabel = currentLabelSetting.SettingValue;

            ////ApplicationSetting tfsProjectName = build.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "TFS Main Project name".Trim().ToUpper());
            ////string projectname = tfsProjectName.SettingValue;

            //NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
            ////TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);
            ////VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;

            //TeamFoundationServer TFS = new TeamFoundationServer(TfsCollection, cre);
            ////TeamFoundationServer tfs = TeamFoundationServerFactory.GetServer(TfsCollection, cre);

            //IBuildServer buildServer = (IBuildServer)TFS.GetService(typeof(IBuildServer));
            //IBuildDefinition buildDef = buildServer.GetBuildDefinition(projectname, buildDefinitionName);
            //var queuedBuild = buildServer.QueueBuild(buildDef);

            //((IBuildDetail)queuedBuild.Builds[0]).SourceGetVersion = "L" + currentLabel; // "LNewBuildLabel";//InitialLabelvalue;


            //queuedBuild.Builds[0].Save();
            # endregion


            NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
            //TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);
            //VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;

            TeamFoundationServer TFS = new TeamFoundationServer(TfsCollection, cre);
            //TeamFoundationServer tfs = TeamFoundationServerFactory.GetServer(TfsCollection, cre);

            IBuildServer buildServer = (IBuildServer)TFS.GetService(typeof(IBuildServer));
            IBuildDefinition buildDef = buildServer.GetBuildDefinition(projectname, buildDefinitionName);
            var queuedBuild = buildServer.QueueBuild(buildDef);

            ((IBuildDetail)queuedBuild.Builds[0]).SourceGetVersion = "L" + currentLabel; // "LNewBuildLabel";//InitialLabelvalue;


            queuedBuild.Builds[0].Save();

        }

        // Sara.N 2-11-2015  QA Control TFS [Generate Build] Using System Admin [End]
        # endregion

        # region Commented
        //public Collection<string> GetTeamCollectionList()
        //{
        //    Collection<string> returnTeamCollectionList = new Collection<string>();

        //   //Uri tfsUri = new Uri("http://tf_server:8080/tfs/");
        // //Uri tfsUri = new Uri("http://41.41.15.230:171/tfs/");



        //    TfsConfigurationServer configServer = new TfsConfigurationServer(TfsUri, new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain));

        //    CatalogNode rootNode = configServer.CatalogNode;


        //    ReadOnlyCollection<CatalogNode> teamProjectCollections = rootNode.QueryChildren(new Guid[] { CatalogResourceTypes.ProjectCollection }, false, CatalogQueryOptions.None);
        //    CatalogNode teamProjectCollection = teamProjectCollections[3];

        //    var teamProjects = teamProjectCollection.QueryChildren(new Guid[] { CatalogResourceTypes.TeamProject }, false, CatalogQueryOptions.None);

        //    foreach (var item in teamProjects.ToList())
        //    {
        //        returnTeamCollectionList.Add(item.Resource.DisplayName.ToString());


        //    }

        //   sourceControl = (VersionControlServer)configServer.GetService(typeof(VersionControlServer));

        //    return returnTeamCollectionList;

        //}

        //public Collection<string> GetProjectFolders(String path)
        //{
        //    Collection<string> returnProjectFolder = new Collection<string>();

        //    try
        //    {
        //        NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
        //        Uri tfsUri = new Uri("http://tf_server:8080/tfs/Aria");
        //        TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(tfsUri, cre);
        //        sourceControl = (VersionControlServer)tfs.GetService(typeof(VersionControlServer));

        //        RecursionType recursion = RecursionType.OneLevel;

        //        Item[] items = null;

        //        // Get the latest version of the information for the items.
        //        ItemSet itemSet = sourceControl.GetItems(path, recursion);

        //        items = itemSet.Items;

        //        foreach (Item keyItem in items)
        //        {
        //            returnProjectFolder.Add(keyItem.ServerItem);
        //        }
        //    }
        //    catch (Exception ex)
        //    {
        //        string errr = ex.Message.ToString();
        //    }
        //    return returnProjectFolder;

        //}

        //public Collection<string> GetProjectFiles(String path)
        //{
        //    Collection<string> returnProjectFiles = new Collection<string>();
        //    try
        //    {
        //        NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
        //        Uri tfsUri = new Uri("http://tf_server:8080/tfs/Aria");

        //        TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(tfsUri, cre);

        //        sourceControl = (VersionControlServer)tfs.GetService(typeof(VersionControlServer));

        //        RecursionType recursion = RecursionType.Full;
        //        //Also have - Full, None
        //        Item[] items = null;

        //        // Get the latest version of the information for the items.
        //        ItemSet itemSet = sourceControl.GetItems(path, recursion);

        //        items = itemSet.Items;

        //        foreach (Item keyItem in items)
        //        {
        //            returnProjectFiles.Add(keyItem.ServerItem);
        //        }

        //    }
        //    catch (Exception ex)
        //    {
        //        string errr = ex.Message.ToString();
        //    }
        //    return returnProjectFiles;
        //}

        //public List<TeamFoundationIdentity> GetResourceList()
        //{
        //    TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(new Uri(TfsCollection), new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain));

        //     //1.Get the Idenitity Management Service
        //    IIdentityManagementService ims = tfs.GetService<IIdentityManagementService>();

        //     //2.Read the group represnting the root node
        //     TeamFoundationIdentity SIDS = ims.ReadIdentity(IdentitySearchFactor.AccountName,
        //         TfsGroupName, MembershipQuery.Direct, ReadIdentityOptions.None);


        //     List<string> ids = new List<string>();
        //     foreach (var member in SIDS.Members)
        //     {
        //         ids.Add(member.Identifier);
        //     }

        //     // get user objects for existing SIDS
        //     TeamFoundationIdentity[][] UserId = ims.ReadIdentities(IdentitySearchFactor.Identifier, ids.ToArray(), MembershipQuery.None, ReadIdentityOptions.ExtendedProperties);
        //     // convert to list
        //     List<TeamFoundationIdentity> UserIds = UserId.SelectMany(T => T).ToList();



        //     return UserIds;


        //}

        //public void GrantCheckoutFilePermission(String filepath, String AssignedResource)
        //{

        //   // Uri tfsUri = new Uri("http://tf_server:8080/tfs/DefaultCollection");
        //    NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);

        //    TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);

        //    VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;

        //    List<SecurityChange> changes = new List<SecurityChange>();

        //    changes.Add(new PermissionChange(filepath, AssignedResource, new string[] { "PendChange" }, null, null));

        //    SecurityChange[] actualChanges = vcs.SetPermissions(changes.ToArray());

        //}

        //public void DenyCheckoutFilePermission(String filepath, String AssignedResource)
        //{

        //    NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);

        //    TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);

        //    VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;

        //    List<SecurityChange> changes = new List<SecurityChange>();

        //    changes.Add(new PermissionChange(filepath, AssignedResource, null, new string[] { "PendChange" }, null));

        //    SecurityChange[] actualChanges = vcs.SetPermissions(changes.ToArray());

        //}

        //public int CommentCheckin(String filename, String assignedResource, String comment)
        //{

        //    NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
        //    TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(new Uri(TfsCollection), cre);


        //    sourceControl = (VersionControlServer)tfs.GetService(typeof(VersionControlServer));


        //    var test = sourceControl.GetWorkspace(TfsCheckinWorkSpace, TfsUserName);

        //    PendingChanges = test.GetPendingChanges();

        //    if (PendingChanges.Where(r => r.ServerItem.Trim().ToUpper() == filename.Trim().ToUpper()).Count() > 0)
        //    {
        //        return test.CheckIn(PendingChanges.Where(r => r.ServerItem.Trim().ToUpper() == filename.Trim().ToUpper()).ToArray(), comment);
        //    }

        //    return 0;
        //}



        //public void UpdateMetadataTFS(String xmlstring, String filename, ApplicationBuild_T build)
        //{

        //    //ApplicationSetting resourceFolder = build.Application_T.ApplicationSettings.First(r => r.SettingID == "Resouce Folder");
        //    //string resourcepathValue = resourceFolder.SettingValue;

        //    //string filename = entityName + "_MetaData.XML";

        //    NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
        //    TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(new Uri (TfsCollection), cre);


        //    sourceControl = (VersionControlServer)tfs.GetService(typeof(VersionControlServer));

        //    var ws = sourceControl.GetWorkspace(BuildWorkSapce, TfsUserName);

        //    //Checkout the Metadata.xml files
        //    //ws.PendEdit(resourcepathValue + entityName + "_MetaData.XML");
        //    ws.PendEdit(filename);


        //    //Download on the same path ?????
        //    //System.IO.File.WriteAllText(resourcepathValue + entityName + "_MetaData.XML", xmlstring);
        //    //System.IO.File.WriteAllText(filename, xmlstring);
        //    string physpth = TfsWorkSpacePath.Trim();
        //    if (!TfsWorkSpacePath.EndsWith("\\")) physpth += "\\";
        //    physpth += filename.Replace("$", "").Replace("/", "\\").Substring(TfsProjectName.Length + 2);

        //    System.IO.File.WriteAllText(physpth, xmlstring);

        //    //Get Pending Changes from Workspace
        //    PendingChanges = ws.GetPendingChanges();

        //    if (PendingChanges.Where(r => r.ServerItem.Trim().ToUpper() == filename.Trim().ToUpper()).Count() > 0)
        //    {
        //        //Checkin the need files after If Condition for Validating the Metadata files.
        //        int changesetNumber = ws.CheckIn(PendingChanges.Where(r => r.ServerItem.Trim().ToUpper() == filename.Trim().ToUpper()).ToArray(), build.TFSLabel);
        //    }


        //}

        //public void CreateWorkSpace(ApplicationBuild_T build) 
        //{

        //    //Check to Remove The String.sln at the end of the tfsProjectPath as example "$/New Test/App5"
        //    ApplicationSetting tfsMappingPath = build.Application_T.ApplicationSettings.First(r => r.SettingID.Trim() == "TFSMappingPath");
        //    string tfsMappingPathValue = tfsMappingPath.SettingValue;

        //    ApplicationSetting initLabelSetting = build.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "InitialLabel".Trim().ToUpper());
        //    string initLabel = initLabelSetting.SettingValue;

        //    ApplicationSetting lastLabelSetting = build.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "LastLabel".Trim().ToUpper());
        //    string lastLabel = lastLabelSetting.SettingValue;

        //    NetworkCredential networkCredential = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);

        //    TeamFoundationServer tfs = new TeamFoundationServer(TfsCollection, networkCredential);

        //    VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;

        //    // Creating TFS WorkSpace 
        //    vcs.CreateWorkspace(BuildWorkSapce, TfsUserName);

        //    var workspace = vcs.GetWorkspace(BuildWorkSapce, TfsUserName);

        //    // Mapping Source Code from ProjectPath to Physical Path at TFS WorkSpace 
        //    workspace.Map(tfsMappingPathValue, TfsWorkSpacePath); //@"D:\Dev\BuildWorkSpace");  //@"D:\Dev\BuildWorkSpace"

        //    //Get Latest Vr. of the Source Code 
        //    var LatestRequest = new GetRequest(new ItemSpec(tfsMappingPathValue, RecursionType.Full), VersionSpec.Latest);
        //    var results = workspace.Get(LatestRequest, GetOptions.GetAll | GetOptions.Overwrite);

        //    if (lastLabel == null || lastLabel.Trim() == "")
        //    {
        //        var LabelRequest = new GetRequest(new ItemSpec(tfsMappingPathValue, RecursionType.Full), new LabelVersionSpec(initLabel));// Mr. Mahmoud check where get the Old Label ????
        //        var LabelResults = workspace.Get(LabelRequest, GetOptions.GetAll | GetOptions.Overwrite);
        //    }
        //    else
        //    {
        //        var LabelRequest = new GetRequest(new ItemSpec(tfsMappingPathValue, RecursionType.Full), new LabelVersionSpec(lastLabel));// Mr. Mahmoud check where get the Old Label ????
        //        var LabelResults = workspace.Get(LabelRequest, GetOptions.GetAll | GetOptions.Overwrite);
        //    }
        //}

        //public void GetSpecificChangeset(ApplicationBuild_T build, int changeset) 
        //{

        //    //Check to Remove The String.sln at the end of the tfsProjectPath as example "$/New Test/App5"
        //    ApplicationSetting tfsMappingPath = build.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "TFSMappingPath".Trim().ToUpper());
        //    string tfsMappingPathValue = tfsMappingPath.SettingValue;

        //    NetworkCredential networkCredential = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);

        //    TeamFoundationServer tfs = new TeamFoundationServer(TfsCollection, networkCredential);

        //    VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;

        //    var workspace = vcs.GetWorkspace(BuildWorkSapce, TfsUserName);

        //    //Get Specific Change Set of the Source Code 
        //    var ChangeSetRequest = new GetRequest(new ItemSpec("$/1TouchAway/Aria.WindowsStore.OneTouchAway.View/Login/Login_1.xaml", RecursionType.None), new ChangesetVersionSpec(changeset));
        //    var ChangeSetResults = workspace.Get(ChangeSetRequest, GetOptions.GetAll | GetOptions.Overwrite);


        //}

        //public void CreateWorkSpaceLabel(ApplicationBuild_T build)
        //{

        //    //Check to Remove The String.sln at the end of the tfsProjectPath as example "$/New Test/App5"
        //    ApplicationSetting tfsMappingPath = build.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "TFSMappingPath".Trim().ToUpper());
        //    string tfsMappingPathValue = tfsMappingPath.SettingValue;

        //    ApplicationSetting currentBuildLable = build.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "CurrentBuildLabel".Trim().ToUpper());
        //    string currentBuildLablestring = currentBuildLable.SettingValue;

        //    NetworkCredential networkCredential = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);

        //    TeamFoundationServer tfs = new TeamFoundationServer(TfsCollection, networkCredential);

        //    VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
        //    //Creating Label From Prepeared WorkSpace with Specific Vr.
        //    VersionControlLabel labelToCreate = new VersionControlLabel(vcs, currentBuildLablestring, TfsUserName, "", ""); // Mr. Mahmoud check where get the New Label 
        //    LabelItemSpec[] labelItemSpecs = new LabelItemSpec[1];
        //    ItemSpec itemSpec = new ItemSpec(tfsMappingPathValue, RecursionType.Full);

        //    VersionSpec a;
        //    a = VersionSpec.ParseSingleSpec("W" + BuildWorkSapce, TfsUserName);

        //    labelItemSpecs[0] = new LabelItemSpec(itemSpec, a, false);
        //    vcs.CreateLabel(labelToCreate, labelItemSpecs, LabelChildOption.Replace);

        //    //Delete WorkSpace After Creating the Need Label.
        //    vcs.DeleteWorkspace(BuildWorkSapce, TfsUserName);
        //}

        ////public void CreateBuildDefinition(ApplicationBuild_T build)
        ////{
        ////    string TFSCollection = WebConfigurationManager.AppSettings["TFSCollection"].ToString();
        ////    string DropLocation = WebConfigurationManager.AppSettings["BuildDropLocation"].ToString();
        ////    string buildAgentName = WebConfigurationManager.AppSettings["buildAgentName"].ToString();
        ////    string buildControllerName = WebConfigurationManager.AppSettings["buildControllerName"].ToString();


        ////    ApplicationSetting mappingPath = build.Application_T.ApplicationSettings.First(r => r.SettingID == "TFSMappingPath");
        ////    string mappingPathValue = mappingPath.SettingValue;
        ////    string mappingDropPathValue = mappingPathValue +"\\Drops";

        ////    ApplicationSetting tfsProjectPath = build.Application_T.ApplicationSettings.First(r => r.SettingID == "TFS Project Path");
        ////    string tfsProjectPathValue = tfsProjectPath.SettingValue;

        ////    ApplicationSetting tfsProjectName = build.Application_T.ApplicationSettings.First(r => r.SettingID == "TFS Main Project name");
        ////    string projectname = tfsProjectName.SettingValue;


        ////    TeamFoundationServer tfs = new TeamFoundationServer(TFSCollection);
        ////    IBuildServer buildServer = (IBuildServer)tfs.GetService(typeof(IBuildServer));

        ////    //Create build definition and give it a name and desription
        ////    IBuildDefinition buildDef = buildServer.CreateBuildDefinition(projectname);
        ////    buildDef.Name = "BuildDef# " + build.ApplicationBuildId;
        ////    buildDef.Description = build.Application_T.Description;
        ////    buildDef.ContinuousIntegrationType = ContinuousIntegrationType.None;


        ////    //Controller and default build process template
        ////    buildDef.BuildController = buildServer.GetBuildController(buildControllerName);
        ////    var defaultTemplate = buildServer.QueryProcessTemplates(projectname).First(p => p.TemplateType == ProcessTemplateType.Default);
        ////    buildDef.Process = defaultTemplate;


        ////    //Drop location
        ////    buildDef.DefaultDropLocation = DropLocation;//"\\\\TF_SERVER\\Builds";    

        ////    //Source Settings
        ////    buildDef.Workspace.AddMapping(mappingPathValue, "$(SourceDir)", WorkspaceMappingType.Map);
        ////    buildDef.Workspace.AddMapping(mappingDropPathValue, "", WorkspaceMappingType.Cloak);



        ////    //Process params
        ////    var process = WorkflowHelpers.DeserializeProcessParameters(buildDef.ProcessParameters);

        ////    //What to build

        ////    process.Add("ProjectsToBuild", new[] { tfsProjectPathValue });
        ////    process.Add("ConfigurationsToBuild", new[] { "" });

        ////    buildDef.ProcessParameters = WorkflowHelpers.SerializeProcessParameters(process);

        ////    //Retention policy
        ////    buildDef.RetentionPolicyList.Clear();
        ////    buildDef.AddRetentionPolicy(BuildReason.Triggered, BuildStatus.Succeeded, 10, DeleteOptions.All);
        ////    buildDef.AddRetentionPolicy(BuildReason.Triggered, BuildStatus.Failed, 10, DeleteOptions.All);
        ////    buildDef.AddRetentionPolicy(BuildReason.Triggered, BuildStatus.Stopped, 1, DeleteOptions.All);
        ////    buildDef.AddRetentionPolicy(BuildReason.Triggered, BuildStatus.PartiallySucceeded, 10, DeleteOptions.All);

        ////    //Lets save it
        ////    buildDef.Save();


        ////}

        //public void BuildDeploy(ApplicationBuild_T build, String buildDefinitionName)
        //{

        //    ApplicationSetting currentLabelSetting = build.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "CurrentBuildLabel".Trim().ToUpper());
        //    string currentLabel = currentLabelSetting.SettingValue;

        //    ApplicationSetting tfsProjectName = build.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "TFS Main Project name".Trim().ToUpper());
        //    string projectname = tfsProjectName.SettingValue;

        //    TeamFoundationServer tfs = TeamFoundationServerFactory.GetServer(TfsCollection);


        //    IBuildServer buildServer = (IBuildServer)tfs.GetService(typeof(IBuildServer));
        //    IBuildDefinition buildDef = buildServer.GetBuildDefinition(projectname, buildDefinitionName);
        //    var queuedBuild = buildServer.QueueBuild(buildDef);

        //    ((IBuildDetail)queuedBuild.Builds[0]).SourceGetVersion = "L" + currentLabel; // "LNewBuildLabel";//InitialLabelvalue;


        //    queuedBuild.Builds[0].Save();
        //}

        # endregion
    }
}