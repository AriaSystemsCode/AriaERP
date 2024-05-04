using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Services;
using Microsoft.TeamFoundation.Client;
using Microsoft.TeamFoundation.Common;
using Microsoft.TeamFoundation.VersionControl;
using Microsoft.TeamFoundation;
using Microsoft.TeamFoundation.Server;
using Microsoft.TeamFoundation.VersionControl.Client;
using System.Net;
using Microsoft.TeamFoundation.Build.Client;
using System.Collections.ObjectModel;
using Microsoft.TeamFoundation.Framework.Client;
using Microsoft.TeamFoundation.Framework.Common;
using System.Web.Configuration;
using System.IO;
using System.Security.AccessControl;
using Microsoft.Azure.NotificationHubs;
using System.Data;
using System.Data.OleDb;
using System.Text;
//using ArrayToMemo;
using WebApplication1.Classes;
using System.Threading;


namespace WebApplication1
{
    /// <summary>
    /// Summary description for WebService1
    /// </summary>
    [WebService(Namespace = "http://tempuri.org/")]
    [WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
    [System.ComponentModel.ToolboxItem(false)]
    // To allow this Web Service to be called from script, using ASP.NET AJAX, uncomment the following line. 
    // [System.Web.Script.Services.ScriptService]
    public class WebService1 : System.Web.Services.WebService
    {

        [WebMethod]
        public string HelloWorld()
        {
            return "Hello World";
        }

        [WebMethod]
        public  void SetParameters(string tfsUserName, string Password, string Domain, string projectName, string GroupName, string tfsCollection, string checkInWorkspace, string buildWorkSapce, string tfsWorkSpacePath)
        {
            TfsUserName = tfsUserName;
            TfsPassWord = Password;
            TfsDomain = Domain;
            TfsProjectName = projectName;
            TfsGroupName = GroupName;
            TfsCollection = tfsCollection;
            BuildWorkSapce = buildWorkSapce;
            TfsWorkSpacePath = tfsWorkSpacePath;
            TfsCheckinWorkSpace = checkInWorkspace;
        }
        
        # region Declarations
            public ProjectInfo[] projects;
            VersionControlServer sourceControl;
            public string path = string.Empty;
            public string filepath = string.Empty;
            public string AssignedResource = string.Empty;
            public Workspace workspace;
            public PendingChange[] PendingChanges;
           // public string TfsWorkSpacePath;
          //  public string BuildWorkSapce;
    # endregion

        # region Properties
        public static Uri TfsUri
        {
            set;
            get;

        }
        public static string TfsUserName
        {
            set;
            get;

        }
        public static string TfsPassWord
        {
            set;
            get;

        }
        public static string TfsDomain
        {
            set;
            get;

        }
        public static string TfsProjectName
        {
            set;
            get;

        }
        public static string TfsGroupName
        {
            set;
            get;

        }

        public static string TfsCollection
        {
            set;
            get;

        }

        public static string BuildWorkSapce
        {
            set;
            get;

        }
        public static string TfsWorkSpacePath
        {
            set;
            get;
        }
        public static string TfsCheckinWorkSpace
        {
            set;
            get;
        }
        # endregion

        # region Tracking 
        [WebMethod]
        public void GrantCheckoutFilePermission(String filepath, String AssignedResource)
        {
            if (filepath.Contains("ARIA4XP") || filepath.Contains("EDI"))
            {
                TfsCollection = "http://tf_server:8080/tfs/Aria-Fox";
            }
            Uri tfsUri = new Uri(TfsCollection);
            //System.IO.File.WriteAllText("D:\\Checkout.TXT", tfsUri.ToString());

            NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
            TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(tfsUri, cre);
            VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
            List<SecurityChange> changes = new List<SecurityChange>();
            changes.Add(new PermissionChange(filepath, AssignedResource, new string[] { "PendChange" }, null, null));
            SecurityChange[] actualChanges = vcs.SetPermissions(changes.ToArray());
            //System.IO.File.WriteAllText("D:\\Checkoutdone.TXT", "Done");

        }
        [WebMethod]
        public void DenyCheckoutFilePermission(String filepath, String AssignedResource)
        {
            //Uri TfsUri = new Uri("http://tf_server:8080/tfs/Aria_Dev-2015");
            if (filepath.Contains("ARIA4XP") || filepath.Contains("EDI"))
            {
                TfsCollection = "http://tf_server:8080/tfs/Aria-Fox";
            }
            Uri TfsUri = new Uri(TfsCollection);
            NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
            TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);
            VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
            List<SecurityChange> changes = new List<SecurityChange>();
            changes.Add(new PermissionChange(filepath, AssignedResource, null, new string[] { "PendChange" }, null));
            SecurityChange[] actualChanges = vcs.SetPermissions(changes.ToArray());
        }
        [WebMethod]
        //ATA modify function return type to return changeset value [start ]
        public string  CommentCheckin(string ProjectPath, String Shelvset, String AssignedResource, String Comment)
        {
            if (ProjectPath.ToUpper ().Contains("ARIA4XP") || ProjectPath.ToUpper ().Contains("EDI"))
            {
                TfsCollection = "http://tf_server:8080/tfs/Aria-Fox";
            }
        

            string NewWorkSpace = "";
            Uri TfsUri = new Uri(TfsCollection);
            NewWorkSpace = @"D:\Dev\" + TfsCheckinWorkSpace;
            NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
            TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);
            VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
            if (vcs.TryGetWorkspace(NewWorkSpace) == null)
            {
                vcs.CreateWorkspace(TfsCheckinWorkSpace, TfsUserName);
            }
            var workspace = vcs.GetWorkspace(TfsCheckinWorkSpace, TfsUserName);
            string MapDir = "";
            if (ProjectPath.ToUpper().Contains("ARIA4XP"))
                MapDir ="$//Aria4XP";
            else
                MapDir = "$//EDI";
            //
            //workspace.Map(ProjectPath, NewWorkSpace);
            workspace.Map(MapDir, NewWorkSpace);
            //
            var LatestRequest = new GetRequest(new ItemSpec(MapDir, RecursionType.Full), VersionSpec.Latest);
            var results = workspace.Get(LatestRequest, GetOptions.GetAll | GetOptions.Overwrite);
            workspace.Unshelve(Shelvset, AssignedResource);
            PendingChanges = workspace.GetPendingChanges();
            var returnval = workspace.CheckIn(PendingChanges, Comment);
            vcs.DeleteWorkspace(TfsCheckinWorkSpace, TfsUserName);
            return returnval.ToString();
        }

        # endregion

        # region Build
        [WebMethod]
        public void CreateWorkSpace(string tfsMappingPathValue, string initLabel, string lastLabel)
        {   //  TfsWorkSpacePath = @"D:\Dev\BuildWorkSpace\";
            // BuildWorkSapce = "BuildWorkSpace";
            NetworkCredential networkCredential = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
            TeamFoundationServer tfs = new TeamFoundationServer(TfsCollection, networkCredential);
            VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
           // System.IO.DirectoryInfo dir = new System.IO.DirectoryInfo(TfsWorkSpacePath);
         //  dir.Attributes = FileAttributes.Normal;
            if (vcs.TryGetWorkspace(BuildWorkSapce) != null)
            {
                vcs.DeleteWorkspace(BuildWorkSapce, TfsUserName);
            }
            //if (dir.Exists)
            //{
            //    //foreach (var item in dir.GetFiles())
            //    //{
            //    //    item.Attributes &= ~FileAttributes.ReadOnly;
            //    //   // item.IsReadOnly = false;
            //    //}
            //    dir.Attributes &= ~FileAttributes.ReadOnly;
            //    dir.Delete(true);
            //}
         
            // Creating TFS WorkSpace Khaled Chec
            if (vcs.TryGetWorkspace(BuildWorkSapce) == null)
            {
                vcs.CreateWorkspace(BuildWorkSapce, TfsUserName);
            }
            var workspace = vcs.GetWorkspace(BuildWorkSapce, TfsUserName);
            // Mapping Source Code from ProjectPath to Physical Path at TFS WorkSpace 
            workspace.Map(tfsMappingPathValue, TfsWorkSpacePath); //@"D:\Dev\BuildWorkSpace");  //@"D:\Dev\BuildWorkSpace"
            //Get Latest Vr. of the Source Code 
            var LatestRequest = new GetRequest(new ItemSpec(tfsMappingPathValue, RecursionType.Full), VersionSpec.Latest);
            var results = workspace.Get(LatestRequest, GetOptions.GetAll | GetOptions.Overwrite);
            if (lastLabel == null || lastLabel.Trim() == "")
            {
                var LabelRequest = new GetRequest(new ItemSpec(tfsMappingPathValue, RecursionType.Full), new LabelVersionSpec(initLabel));// Mr. Mahmoud check where get the Old Label ????
                var LabelResults = workspace.Get(LabelRequest, GetOptions.GetAll | GetOptions.Overwrite);
            }
            else
            {
                var LabelRequest = new GetRequest(new ItemSpec(tfsMappingPathValue, RecursionType.Full), new LabelVersionSpec(lastLabel));// Mr. Mahmoud check where get the Old Label ????
                var LabelResults = workspace.Get(LabelRequest, GetOptions.GetAll | GetOptions.Overwrite);
            }
        }
        [WebMethod]
        public void GetSpecificChangeset(string tfsMappingPathValue, int changeset, string StorfgefilePath)
        {

            NetworkCredential networkCredential = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);

            TeamFoundationServer tfs = new TeamFoundationServer(TfsCollection, networkCredential);

            VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;

            var workspace = vcs.GetWorkspace(BuildWorkSapce, TfsUserName);

            //Get Specific Change Set of the Source Code 
            // Bug
            //var ChangeSetRequest = new GetRequest(new ItemSpec("$/1TouchAway/Aria.WindowsStore.OneTouchAway.View/Login/Login_1.xaml", RecursionType.None), new ChangesetVersionSpec(changeset));
            var ChangeSetRequest = new GetRequest(new ItemSpec(StorfgefilePath, RecursionType.None), new ChangesetVersionSpec(changeset));

            var ChangeSetResults = workspace.Get(ChangeSetRequest, GetOptions.GetAll | GetOptions.Overwrite);


        }
        [WebMethod]
        public void CreateWorkSpaceLabel(string tfsMappingPathValue, string currentBuildLablestring)
        {

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
        //    System.IO.DirectoryInfo dir = new System.IO.DirectoryInfo(TfsWorkSpacePath);
          
         //   dir.Attributes = FileAttributes.Normal;

            //if (dir.Exists)
            //{
            //    foreach (var item in dir.GetFiles())
            //    {
            //        item.Attributes &= ~FileAttributes.ReadOnly;
            //        // item.IsReadOnly = false;
            //    }
            //    dir.Attributes &= ~FileAttributes.ReadOnly;
            //    dir.Delete(true);
            //    Directory.GetAccessControl(TfsWorkSpacePath);
            //    Directory.Delete(TfsWorkSpacePath, true);
            //}
            //----------------
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
        }

        [WebMethod]
        public void BuildDeploy(string currentLabel, string projectname, String buildDefinitionName)
        {

            //ApplicationSetting currentLabelSetting = build.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "CurrentBuildLabel".Trim().ToUpper());
            //string currentLabel = currentLabelSetting.SettingValue;

            //ApplicationSetting tfsProjectName = build.Application_T.ApplicationSettings.First(r => r.SettingID.Trim().ToUpper() == "TFS Main Project name".Trim().ToUpper());
            //string projectname = tfsProjectName.SettingValue;

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

        # endregion


        # region Commented
        //  [WebMethod]
        //  public void SetParameters(string tfsUserName, string Password, string Domain, string projectName, string GroupName, string tfsCollection, string checkInWorkspace, string buildWorkSapce,string tfsWorkSpacePath)
        //    {
        //     TfsUserName = tfsUserName;
        //   TfsPassWord = Password;
        //  TfsDomain = Domain;
        //TfsProjectName = projectName;
        // TfsGroupName = GroupName;
        //TfsCollection = tfsCollection;
        //BuildWorkSapce = buildWorkSapce;
        //TfsWorkSpacePath = tfsWorkSpacePath;
        //TfsCheckinWorkSpace = checkInWorkspace;
        //TfsUri = new Uri(tfsUri);
        //TfsUserName = tfsUserName;
        //TfsPassWord = tfsPassWord;
        //TfsDomain = tfsDomain;
        //TfsProjectName = tfsProjectName;
        ////TfsGroupName = tfsGroupName;
        //         }

        //public TfsManager()
        //{

        //TfsUri = new Uri(WebConfigurationManager.AppSettings["tfsUri"].ToString());
        //TfsUserName = WebConfigurationManager.AppSettings["tfsUserName"].ToString();
        //TfsPassWord = WebConfigurationManager.AppSettings["tfsPassWord"].ToString();
        //TfsDomain = WebConfigurationManager.AppSettings["tfsDomain"].ToString();
        //TfsProjectName = WebConfigurationManager.AppSettings["tfsProjectName"].ToString();
        //TfsGroupName = WebConfigurationManager.AppSettings["tfsGroupName"].ToString();
        //TfsCollection = WebConfigurationManager.AppSettings["TFSCollection"].ToString();
        //BuildWorkSapce = WebConfigurationManager.AppSettings["BuildWorkSpace"].ToString();
        //TfsWorkSpacePath = WebConfigurationManager.AppSettings["tfsWorkSpacePath"].ToString();
        //TfsCheckinWorkSpace = WebConfigurationManager.AppSettings["tfsCheckinWorkSpaceCreated"].ToString();

        //TfsUri = new Uri(tfsUri);
        //TfsUserName = tfsUserName;
        //TfsPassWord = tfsPassWord;
        //TfsDomain = tfsDomain;
        //TfsProjectName = tfsProjectName;
        ////TfsGroupName = tfsGroupName;
        //}
        //public WebService1(string tfsUserName, string Password, string Domain, string projectName, string GroupName, string tfsCollection, string checkInWorkspace)
        //{
        //    TfsUserName = tfsUserName;
        //    TfsPassWord = Password;
        //    TfsDomain = Domain;
        //    TfsProjectName = projectName;
        //    TfsGroupName = GroupName;
        //    TfsCollection = tfsCollection;
        //   BuildWorkSapce = WebConfigurationManager.AppSettings["BuildWorkSpace"].ToString();
        //    // TfsWorkSpacePath = WebConfigurationManager.AppSettings["tfsWorkSpacePath"].ToString();
        //    TfsCheckinWorkSpace = checkInWorkspace;

        //    //TfsUri = new Uri(tfsUri);
        //    //TfsUserName = tfsUserName;
        //    //TfsPassWord = tfsPassWord;
        //    //TfsDomain = tfsDomain;
        //    //TfsProjectName = tfsProjectName;
        //    //TfsGroupName = tfsGroupName;
        //}
        //public TfsManager()
        //{

        //TfsUri = new Uri(WebConfigurationManager.AppSettings["tfsUri"].ToString());
        //TfsUserName = WebConfigurationManager.AppSettings["tfsUserName"].ToString();
        //TfsPassWord = WebConfigurationManager.AppSettings["tfsPassWord"].ToString();
        //TfsDomain = WebConfigurationManager.AppSettings["tfsDomain"].ToString();
        //TfsProjectName = WebConfigurationManager.AppSettings["tfsProjectName"].ToString();
        //TfsGroupName = WebConfigurationManager.AppSettings["tfsGroupName"].ToString();
        //TfsCollection = WebConfigurationManager.AppSettings["TFSCollection"].ToString();
        //BuildWorkSapce = WebConfigurationManager.AppSettings["BuildWorkSpace"].ToString();
        //TfsWorkSpacePath = WebConfigurationManager.AppSettings["tfsWorkSpacePath"].ToString();
        //TfsCheckinWorkSpace = WebConfigurationManager.AppSettings["tfsCheckinWorkSpaceCreated"].ToString();

        //TfsUri = new Uri(tfsUri);
        //TfsUserName = tfsUserName;
        //TfsPassWord = tfsPassWord;
        //TfsDomain = tfsDomain;
        //TfsProjectName = tfsProjectName;
        ////TfsGroupName = tfsGroupName;
        //}
        // Sara.N 19/09/2015 QA Control TFS [ChekOut and CheckIn] Using System Admin [Start]
        //[WebMethod]
        //public void GrantCheckoutFilePermission(String filepath, String AssignedResource)
        //{

        //    Uri tfsUri = new Uri(TfsCollection);
        //    NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
        //    TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(tfsUri, cre);
        //    VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
        //    List<SecurityChange> changes = new List<SecurityChange>();
        //    changes.Add(new PermissionChange(filepath, AssignedResource, new string[] { "PendChange" }, null, null));
        //    SecurityChange[] actualChanges = vcs.SetPermissions(changes.ToArray());
        //}
        //[WebMethod]
        //public void DenyCheckoutFilePermission(String filepath, String AssignedResource)
        //{
        //    //Uri TfsUri = new Uri("http://tf_server:8080/tfs/Aria_Dev-2015");
        //    Uri TfsUri = new Uri(TfsCollection);
        //    NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
        //    TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);
        //    VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
        //    List<SecurityChange> changes = new List<SecurityChange>();
        //    changes.Add(new PermissionChange(filepath, AssignedResource, null, new string[] { "PendChange" }, null));
        //    SecurityChange[] actualChanges = vcs.SetPermissions(changes.ToArray());
        //}
        //[WebMethod]
        //public void CommentCheckin(string ProjectPath, String Shelvset, String AssignedResource, String Comment)
        //{
        //    string NewWorkSpace = "";
        //    Uri TfsUri = new Uri(TfsCollection);
        //    NewWorkSpace = @"D:\Dev\" + TfsCheckinWorkSpace;
        //    NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
        //    TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);
        //    VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
        //    if (vcs.TryGetWorkspace(NewWorkSpace) == null)
        //    {
        //        vcs.CreateWorkspace(TfsCheckinWorkSpace, TfsUserName);
        //    }
        //    var workspace = vcs.GetWorkspace(TfsCheckinWorkSpace, TfsUserName);
        //    workspace.Map(ProjectPath, NewWorkSpace);
        //    var LatestRequest = new GetRequest(new ItemSpec(ProjectPath, RecursionType.Full), VersionSpec.Latest);
        //    var results = workspace.Get(LatestRequest, GetOptions.GetAll | GetOptions.Overwrite);
        //    workspace.Unshelve(Shelvset, AssignedResource);
        //    PendingChanges = workspace.GetPendingChanges();
        //    var returnval = workspace.CheckIn(PendingChanges, Comment);
        //    vcs.DeleteWorkspace(TfsCheckinWorkSpace, TfsUserName);
        //}



        //    public ProjectInfo[] projects;
        //    VersionControlServer sourceControl;
        //    public string path = string.Empty;
        //    public string filepath = string.Empty;
        //    public string AssignedResource = string.Empty;
        //    public Workspace workspace;
        //    public PendingChange[] PendingChanges;

        //    public Uri TfsUri = new Uri("http://tf_server:8080/tfs/");
        //    public string TfsUserName = "ProjectAdmin";
        //    public  string TfsPassWord = "aria_123";
        //    public  string TfsDomain = "TF_Server";
        //    public string TfsProjectName = "1touchaway";
        //    public  string TfsGroupName = "1touchaway Team";




        //    //[WebMethod]
        //    //public TfsHandler(string tfsUri, string tfsUserName, string tfsPassWord, string tfsDomain, string tfsProjectName, string tfsGroupName)
        //    //{


        //    //    TfsUri = new Uri(tfsUri);
        //    //    TfsUserName = tfsUserName;
        //    //    TfsPassWord = tfsPassWord;
        //    //    TfsDomain = tfsDomain;
        //    //    TfsProjectName = tfsProjectName;
        //    //    TfsGroupName = tfsGroupName;

        //    //    return ;

        //    //}

        //[WebMethod]
        //    public Collection<string> GetTeamCollectionList()
        //    {
        //        Collection<string> returnTeamCollectionList = new Collection<string>();

        //        // Uri tfsUri = new Uri("http://tf_server:8080/tfs/");
        //        //Uri tfsUri = new Uri("http://41.41.15.230:171/tfs/");



        //        TfsConfigurationServer configServer = new TfsConfigurationServer(TfsUri, new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain));

        //        CatalogNode rootNode = configServer.CatalogNode;


        //        ReadOnlyCollection<CatalogNode> teamProjectCollections = rootNode.QueryChildren(new Guid[] { CatalogResourceTypes.ProjectCollection }, false, CatalogQueryOptions.None);
        //        CatalogNode teamProjectCollection = teamProjectCollections[0];

        //        var teamProjects = teamProjectCollection.QueryChildren(new Guid[] { CatalogResourceTypes.TeamProject }, false, CatalogQueryOptions.None);

        //        foreach (var item in teamProjects.ToList())
        //        {
        //            returnTeamCollectionList.Add(item.Resource.DisplayName.ToString());


        //        }

        //        sourceControl = (VersionControlServer)configServer.GetService(typeof(VersionControlServer));

        //        return returnTeamCollectionList;

        //    }

        //[WebMethod]
        //    public Collection<string> GetProjectFolders(String path)
        //    {
        //        Collection<string> returnProjectFolder = new Collection<string>();

        //        try
        //        {
        //            NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
        //            Uri tfsUri = new Uri("http://tf_server:8080/tfs/DefaultCollection");
        //            // Uri tfsUri = new Uri("http://197.44.68.247:171/tfs/DefaultCollection"); 
        //            TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(tfsUri, cre);
        //            sourceControl = (VersionControlServer)tfs.GetService(typeof(VersionControlServer));

        //            RecursionType recursion = RecursionType.OneLevel;

        //            Item[] items = null;

        //            // Get the latest version of the information for the items.
        //            ItemSet itemSet = sourceControl.GetItems(path, recursion);

        //            items = itemSet.Items;

        //            foreach (Item keyItem in items)
        //            {
        //                returnProjectFolder.Add(keyItem.ServerItem);
        //            }
        //        }
        //        catch (Exception ex)
        //        {
        //            string errr = ex.Message.ToString();
        //        }
        //        return returnProjectFolder;

        //    }

        //[WebMethod]
        //    public Collection<string> GetProjectFiles(String path)
        //    {
        //        Collection<string> returnProjectFiles = new Collection<string>();
        //        try
        //        {
        //            NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
        //            Uri tfsUri = new Uri("http://tf_server:8080/tfs/DefaultCollection");
        //            //   Uri tfsUri = new Uri("http://197.44.68.247:171/tfs/DefaultCollection");   
        //            TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(tfsUri, cre);

        //            sourceControl = (VersionControlServer)tfs.GetService(typeof(VersionControlServer));

        //            RecursionType recursion = RecursionType.Full;
        //            //Also have - Full, None
        //            Item[] items = null;

        //            // Get the latest version of the information for the items.
        //            ItemSet itemSet = sourceControl.GetItems(path, recursion);

        //            items = itemSet.Items;

        //            foreach (Item keyItem in items)
        //            {
        //                returnProjectFiles.Add(keyItem.ServerItem);
        //            }

        //        }
        //        catch (Exception ex)
        //        {
        //            string errr = ex.Message.ToString();
        //        }
        //        return returnProjectFiles;
        //    }

        //[WebMethod]
        //    public List<TeamFoundationIdentity> GetResourceList(String projectUri)
        //    {

        //        TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(new Uri(projectUri), new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain));

        //            //1.Get the Idenitity Management Service
        //        IIdentityManagementService ims = tfs.GetService<IIdentityManagementService>();

        //            //2.Read the group represnting the root node
        //            TeamFoundationIdentity SIDS = ims.ReadIdentity(IdentitySearchFactor.AccountName,
        //                TfsGroupName, MembershipQuery.Direct, ReadIdentityOptions.None);


        //            List<string> ids = new List<string>();
        //            foreach (var member in SIDS.Members)
        //            {
        //                ids.Add(member.Identifier);
        //            }

        //            // get user objects for existing SIDS
        //            TeamFoundationIdentity[][] UserId = ims.ReadIdentities(IdentitySearchFactor.Identifier, ids.ToArray(), MembershipQuery.None, ReadIdentityOptions.ExtendedProperties);
        //            // convert to list
        //            List<TeamFoundationIdentity> UserIds = UserId.SelectMany(T => T).ToList();



        //            return UserIds;


        //    }

        //        [WebMethod]
        //public void GrantCheckoutFilePermission(String filepath, String AssignedResource)
        //{

        //    // Uri tfsUri = new Uri("http://41.41.15.230:171/tfs/Aria");
        //    NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);

        //    TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);

        //    VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;

        //    List<SecurityChange> changes = new List<SecurityChange>();

        //    changes.Add(new PermissionChange(filepath, AssignedResource, new string[] { "PendChange" }, null, null));

        //    SecurityChange[] actualChanges = vcs.SetPermissions(changes.ToArray());



        //}

        //        [WebMethod]

        //        public void DenyCheckoutFilePermission(String filepath, String AssignedResource)
        //        {
        //            //Uri tfsUri = new Uri("http://41.41.15.230:171/tfs/Aria");

        //            NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);

        //            TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);

        //            VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;

        //            List<SecurityChange> changes = new List<SecurityChange>();

        //            changes.Add(new PermissionChange(filepath, AssignedResource, null, new string[] { "PendChange" }, null));

        //            SecurityChange[] actualChanges = vcs.SetPermissions(changes.ToArray());

        //        }

        //        [WebMethod]

        //        public void CommentCheckin(String Shelvset, String AssignedResource, String Comment)
        //        {





        //            // Uri tfsUri = new Uri("http://41.41.15.230:171/tfs/DefaultCollection");
        //            Uri tfsUri = new Uri("http://tf_server:8080/tfs/DefaultCollection");
        //            string ProjectPath = "$/New Test/App5";

        //            NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
        //            TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(tfsUri, cre);


        //            VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;


        //            vcs.CreateWorkspace("CheckinWorkSpace", "ProjectAdmin");


        //            var workspace = vcs.GetWorkspace("CheckinWorkSpace", "ProjectAdmin");

        //            workspace.Map(ProjectPath, @"D:\Dev\CheckinWorkSpace");
        //            var LatestRequest = new GetRequest(new ItemSpec(ProjectPath, RecursionType.Full), VersionSpec.Latest);
        //            var results = workspace.Get(LatestRequest, GetOptions.GetAll | GetOptions.Overwrite);

        //            workspace.Unshelve(Shelvset, AssignedResource);
        //            PendingChanges = workspace.GetPendingChanges();


        //            var returnval = workspace.CheckIn(PendingChanges, Comment);

        //            vcs.DeleteWorkspace("CheckinWorkSpace", "ProjectAdmin");



        //        }








        //    public ProjectInfo[] projects;
        //    VersionControlServer sourceControl;
        //    public string path = string.Empty;
        //    public string filepath = string.Empty;
        //    public string AssignedResource = string.Empty;
        //    public Workspace workspace;
        //    public PendingChange[] PendingChanges;

        //    public Uri TfsUri = new Uri("http://tf_server:8080/tfs/");
        //    public string TfsUserName = "ProjectAdmin";
        //    public  string TfsPassWord = "aria_123";
        //    public  string TfsDomain = "TF_Server";
        //    public string TfsProjectName = "1touchaway";
        //    public  string TfsGroupName = "1touchaway Team";




        //    //[WebMethod]
        //    //public TfsHandler(string tfsUri, string tfsUserName, string tfsPassWord, string tfsDomain, string tfsProjectName, string tfsGroupName)
        //    //{


        //    //    TfsUri = new Uri(tfsUri);
        //    //    TfsUserName = tfsUserName;
        //    //    TfsPassWord = tfsPassWord;
        //    //    TfsDomain = tfsDomain;
        //    //    TfsProjectName = tfsProjectName;
        //    //    TfsGroupName = tfsGroupName;

        //    //    return ;

        //    //}

        //[WebMethod]
        //    public Collection<string> GetTeamCollectionList()
        //    {
        //        Collection<string> returnTeamCollectionList = new Collection<string>();

        //        // Uri tfsUri = new Uri("http://tf_server:8080/tfs/");
        //        //Uri tfsUri = new Uri("http://41.41.15.230:171/tfs/");



        //        TfsConfigurationServer configServer = new TfsConfigurationServer(TfsUri, new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain));

        //        CatalogNode rootNode = configServer.CatalogNode;


        //        ReadOnlyCollection<CatalogNode> teamProjectCollections = rootNode.QueryChildren(new Guid[] { CatalogResourceTypes.ProjectCollection }, false, CatalogQueryOptions.None);
        //        CatalogNode teamProjectCollection = teamProjectCollections[0];

        //        var teamProjects = teamProjectCollection.QueryChildren(new Guid[] { CatalogResourceTypes.TeamProject }, false, CatalogQueryOptions.None);

        //        foreach (var item in teamProjects.ToList())
        //        {
        //            returnTeamCollectionList.Add(item.Resource.DisplayName.ToString());


        //        }

        //        sourceControl = (VersionControlServer)configServer.GetService(typeof(VersionControlServer));

        //        return returnTeamCollectionList;

        //    }

        //[WebMethod]
        //    public Collection<string> GetProjectFolders(String path)
        //    {
        //        Collection<string> returnProjectFolder = new Collection<string>();

        //        try
        //        {
        //            NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
        //            Uri tfsUri = new Uri("http://tf_server:8080/tfs/DefaultCollection");
        //            // Uri tfsUri = new Uri("http://197.44.68.247:171/tfs/DefaultCollection"); 
        //            TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(tfsUri, cre);
        //            sourceControl = (VersionControlServer)tfs.GetService(typeof(VersionControlServer));

        //            RecursionType recursion = RecursionType.OneLevel;

        //            Item[] items = null;

        //            // Get the latest version of the information for the items.
        //            ItemSet itemSet = sourceControl.GetItems(path, recursion);

        //            items = itemSet.Items;

        //            foreach (Item keyItem in items)
        //            {
        //                returnProjectFolder.Add(keyItem.ServerItem);
        //            }
        //        }
        //        catch (Exception ex)
        //        {
        //            string errr = ex.Message.ToString();
        //        }
        //        return returnProjectFolder;

        //    }

        //[WebMethod]
        //    public Collection<string> GetProjectFiles(String path)
        //    {
        //        Collection<string> returnProjectFiles = new Collection<string>();
        //        try
        //        {
        //            NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
        //            Uri tfsUri = new Uri("http://tf_server:8080/tfs/DefaultCollection");
        //            //   Uri tfsUri = new Uri("http://197.44.68.247:171/tfs/DefaultCollection");   
        //            TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(tfsUri, cre);

        //            sourceControl = (VersionControlServer)tfs.GetService(typeof(VersionControlServer));

        //            RecursionType recursion = RecursionType.Full;
        //            //Also have - Full, None
        //            Item[] items = null;

        //            // Get the latest version of the information for the items.
        //            ItemSet itemSet = sourceControl.GetItems(path, recursion);

        //            items = itemSet.Items;

        //            foreach (Item keyItem in items)
        //            {
        //                returnProjectFiles.Add(keyItem.ServerItem);
        //            }

        //        }
        //        catch (Exception ex)
        //        {
        //            string errr = ex.Message.ToString();
        //        }
        //        return returnProjectFiles;
        //    }

        //[WebMethod]
        //    public List<TeamFoundationIdentity> GetResourceList(String projectUri)
        //    {

        //        TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(new Uri(projectUri), new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain));

        //            //1.Get the Idenitity Management Service
        //        IIdentityManagementService ims = tfs.GetService<IIdentityManagementService>();

        //            //2.Read the group represnting the root node
        //            TeamFoundationIdentity SIDS = ims.ReadIdentity(IdentitySearchFactor.AccountName,
        //                TfsGroupName, MembershipQuery.Direct, ReadIdentityOptions.None);


        //            List<string> ids = new List<string>();
        //            foreach (var member in SIDS.Members)
        //            {
        //                ids.Add(member.Identifier);
        //            }

        //            // get user objects for existing SIDS
        //            TeamFoundationIdentity[][] UserId = ims.ReadIdentities(IdentitySearchFactor.Identifier, ids.ToArray(), MembershipQuery.None, ReadIdentityOptions.ExtendedProperties);
        //            // convert to list
        //            List<TeamFoundationIdentity> UserIds = UserId.SelectMany(T => T).ToList();



        //            return UserIds;


        //    }

        //        [WebMethod]
        //public void GrantCheckoutFilePermission(String filepath, String AssignedResource)
        //{

        //    // Uri tfsUri = new Uri("http://41.41.15.230:171/tfs/Aria");
        //    NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);

        //    TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);

        //    VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;

        //    List<SecurityChange> changes = new List<SecurityChange>();

        //    changes.Add(new PermissionChange(filepath, AssignedResource, new string[] { "PendChange" }, null, null));

        //    SecurityChange[] actualChanges = vcs.SetPermissions(changes.ToArray());



        //}

        //        [WebMethod]

        //        public void DenyCheckoutFilePermission(String filepath, String AssignedResource)
        //        {
        //            //Uri tfsUri = new Uri("http://41.41.15.230:171/tfs/Aria");

        //            NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);

        //            TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);

        //            VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;

        //            List<SecurityChange> changes = new List<SecurityChange>();

        //            changes.Add(new PermissionChange(filepath, AssignedResource, null, new string[] { "PendChange" }, null));

        //            SecurityChange[] actualChanges = vcs.SetPermissions(changes.ToArray());

        //        }

        //        [WebMethod]

        //        public void CommentCheckin(String Shelvset, String AssignedResource, String Comment)
        //        {





        //            // Uri tfsUri = new Uri("http://41.41.15.230:171/tfs/DefaultCollection");
        //            Uri tfsUri = new Uri("http://tf_server:8080/tfs/DefaultCollection");
        //            string ProjectPath = "$/New Test/App5";

        //            NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
        //            TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(tfsUri, cre);


        //            VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;


        //            vcs.CreateWorkspace("CheckinWorkSpace", "ProjectAdmin");


        //            var workspace = vcs.GetWorkspace("CheckinWorkSpace", "ProjectAdmin");

        //            workspace.Map(ProjectPath, @"D:\Dev\CheckinWorkSpace");
        //            var LatestRequest = new GetRequest(new ItemSpec(ProjectPath, RecursionType.Full), VersionSpec.Latest);
        //            var results = workspace.Get(LatestRequest, GetOptions.GetAll | GetOptions.Overwrite);

        //            workspace.Unshelve(Shelvset, AssignedResource);
        //            PendingChanges = workspace.GetPendingChanges();


        //            var returnval = workspace.CheckIn(PendingChanges, Comment);

        //            vcs.DeleteWorkspace("CheckinWorkSpace", "ProjectAdmin");



        //        }

        //        [WebMethod]

        //        public void Shelving(String filepath)
        //        {


        //            Uri tfsUri = new Uri("http://tf_server:8080/tfs/DefaultCollection");

        //            NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, TfsDomain);
        //            TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(tfsUri, cre);
        //            sourceControl = (VersionControlServer)tfs.GetService(typeof(VersionControlServer));


        //            Workspace w = sourceControl.GetWorkspace("TR-Khaled", "Khaled Mayhoub");

        //            PendingChange[] changes = w.GetPendingChanges(filepath);//we want to shelve all pending changes in the workspace

        //            if (changes.Length != 0)
        //            {

        //                Shelveset s = new Shelveset(sourceControl, "NewShelv1", TfsUserName);

        //                w.Shelve(s, changes, ShelvingOptions.None);//you can specify to replace existing shelveset, or to remove pending changes from the local workspace with ShelvingOptions

        //            }

        //        }


        //[WebMethod]
        //public void UnShelving(String Shelvset, String AssignedResource)
        //{


        //    Uri tfsUri = new Uri("http://tf_server:8080/tfs/DefaultCollection");

        //    NetworkCredential cre = new NetworkCredential("ProjectAdmin", "aria_123", "TF_Server");
        //    TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(tfsUri, cre);
        //    sourceControl = (VersionControlServer)tfs.GetService(typeof(VersionControlServer));

        //    Workspace w = sourceControl.GetWorkspace("TF_Test", "ProjectAdmin");
        //    w.Unshelve(Shelvset, "Khaled Mayhoub");




        //}


        //}

        # endregion


        # region Azure Mobile Service
      

          [WebMethod]
     
        public bool Notify(String txt, Guid account)
        {

          //  NotificationHubClient hub = NotificationHubClient.CreateClientFromConnectionString("Endpoint=sb://mariamtesthub-ns.servicebus.windows.net/;SharedAccessKeyName=DefaultFullSharedAccessSignature;SharedAccessKey=Z8q6zEf9qJ70eVYLj6LyPjO7UZ0ekDFY790cM+pB9b8=", "mariamtesthub");
           // hub.SendWindowsNativeNotificationAsync(txt, account.ToString());
            NotificationHubClient hub = NotificationHubClient.CreateClientFromConnectionString(WebConfigurationManager.ConnectionStrings["AzureMobileConnectionString"].ConnectionString.ToString(), "mobileserviceresto");
            Notification notification = new WindowsNotification("New Data");
            notification.Headers.Add("X-WNS-Cache-Policy", "cache");
            notification.Headers.Add("X-WNS-Type", "wns/raw");
            notification.ContentType = "application/octet-stream";
            hub.SendNotificationAsync(notification, account.ToString());
              
              return true;
        }
        #endregion

#region TradtionalProductcreatefix
          [WebMethod]
          public bool Createenvironment(string TNUM, string projectName, string ShelveUser, bool CallTFSService, out string exceptionmsg)
          {
              string type = TNUM.Substring(0, 1);
              string number = TNUM.Substring(1);
              TNUM = type + number.Trim().PadLeft(6, '0');

              exceptionmsg = "";
              string sourcepath = "D:\\Tracking";
              string destPath = "D:\\Tracking\\Fixes\\" + TNUM + "";
              try
              {
                  if (Directory.Exists(destPath))
                      Directory.Delete(destPath, true);
              }
              catch
              {

              }
             
              Directory.CreateDirectory(destPath);
              Directory.CreateDirectory(System.IO.Path.Combine(destPath, "Classes"));
              Directory.CreateDirectory(System.IO.Path.Combine(destPath, "Attachments"));
              Directory.CreateDirectory(System.IO.Path.Combine(destPath, "Temp"));
              Directory.CreateDirectory(System.IO.Path.Combine(destPath, "Attachments", "Sysfiles"));

              foreach (string fname in Directory.GetFiles(sourcepath))
              {
                  string pp = Path.GetExtension(fname);
                  if (Path.GetFileNameWithoutExtension(fname) == "genexe" || Path.GetFileNameWithoutExtension(fname) == "geninfo" ||
                      Path.GetFileName(fname) == "config.fpw" || Path.GetFileName(fname).ToUpper() == "ARIA.ICO" || Path.GetFileName(fname) == "tracking.h")
                  {
                      File.Copy(fname, Path.Combine(destPath, Path.GetFileName(fname)));
                  }
                  else if (Path.GetExtension(fname) == ".prg" && Path.GetFileNameWithoutExtension(fname).StartsWith("m", StringComparison.CurrentCultureIgnoreCase))
                  {
                      File.Copy(fname, Path.Combine(destPath, Path.GetFileName(fname)));
                  }
                  else if (Path.GetFileNameWithoutExtension(fname) == "frmgnexe" && Path.GetExtension(fname).StartsWith(".SC", StringComparison.CurrentCultureIgnoreCase))
                  {
                      File.Copy(fname, Path.Combine(destPath, Path.GetFileName(fname)));
                  }
              }
              foreach (string fname in Directory.GetFiles(Path.Combine(sourcepath, "CLASSES")).Where(x => Path.GetFileNameWithoutExtension(x).ToUpper() == "MAIN"))
              {
                  if (Path.GetExtension(fname).StartsWith(".VC", StringComparison.CurrentCultureIgnoreCase))
                  {
                      File.Copy(fname, Path.Combine(destPath, "Classes", Path.GetFileName(fname)));
                  }
              }
              if (CallTFSService)
              {
                  string NewWorkSpace = "";
                  string TfsUserName = "ProjectAdmin";
                  string TfsPassWord = "aria_123";
                  Uri TfsUri = new Uri("http://tf_server:8080/tfs/Aria-Fox");
                  string ERPmap = "$//Aria4XP";
                  string EDImap = "$//EDI";
                  if (projectName.ToUpper() == "ARIA4XP")
                      NewWorkSpace = @"D:\Aria4xp\" + "Aria4xp";
                  else
                      NewWorkSpace = @"D:\EDI";

                  DirectoryInfo parentDirectoryInfo = new DirectoryInfo(NewWorkSpace);
                  ClearReadOnly(parentDirectoryInfo,true);


                  NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, "TF_Server");
                  TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);
                  VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
                  if (vcs.TryGetWorkspace(NewWorkSpace) == null)
                  {
                      //  vcs.DeleteWorkspace("Aria4xp", TfsUserName);
                      if (projectName.ToUpper() == "ARIA4XP")
                          vcs.CreateWorkspace("Aria4xp", TfsUserName);
                      else
                          vcs.CreateWorkspace("EDI", TfsUserName);
                  }
                  var workspace = vcs.GetWorkspace(projectName, TfsUserName);
                  if (projectName.ToUpper() == "ARIA4XP")
                  {
                      workspace.Map(ERPmap, NewWorkSpace);
                      // var LatestRequest = new GetRequest(new ItemSpec(ERPmap, RecursionType.Full), VersionSpec.Latest);
                      // var results = workspace.Get(LatestRequest, GetOptions.GetAll | GetOptions.Overwrite);
                      // var results = workspace.Get(LatestRequest, GetOptions.GetAll);
                  }
                  else
                  {
                      workspace.Map(EDImap, NewWorkSpace);
                       //var LatestRequest = new GetRequest(new ItemSpec(EDImap, RecursionType.Full), VersionSpec.Latest);
                      // var results = workspace.Get(LatestRequest, GetOptions.GetAll | GetOptions.Overwrite);
                       //var results = workspace.Get(LatestRequest, GetOptions.GetAll);
                  }

                  try
                  {
                      workspace.Unshelve(TNUM, ShelveUser);
                  }
                  catch (Exception e)
                  {
                      exceptionmsg = e.Message.ToString();
                      // System.IO.File.WriteAllText("D:\\Testunshelveerror.txt", e.Message + projectName);
                      return false;
                  }
                  if (projectName.ToUpper() == "ARIA4XP")
                  {
                      var LatestRequest = new GetRequest(new ItemSpec(ERPmap, RecursionType.Full), VersionSpec.Latest);
                      var results = workspace.Get(LatestRequest, GetOptions.GetAll);
                  }
                  else {
                      var LatestRequest = new GetRequest(new ItemSpec(EDImap, RecursionType.Full), VersionSpec.Latest);
                      var results = workspace.Get(LatestRequest, GetOptions.GetAll);
                  }
                  ClearReadOnly(parentDirectoryInfo, false);
              }
              
              return true;
          }
          [WebMethod]
          public bool Createsysfiletable(Testarray[] ObjectData, string TNUM, string filename, string tag, string modificationtype,string pname)
            {
                string type = TNUM.Substring(0, 1);
                string number = TNUM.Substring(1);
                TNUM = type + number.Trim().PadLeft(6, '0');

              System.IO.File.WriteAllText("D:\\Testsysfileswithmemofields.txt", "Get this function succefully");
              DataTable sqlqueries = new DataTable();
              OleDbConnection yourConnectionHandler = new OleDbConnection(
                 @"Provider=VFPOLEDB.1;Data Source=D:\\sysfiles\\");
              yourConnectionHandler.Open();
              if (yourConnectionHandler.State == ConnectionState.Open)
              {
                  string mySQL = "select * from " + filename.ToUpper() + " where 1 = 2";  // dbf table name
                  OleDbCommand MyQuery = new OleDbCommand(mySQL, yourConnectionHandler);
                  OleDbDataAdapter DA = new OleDbDataAdapter(MyQuery);
                  DA.Fill(sqlqueries);
                  DataRow row = sqlqueries.NewRow();
                  sqlqueries.Rows.Add(row);
                  for (int i = 0; i < ObjectData.GetLength(0); i++)
                  {
                      if (ObjectData[i] != null && ObjectData[i].columnname != null)
                      {

                          try
                          {
                              ConvertingArraytoMemo arrtomem = new ConvertingArraytoMemo();
                              //ATA start convert from array to memo 
                              if (ObjectData[i].columnname.ToUpper().StartsWith("M"))
                              {
                                  if (ObjectData[i].columnname.ToUpper().Contains("LT"))
                                  {
                                      List<Filter> filters = new List<Filter>();
                                      FilterConverter fltconvertorc = new FilterConverter();
                                      filters = (List<Filter>)fltconvertorc.ConvertFromStorageType(ObjectData[i].valuename.ToString());
                                      if (filters.Count > 0)
                                      {
                                           
                                          if (ObjectData[i].columnname.ToUpper().Contains("FX"))
                                          {
                                              //System.IO.File.WriteAllText("D:\\Temptableformemofields\\" + ObjectData[i].columnname.ToString() + ".txt", ObjectData[i].valuename.ToString());
                                              //System.IO.File.WriteAllText("D:\\Temptableformemofields\\3" + ObjectData[i].columnname.ToString() + ".txt", filters[0].DataType + filters[0].Name + filters[0].Operator + filters[0].ValueType);
                                              row[ObjectData[i].columnname.ToString().Trim()] = Filter.Convertor(filters);// arrtomem.ArrayToMemo(filters.ToArray(), "LAOGFXFLT");
                                              //System.IO.File.WriteAllText("D:\\Temptableformemofields\\2" + ObjectData[i].columnname.ToString() + ".txt", Filter.Convertor(filters));

                                          }
                                          else if (ObjectData[i].columnname.ToUpper().Contains("VR"))
                                          {
                                             // System.IO.File.WriteAllText("D:\\Temptableformemofields\\" + ObjectData[i].columnname.ToString() + ".txt", ObjectData[i].valuename.ToString());
                                             // System.IO.File.WriteAllText("D:\\Temptableformemofields\\3" + ObjectData[i].columnname.ToString() + ".txt", filters[0].DataType + filters[0].Name + filters[0].Operator+filters[0].ValueType);
                                              row[ObjectData[i].columnname.ToString().Trim()] = Filter.Convertor(filters);//arrtomem.ArrayToMemo(filters.ToArray(), "LAOGVRFLT");
                                             // System.IO.File.WriteAllText("D:\\Temptableformemofields\\2" + ObjectData[i].columnname.ToString() + ".txt", Filter.Convertor(filters));

                                          }
                                          else
                                          {
                                             // System.IO.File.WriteAllText("D:\\Temptableformemofields\\" + ObjectData[i].columnname.ToString() + ".txt", ObjectData[i].valuename.ToString());
                                             // System.IO.File.WriteAllText("D:\\Temptableformemofields\\3" + ObjectData[i].columnname.ToString() + ".txt", filters.Count.ToString());
                                              row[ObjectData[i].columnname.ToString().Trim()] = Filter.Convertor(filters);//arrtomem.ArrayToMemo(filters.ToArray(), "LAOGHDFLT");
                                              //System.IO.File.WriteAllText("D:\\Temptableformemofields\\2" + ObjectData[i].columnname.ToString() + ".txt", Filter.Convertor(filters));

                                          }
                                      }

                                  }
                                  else if (ObjectData[i].columnname.ToUpper().Contains("LD"))
                                  {
                                      List<Fields> Fields = new List<Fields>();
                                      FieldsConverter fldconvertor = new FieldsConverter();
                                      Fields = (List<Fields>)fldconvertor.ConvertFromStorageType(ObjectData[i].valuename.ToString());


                                      if (Fields.Count > 0)
                                      {
                                          if (ObjectData[i].columnname.ToUpper().Contains("AV"))
                                          {
                                              //ATA will be handled as tow arrays not just one array 
                                              row[ObjectData[i].columnname.ToString().Trim()] = arrtomem.ArrayToMemo(Fields.ToArray(), "LAOGFIELDH");
                                          }
                                          else
                                          {
                                              row[ObjectData[i].columnname.ToString().Trim()] = arrtomem.ArrayToMemo(Fields.ToArray(), "LASELFIELD");
                                          }

                                      }

                                  }
                                  else if (ObjectData[i].columnname.ToUpper().Contains("IL"))
                                  {
                                      List<Files> Files = new List<Files>();
                                      FilesConverter filconvertor = new FilesConverter();
                                      Files = (List<Files>)filconvertor.ConvertFromStorageType(ObjectData[i].valuename.ToString());
                                      if (Files.Count > 0)
                                      {
                                          row[ObjectData[i].columnname.ToString().Trim()] = arrtomem.ArrayToMemo(Files.ToArray(), "LASELFILE");
                                      }
                                  }
                                  else
                                  {
                                      row[ObjectData[i].columnname.ToString().Trim()] = ObjectData[i].valuename.ToString();
                                  }
                              }
                              else
                              {
                                  if (ObjectData[i].columnname.ToString().ToUpper().StartsWith("L"))
                                  {
                                      if (!string.IsNullOrEmpty(ObjectData[i].valuename.ToString()))
                                      row[ObjectData[i].columnname.ToString().Trim()] = Boolean.Parse(ObjectData[i].valuename.ToString());
                                  }
                                  else
                                  {
                                      try
                                      {
                                          if (string.IsNullOrEmpty(row[ObjectData[i].columnname.ToString().Trim()].ToString ()) && !string.IsNullOrEmpty(ObjectData[i].valuename.ToString()))
                                          row[ObjectData[i].columnname.ToString().Trim()] = ObjectData[i].valuename.ToString();
                                      }
                                      catch { }
                                  
                                  }
                              }
                             
                          }
                          catch (Exception e)
                          {
                              try
                              {
                                  if (ObjectData[i].columnname.ToString().ToUpper().StartsWith("L"))
                                  {
                                      if (!string.IsNullOrEmpty(ObjectData[i].valuename.ToString()))
                                          row[ObjectData[i].columnname.ToString().Trim()] = Boolean.Parse(ObjectData[i].valuename.ToString());
                                  }
                                  else
                                  {
                                      row[ObjectData[i].columnname.ToString().Trim()] = ObjectData[i].valuename.ToString();
                                  }
                              }
                              catch
                              { }

                          }

                      }
                  }
                  yourConnectionHandler.Close();
              }
              createfixproject.createnewfix newinst = new createfixproject.createnewfix();
              string destpath = newinst.createsystable(filename, "D:\\sysfiles", "D:\\Tracking\\Fixes\\" + TNUM + "\\Attachments\\Sysfiles",false);
              string destpathTmp = newinst.createsystable(filename, "D:\\sysfiles", "D:\\Tracking\\Fixes\\" + TNUM + "\\Temp\\",true);
              //ATA get the table all fields from table as a rray 
              object[,] array = newinst.getthefields(filename, "D:\\sysfiles");
              //MMT
              //System.Data.OleDb.OleDbConnection OleConn = new System.Data.OleDb.OleDbConnection(@"Provider=Microsoft.Jet.OLEDB.4.0;Data Source=D:\\;Extended Properties=DBASE IV;");
              //OleConn.Open();
              //MMT
              //ATA create this array to contain only the name of fields that it's data type is memo 
              string[] memofields = new string[20];
              string[] noncharfields = new string[20];
              string[] datetimevalues = new string[10];
              int y = 0;
              int c = 0;
              int d = 0;
              StringBuilder commandcolumns = new StringBuilder();
              StringBuilder commandvalues = new StringBuilder();

              StringBuilder commandcolumnsTmp = new StringBuilder();
              StringBuilder commandvaluesTmp = new StringBuilder();
              //ATA start generate script for the sysfile 
              commandcolumns.Append("insert into  " + destpath + "(");
              commandcolumnsTmp.Append("insert into  " + destpathTmp + "(");
              
              commandvalues.Append(")Values(");
              commandvaluesTmp.Append(")Values(");
              //ATA fill the array of memofields and create first part of query 
              for (int i = 1; i <= array.GetLength(0); i++)
              {


                  if (array[i, 2].ToString().ToUpper() == "M")
                  {
                      memofields[y] = array[i, 1].ToString();
                      y++;
                  }
                  else if (array[i, 2].ToString().ToUpper() == "D")
                  {
                      datetimevalues[d] = array[i, 1].ToString();
                      d++;

                  }
                  else if (array[i, 2].ToString().ToUpper() != "C" && array[i, 2].ToString().ToUpper() != "M" && array[i, 2].ToString().ToUpper() != "D")
                  {
                      noncharfields[c] = array[i, 1].ToString();
                      c++;
                  }

              }
            
              //ATA loop over the datatable to get the memo fields from it  and create the insert query 

              for (int row = 0; row < sqlqueries.Rows.Count; row++)
              {
                 
                  for (int column = 0; column < sqlqueries.Columns.Count; column++)
                  {
                      if (!string.IsNullOrEmpty(sqlqueries.Rows[row][column].ToString()))
                      {
                          commandcolumns.Append("[" + sqlqueries.Columns[column].ColumnName.ToString() + "],");
                          commandcolumnsTmp.Append("[" + sqlqueries.Columns[column].ColumnName.ToString() + "],");
                          if (memofields.Contains(sqlqueries.Columns[column].ColumnName.ToUpper()))
                          {
                              System.IO.File.WriteAllText("D:\\tablememofields\\" + sqlqueries.Columns[column].ColumnName.ToString() + ".txt", sqlqueries.Rows[row][column].ToString());
                              commandvalues.Append("FILETOSTR('D:\\tablememofields\\" + sqlqueries.Columns[column].ColumnName.ToString() + ".txt'),");
                              commandvaluesTmp.Append("FILETOSTR('D:\\tablememofields\\" + sqlqueries.Columns[column].ColumnName.ToString() + ".txt'),");
                          }
                          else if (noncharfields.Contains(sqlqueries.Columns[column].ColumnName.ToUpper()))
                          {
                              commandvalues.Append(sqlqueries.Rows[row][column].ToString() + ",");
                              commandvaluesTmp.Append(sqlqueries.Rows[row][column].ToString() + ",");

                          }
                          else if (datetimevalues.Contains(sqlqueries.Columns[column].ColumnName.ToUpper()))
                          {
                              string te = sqlqueries.Rows[row][column].ToString();
                              if (!string.IsNullOrEmpty(sqlqueries.Rows[row][column].ToString()))
                              {
                                  string testdate = DateTime.Parse(sqlqueries.Rows[row][column].ToString()).Date.ToString("d");
                                  commandvalues.Append("CTOD(\"" + DateTime.Parse(sqlqueries.Rows[row][column].ToString()).Date.ToString("d") + "\"),");
                                  commandvaluesTmp.Append("CTOD(\"" + DateTime.Parse(sqlqueries.Rows[row][column].ToString()).Date.ToString("d") + "\"),");
                              }
                              else
                              {
                                  commandvalues.Append("CTOD(''),");
                                  commandvaluesTmp.Append("CTOD(''),");
                              }
                          }
                          else
                          {
                              commandvalues.Append("'" + sqlqueries.Rows[row][column].ToString() + "',");
                              commandvaluesTmp.Append("'" + sqlqueries.Rows[row][column].ToString() + "',");

                          }
                      }
                  }
              
                  StringBuilder test = new StringBuilder();
                  StringBuilder testTmp = new StringBuilder();
                  test.Append(commandcolumns.ToString(0, commandcolumns.Length - 1) + commandvalues.ToString(0, commandvalues.Length - 1).Replace("False", ".F.").Replace("True", ".T.") + ")");
                  testTmp.Append(commandcolumnsTmp.ToString(0, commandcolumnsTmp.Length - 1) + commandvaluesTmp.ToString(0, commandvaluesTmp.Length - 1).Replace("False", ".F.").Replace("True", ".T.") + ")");
                  //ATA record into temp table that created under the fix folder attachement 
                 
                      var parse = new VisualFoxpro.FoxApplication();
                      string foxCommand = "use 'D:\\Tracking\\Fixes\\" + TNUM + "\\Attachments\\Sysfiles\\" + filename + "' Shared";
                    //  System.IO.File.WriteAllText("D:\\ErrorHappenedHere.txt", test.ToString());
                      
                      parse.DoCmd(foxCommand);
                      parse.DoCmd(test.ToString());
                      foxCommand = "Close All";

                      foxCommand = "use 'D:\\Tracking\\Fixes\\" + TNUM + "\\Temp\\" + filename + "' Shared";
                      //  System.IO.File.WriteAllText("D:\\ErrorHappenedHere.txt", test.ToString());

                      parse.DoCmd(foxCommand);
                      parse.DoCmd(testTmp.ToString());
                      foxCommand = "Close All";

                      parse.DoCmd(foxCommand);
                      var task = System.Diagnostics.Process.GetProcessById(parse.ProcessId);
                  //
                    //  parse.Quit();
                      task.Kill();
                  //
                      task.Dispose();
                      
                  
                  foreach (string fname in Directory.GetFiles("D:\\tablememofields"))
                  {
                      File.Delete(fname);
                  }
                  //newinst.updatesystemfile("D:\\Sysfiles\\" + filename, destpath, modificationtype, tag);
                  newinst.updatesystemfile("D:\\Sysfiles\\" + filename, destpathTmp, modificationtype, tag);
                  //MMT
                  newinst = null;
                  //MMT
              }
              return true;
          }
          [WebMethod]
          public bool Createattachmenttable(Attachments[] Attachments, string TNUM)
          {

              createfixproject.createnewfix newinst = new createfixproject.createnewfix();
              //ATA create attachment table using fox code
              newinst.createattachmenttable("D:\\Tracking\\Fixes\\" + TNUM + "\\Temp\\Attachment.dbf", "Crep_id");

              System.Data.OleDb.OleDbConnection OleConn = new System.Data.OleDb.OleDbConnection();
              OleConn.ConnectionString = @"Provider=VFPOLEDB.1;Data Source=D:\";
              OleConn.Open();


              for (int i = 0; i < Attachments.GetLength(0); i++)
              {
                  if (Attachments[i] != null )//&& Attachments[i].Type.ToString() == "System")
                  {
                      if (Attachments[i].Type.ToString() == "System")
                      {
                          //Attachments[i].Key = newinst.GetKey("D:\\Tracking\\Fixes\\" + TNUM + "\\attachments\\sysfiles\\" + Attachments[i].Name + "", Attachments[i].cKeyExpression.ToString());
                          Attachments[i].Key = newinst.GetKey("D:\\Tracking\\Fixes\\" + TNUM + "\\Temp\\" + Attachments[i].Name + "", Attachments[i].cKeyExpression.ToString());
                      }
                      else
                      {
                          var di = new DirectoryInfo(Attachments[i].Source);
                          di.Attributes &= ~FileAttributes.ReadOnly;
                      }
                      string insertquery = "Insert into D:\\Tracking\\Fixes\\" + TNUM + "\\Temp\\Attachment.dbf (Name,Source,Dest,Tag,Key,Type,Transfer,Verify,ldontcomp,lSysData,cObjType) Values('" + Attachments[i].Name + "','" + Attachments[i].Source + "','" + Attachments[i].Dest + "','" + Attachments[i].Tag + "','" + Attachments[i].Key + "','" + Attachments[i].Type + "',.F.,.T.,.F.,.T.,'s')";
                      
                    //  System.IO.File.WriteAllText("D:\\Tracking\\Fixes\\" + TNUM + "\\Checkfixgenerations2.TXT", insertquery);
                      System.Data.OleDb.OleDbCommand OleComm1 = new System.Data.OleDb.OleDbCommand(insertquery, OleConn);
                      OleComm1.ExecuteNonQuery();
                      string entrytype = TNUM.Substring(0,1);
                      string entryid = TNUM.Substring(1,TNUM.Length-1);
                      var result = newinst.checktheattachment(entrytype,entryid,Attachments[i].cObjType,Attachments[i].Name,"",Attachments[i].Tag,Attachments[i].Key);
                      if (!result)
                      {
                          newinst.addrosuautoprc(entryid , entrytype, Attachments[i].cObjType, Attachments[i].Source, Attachments[i].Dest, Attachments[i].Name , Attachments[i].cObjType, Attachments[i].Tag , Attachments[i].Key,false,false,"U");
                          //string insertinstuatuoprc = "insert into 'D:\\Tracking\\SUAUTPRC.DBF'(centryid,centrytype,cobjecttyp,csource,cdisten,cobjname,cobjtype,ctag,ckey,ldontcomp,lsys_data,CPROSSTYPE,MCONTENT,MUPDMODULS,CADD_USER) Values ('" + entryid + "','" + entrytype + "','" + Attachments[i].cObjType + "','" + Attachments[i].Source + "','" + Attachments[i].Dest + "','" + Attachments[i].Name + "','" + Attachments[i].cObjType + "','" + Attachments[i].Tag + "','" + Attachments[i].Key + "',.F.,.F.,'U','','','')";
                        //  System.Data.OleDb.OleDbCommand OleComm2 = new System.Data.OleDb.OleDbCommand(insertinstuatuoprc, OleConn);
                         // OleComm2.ExecuteNonQuery();
                      }
                     
                  }

              }
              OleConn.Close();



              return true;
          }
          private void ClearReadOnly(DirectoryInfo parentDirectory,bool deletefile)
          {
              if (parentDirectory != null)
              {
                  parentDirectory.Attributes = FileAttributes.Normal;
                  foreach (FileInfo fi in parentDirectory.GetFiles())
                  {
                      fi.Attributes = FileAttributes.Normal;
                      if (deletefile)
                          fi.Delete();

                  }
                  foreach (DirectoryInfo di in parentDirectory.GetDirectories())
                  {
                      ClearReadOnly(di,deletefile);
                  }
              }
          }
          [WebMethod]
          public bool createfix(string TNUM,string ProjectName)
          {
              string type = TNUM.Substring(0, 1);
              string number = TNUM.Substring(1);
              TNUM = type + number.Trim().PadLeft(6, '0');
              createfixproject.createnewfix newinst = new createfixproject.createnewfix();
              string product = "";
              string defaultfolder = "";
              // throw new Exception("there is error");
              if (ProjectName.Contains("4")){
                  product = "A40";
                  defaultfolder = "D:\\Aria4xp\\ARIA4XP\\Aria4xp (R13)\\";
              }
              else{
                  product = "A30";
                  defaultfolder =@"D:\EDI\EDI3.0\";

              }
              newinst.createthefix(TNUM, "D:\\Tracking\\Fixes\\" + TNUM + "\\", "B", "D:\\Tracking\\Fixes\\" + TNUM + "\\Temp\\Attachment.dbf", defaultfolder, "", "13", product, true, "D:\\Aria4xp\\ARIA4XP\\Aria4xp (R13)\\");
              
              string tracking = TNUM;
              var parse = new VisualFoxpro.FoxApplication();
              string foxCommand = "DO D:\\FoxProProject\\ss.prg with '" + tracking + "','D:\\Tracking\\Fixes\\" + tracking + "\\'";
              parse.DefaultFilePath = @"D:\\FoxProProject\\";
             
              try
              {
                 
                  parse.DoCmd(foxCommand);
                  parse.Quit();
              }
              catch
              {
                  //parse.Quit();
                  var test1 = System.Diagnostics.Process.GetProcessById(parse.ProcessId);
                  test1.Close();
                //  if (test1.HasExited)
                //  {
                      string tracking1 = TNUM;
                      var parse1 = new VisualFoxpro.FoxApplication();
                      string foxCommand1 = "DO D:\\FoxProProject\\ss.prg with '" + tracking1 + "','D:\\Tracking\\Fixes\\" + tracking1 + "\\'";
                      parse1.DefaultFilePath = @"D:\\FoxProProject\\";
                      parse1.DoCmd(foxCommand1);
                      var test = System.Diagnostics.Process.GetProcessById(parse1.ProcessId);
                     // test.Close();
                      //parse1.Quit();
                      test.Kill();
                     // System.IO.File.WriteAllText("D:\\Tracking\\Fixes\\" + TNUM + "\\Checkfixgenerations3.TXT", "Catch completed");
                 // }


              }
              finally
              {

              }
              //ATA add this code to handle undo changes for the used files 
              string NewWorkSpace = "";
              string TfsUserName = "ProjectAdmin";
              string TfsPassWord = "aria_123";
              Uri TfsUri = new Uri("http://tf_server:8080/tfs/Aria-Fox");
              if (ProjectName.ToUpper() == "ARIA4XP")
                  NewWorkSpace = @"D:\Aria4xp\" + "Aria4xp";
              else
                  NewWorkSpace = @"D:\EDI";
             
              NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, "TF_Server");
              TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);
              VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
              if (vcs.TryGetWorkspace(NewWorkSpace) == null)
              {
                     // vcs.DeleteWorkspace(ProjectName, TfsUserName);
                      vcs.CreateWorkspace(ProjectName, TfsUserName);
              }
              var workspace = vcs.GetWorkspace(ProjectName, TfsUserName);
              if (ProjectName.ToUpper() == "ARIA4XP")
              {
                  workspace.Map("$//Aria4XP", NewWorkSpace);
                  var LatestRequest = new GetRequest(new ItemSpec("$//Aria4xp", RecursionType.Full), VersionSpec.Latest);
                  // var results = workspace.Get(LatestRequest, GetOptions.GetAll | GetOptions.Overwrite);
                  PendingChanges = workspace.GetPendingChanges();
              }
              else
              {
                  workspace.Map("$//EDI", NewWorkSpace);
                  var LatestRequest = new GetRequest(new ItemSpec("$//EDI", RecursionType.Full), VersionSpec.Latest);
                  // var results = workspace.Get(LatestRequest, GetOptions.GetAll | GetOptions.Overwrite);
                  PendingChanges = workspace.GetPendingChanges();
              }
             
              if(PendingChanges.Count() > 0)
              workspace.Undo(PendingChanges);

              if (ProjectName.ToUpper() == "ARIA4XP")
                  vcs.DeleteWorkspace("Aria4xp", TfsUserName);
              else
                  vcs.DeleteWorkspace("EDI", TfsUserName); 
              //  parse.ProcessId
             newinst.clearentryfolder("D:\\Tracking\\Fixes\\" + tracking + "\\");

           //  newinst.clearentryfolder("D:\\Tracking\\Fixes\\" + tracking + "\\Classes");
            // newinst.clearentryfolder("D:\\Tracking\\Fixes\\" + tracking + "\\Temp");

              return true;
          }


        [WebMethod]
          public void addnewfile(string file,string ProjectName, string ResourceName)
          {
              //string file = path + filename;
              File.Create(file).Close();

              string NewWorkSpace = "";
              string TfsUserName = "ProjectAdmin";
              string TfsPassWord = "aria_123";
              Uri TfsUri = new Uri("http://tf_server:8080/tfs/Aria-Fox");
              if (ProjectName.ToUpper() == "ARIA4XP")
                  NewWorkSpace = @"D:\Aria4xp\" + "Aria4xp";
              else
                  NewWorkSpace = @"D:\EDI";
              NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, "TF_Server");
              TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);
              VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
              if (vcs.TryGetWorkspace(NewWorkSpace) == null)
              {
                  // vcs.DeleteWorkspace(ProjectName, TfsUserName);
                   vcs.CreateWorkspace(ProjectName, TfsUserName);

                  //if (projectName.ToUpper() == "ARIA4XP")
                  //    vcs.CreateWorkspace("Aria4xp", TfsUserName);
                  //else
                  //    vcs.CreateWorkspace("EDI", TfsUserName);
              }
              var workspace = vcs.GetWorkspace(ProjectName, TfsUserName);
              if (ProjectName.ToUpper() == "ARIA4XP")
              {
                  workspace.Map("$//Aria4XP", NewWorkSpace);
              }
              else
              {
                  workspace.Map("$//EDI", NewWorkSpace);
              }
              workspace.PendAdd(file);
              PendingChanges = workspace.GetPendingChanges();
              if (PendingChanges.Count() > 0)
              {
                  workspace.CheckIn(PendingChanges, "New file Added for the resource'" + ResourceName + "'");
                 // if (projectName.ToUpper() == "ARIA4XP")
                    //  vcs.DeleteWorkspace(NewWorkSpace, TfsUserName);
                 // else
                  vcs.DeleteWorkspace(ProjectName, TfsUserName);
              }

          }
          [WebMethod]
          public void ClearEntryfolder(string path)
          {
              createfixproject.createnewfix newinst = new createfixproject.createnewfix();
              newinst.clearentryfolder(path);
          }

          [WebMethod]
          public void CreateBuild(string[] TrackingEntryNumbers,string BuildType, string product,string Release,string servc_pack,int build)
          {
              System.Data.OleDb.OleDbConnection OleConn = new System.Data.OleDb.OleDbConnection();
              OleConn.ConnectionString = @"Provider=VFPOLEDB.1;Data Source=D:\";
              OleConn.Open();
              string DeleteoldEntries = "Delete From  'D:\\Tracking\\Builds\\" + product + "\\" + "Entries\\Entries.dbf'";
              System.Data.OleDb.OleDbCommand OleComm1 = new System.Data.OleDb.OleDbCommand(DeleteoldEntries, OleConn);
              OleComm1.ExecuteNonQuery();
              foreach (string item in TrackingEntryNumbers)
              {
                  string InsertEntries = "insert into 'D:\\Tracking\\Builds\\"+product+"\\"+"Entries\\Entries.dbf'(entryid)Values ('" + item + "')";
                  System.Data.OleDb.OleDbCommand OleComm2 = new System.Data.OleDb.OleDbCommand(InsertEntries, OleConn);
                  OleComm2.ExecuteNonQuery();
              }
              createfixproject.createnewfix newins = new createfixproject.createnewfix();
            var Buildnumber =  newins.createthebuild(BuildType, product, Release, true, servc_pack, build.ToString(), "D:\\Tracking\\Fixes\\", "D:\\Tracking\\Fixes\\Entries\\Entries.dbf");


            string tracking =product+Buildnumber;
              var parse = new VisualFoxpro.FoxApplication();
              string foxCommand = "DO D:\\FoxProProject\\ss.prg with '" + Buildnumber + "','D:\\Tracking\\Builds\\" + product + "\\" + tracking + "\\'";
              parse.DefaultFilePath = @"D:\\FoxProProject\\";

              try
              {

                  parse.DoCmd(foxCommand);
                  parse.Quit();
              }
              catch
              {
                  parse.Quit();
                  var test1 = System.Diagnostics.Process.GetProcessById(parse.ProcessId);
                  test1.Close();
                  //  if (test1.HasExited)
                  //  {
                  string tracking1 =product+Buildnumber;
                  var parse1 = new VisualFoxpro.FoxApplication();
                  string foxCommand1 = "DO D:\\FoxProProject\\ss.prg with '" + Buildnumber + "','D:\\Tracking\\Builds\\" + product +"\\"+ tracking1 + "\\'";
                  parse1.DefaultFilePath = @"D:\\FoxProProject\\";
                  parse1.DoCmd(foxCommand1);
                  var test = System.Diagnostics.Process.GetProcessById(parse1.ProcessId);
                  test.Close();
                  //
                  //parse1.Quit();
                  test.Kill();
                  //
                  // System.IO.File.WriteAllText("D:\\Tracking\\Fixes\\" + TNUM + "\\Checkfixgenerations3.TXT", "Catch completed");
                  // }
              }
            string buildfolderpath = product+Buildnumber;
            
            this.ClearEntryfolder("D:\\Tracking\\Fixes\\" + buildfolderpath + "\\");
          }
#endregion
        //Sara.n Handle FOX BUILD 11/06/2018 [Start]
        [WebMethod]
          public bool CreateBuildEnvironment(string BNUM, List <string> TNUMBERS, string ProjectName)
            {
                string sourcepath = "D:\\Tracking";
                if (ProjectName.Contains("4"))
                  ProjectName = "A40";
                else
                  ProjectName = "EDI";

                string destPath = "D:\\Tracking\\BUILDS\\" + ProjectName + "\\BD" + BNUM + ProjectName + " ";
                //MMT
                try
                {
                    var dir = new DirectoryInfo(destPath);
                    foreach (var file in dir.GetFiles("*", SearchOption.AllDirectories))
                        file.Attributes &= ~FileAttributes.ReadOnly;
                    System.IO.File.WriteAllText("D:\\Tracking\\" + "buildbeforeDel.TXT", "before");
                    //dir.Delete(true);
                    EmptyFolder(new DirectoryInfo(destPath));
                    System.IO.File.WriteAllText("D:\\Tracking\\" + "buildAfterDel.TXT", "After");
                }
                catch(Exception ex)
                { System.IO.File.WriteAllText("D:\\Tracking\\" + "buildclrexcp.TXT", ex.Message ); }
                //MMT
                //try
                //{
                //    if (Directory.Exists(destPath))
                //        Directory.Delete(destPath, true);
                //}
                //catch
                //{

                //}
                Directory.CreateDirectory(destPath);
                Directory.CreateDirectory(System.IO.Path.Combine(destPath, "Classes"));
                Directory.CreateDirectory(System.IO.Path.Combine(destPath, "Attachments"));
                Directory.CreateDirectory(System.IO.Path.Combine(destPath, "Temp"));
                Directory.CreateDirectory(System.IO.Path.Combine(destPath, "Attachments", "Sysfiles"));
                foreach (string fname in Directory.GetFiles(sourcepath))
                {
                    string pp = Path.GetExtension(fname);
                    if (Path.GetFileNameWithoutExtension(fname) == "genexe" || Path.GetFileNameWithoutExtension(fname) == "geninfo" ||
                        Path.GetFileName(fname) == "config.fpw" || Path.GetFileName(fname).ToUpper() == "ARIA.ICO" || Path.GetFileName(fname) == "tracking.h")
                    {
                        File.Copy(fname, Path.Combine(destPath, Path.GetFileName(fname)));
                    }
                    else if (Path.GetExtension(fname) == ".prg" && Path.GetFileNameWithoutExtension(fname).StartsWith("m", StringComparison.CurrentCultureIgnoreCase))
                    {
                        File.Copy(fname, Path.Combine(destPath, Path.GetFileName(fname)));
                    }
                    else if (Path.GetFileNameWithoutExtension(fname) == "frmgnexe" && Path.GetExtension(fname).StartsWith(".SC", StringComparison.CurrentCultureIgnoreCase))
                    {
                        File.Copy(fname, Path.Combine(destPath, Path.GetFileName(fname)));
                    }
                }
                foreach (string fname in Directory.GetFiles(Path.Combine(sourcepath, "CLASSES")).Where(x => Path.GetFileNameWithoutExtension(x).ToUpper() == "MAIN"))
                {
                    if (Path.GetExtension(fname).StartsWith(".VC", StringComparison.CurrentCultureIgnoreCase))
                    {
                        File.Copy(fname, Path.Combine(destPath, "Classes", Path.GetFileName(fname)));
                    }
                }
            //MMT
                foreach (string fname in Directory.GetFiles(Path.Combine(sourcepath, "CLASSES")).Where(x => Path.GetFileNameWithoutExtension(x).ToUpper() == "ARIAAPPLICATION"))
                {
                    if (Path.GetExtension(fname).StartsWith(".H", StringComparison.CurrentCultureIgnoreCase))
                    {
                        File.Copy(fname, Path.Combine(destPath, "Classes", Path.GetFileName(fname)));
                    }
                }
            //MMT
                //string cSourceCodePath ="";
                //if (ProjectName == "A40")
                //    cSourceCodePath = @"D:\Aria4xp\" + "Aria4xp";
                //else
                //    cSourceCodePath = @"D:\EDI";
                string defaultfolder = "";
                if (ProjectName.Contains("4"))
                {
                  defaultfolder = "D:\\Aria4xp\\ARIA4XP\\Aria4xp (R13)\\";
                }
                else
                {
                    defaultfolder = @"D:\EDI\EDI3.0\";
                }
                string NewWorkSpace = "";
                string TfsUserName = "ProjectAdmin";
                string TfsPassWord = "aria_123";
                Uri TfsUri = new Uri("http://tf_server:8080/tfs/Aria-Fox");
                string ERPmap = "$//Aria4XP";
                string EDImap = "$//EDI";
                if (ProjectName.Contains("4"))
                    NewWorkSpace = @"D:\Aria4xp\" + "Aria4xp";
                else
                    NewWorkSpace = @"D:\EDI";

                DirectoryInfo parentDirectoryInfo = new DirectoryInfo(defaultfolder);
                ClearReadOnly(parentDirectoryInfo, true);

                NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, "TF_Server");
                TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);
                VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
                if (vcs.TryGetWorkspace(NewWorkSpace) == null)
                {
                    //  vcs.DeleteWorkspace("Aria4xp", TfsUserName);
                    if (ProjectName.Contains("4"))
                        vcs.CreateWorkspace("Aria4xp", TfsUserName);
                    else
                        vcs.CreateWorkspace("EDI", TfsUserName);
                }
                Workspace  workspace ;
                
                if (ProjectName.Contains("4"))
                {
                    workspace = vcs.GetWorkspace("Aria4xp", TfsUserName);
                    workspace.Map(ERPmap, NewWorkSpace);
                    // var LatestRequest = new GetRequest(new ItemSpec(ERPmap, RecursionType.Full), VersionSpec.Latest);
                    // var results = workspace.Get(LatestRequest, GetOptions.GetAll | GetOptions.Overwrite);
                    // var results = workspace.Get(LatestRequest, GetOptions.GetAll);
                }
                else
                {
                    workspace = vcs.GetWorkspace("EDI", TfsUserName);
                    workspace.Map(EDImap, NewWorkSpace);
                    //var LatestRequest = new GetRequest(new ItemSpec(EDImap, RecursionType.Full), VersionSpec.Latest);
                    // var results = workspace.Get(LatestRequest, GetOptions.GetAll | GetOptions.Overwrite);
                    //var results = workspace.Get(LatestRequest, GetOptions.GetAll);
                }

               
               

                foreach (var TNUM in TNUMBERS)
                {
                    try
                    {
                        workspace.Unshelve(TNUM.Substring(0, 7), TNUM.Substring(8));
                    }
                    catch (Exception e)
                    {
                        // exceptionmsg = e.Message.ToString();
                        // System.IO.File.WriteAllText("D:\\Testunshelveerror.txt", e.Message + projectName);
                       // return false;
                    }

                    //foreach (string dirPath in Directory.GetDirectories(sourcepath + "\\Fixes\\" + TNUM + "\\" + "Attachments\\", "*",
                    //   SearchOption.AllDirectories))
                    //    Directory.CreateDirectory(dirPath.Replace(sourcepath + "\\Fixes\\" + TNUM + "\\" + "Attachments\\", destPath + "\\" + "Attachments\\"));
                    //Copy all the files & Replaces any files with the same name
                    ////foreach (string newPath in Directory.GetFiles(sourcepath + "\\Fixes\\" + TNUM + "\\" + "Attachments\\", "*.*",
                    ////    SearchOption.AllDirectories))
                    ////{
                    ////    FileInfo xx = new FileInfo (newPath);
                    ////    if (xx.Extension.ToUpper().Trim()   == "FRX" || xx.Extension.ToUpper().Trim()   == "FRT") 
                    ////    //File.Copy(newPath, newPath.Replace(sourcepath + "\\Fixes\\" + TNUM + "\\" + "Attachments\\", destPath + "\\" + "Attachments\\"), true);
                    ////      File.Copy(newPath, newPath.Replace(sourcepath + "\\Fixes\\" + TNUM + "\\" + "Attachments\\", defaultfolder + "\\"), true);
                    ////}
                    //////Mariam
                    //////foreach (string dirPath in Directory.GetDirectories(sourcepath + "\\Fixes\\" + TNUM + "\\" + "brands\\", "*",
                    //////   SearchOption.AllDirectories))
                    //////    Directory.CreateDirectory(dirPath.Replace(sourcepath + "\\Fixes\\" + TNUM + "\\" + "brands\\", destPath + "\\" + "brands\\"));
                    //////Copy all the files & Replaces any files with the same name
                    ////foreach (string newPath in Directory.GetFiles(sourcepath + "\\Fixes\\" + TNUM + "\\" + "brands\\", "*.*",
                    ////    SearchOption.AllDirectories))
                    ////{
                    ////    FileInfo yy = new FileInfo(newPath);
                    ////    if (yy.Extension.ToUpper().Trim() != "FRX" && yy.Extension.ToUpper().Trim() != "FRT")
                    ////    {
                    ////        //File.Copy(newPath, newPath.Replace(sourcepath + "\\Fixes\\" + TNUM + "\\" + "brands\\", destPath + "\\" + "brands\\"), true);
                    ////        File.Copy(newPath, newPath.Replace(sourcepath + "\\Fixes\\" + TNUM + "\\" + "brands\\", defaultfolder + "\\"), true);
                    ////    }
                    ////}
                    
                    //Mariam
                }
              //MMT
                if (ProjectName.Contains("4"))
                {
                    var LatestRequest = new GetRequest(new ItemSpec(ERPmap, RecursionType.Full), VersionSpec.Latest);
                    var results = workspace.Get(LatestRequest, GetOptions.GetAll);
                }
                else
                {
                    var LatestRequest = new GetRequest(new ItemSpec(EDImap, RecursionType.Full), VersionSpec.Latest);
                    var results = workspace.Get(LatestRequest, GetOptions.GetAll);
                }
                ClearReadOnly(parentDirectoryInfo, false);

                var directroy = new DirectoryInfo(destPath);
                //foreach (var file in directroy.GetFiles("*", SearchOption.AllDirectories))
                //    file.Attributes &= ~FileAttributes.ReadOnly;
                ClearReadOnly(directroy, false);
            //MMT

                createfixproject.createnewfix newinst = new createfixproject.createnewfix();
                //ATA create attachment table using fox code
                newinst.createattachmenttable("D:\\Tracking\\Builds\\"+ProjectName+"\\BD" + BNUM + ProjectName + "\\Temp\\Attachment.dbf", "Crep_id");
            //MMT
                newinst = null;
            //MMT
                return true;
            }  
        [WebMethod]
          public bool createFOXBuild(string BUILD, string ProjectName)
              {
                  string product = "";
                  string defaultfolder = "";
                  string tracking = BUILD;
                  string foxCommand = "";
                  if (ProjectName.Contains("4"))
                  {
                      product = "A40";
                      defaultfolder = "D:\\Aria4xp\\ARIA4XP\\Aria4xp (R13)\\";
                  }
                  else
                  {
                      product = "EDI";
                      defaultfolder = @"D:\EDI\EDI3.0\";
                  }
                  //MMT
                  var directroy = new DirectoryInfo("D:\\Tracking\\BUILDS\\" + product + "\\BD" + BUILD.ToString() + product + "\\");
                  foreach (var file in directroy.GetFiles("*", SearchOption.AllDirectories))
                  {
                      file.Attributes &= ~FileAttributes.ReadOnly;
                  }
                  //MMT
        
                  createfixproject.createnewfix newinst = new createfixproject.createnewfix();
                 
                  var parse = new VisualFoxpro.FoxApplication();


                  newinst.createthefix(BUILD, "D:\\Tracking\\BUILDS\\" + product + "\\BD" + BUILD.ToString() + product + "\\", "D", "D:\\Tracking\\BUILDS\\" + product + "\\BD" + BUILD + product + "\\" + "Temp\\Attachment.dbf", defaultfolder, "000", "13", product, true, "D:\\Aria4xp\\ARIA4XP\\Aria4xp (R13)\\");
                  //BD141R13
                  try
                  {
                     // parse.DoCmd("Close All");
                     
                     // System.IO.File.WriteAllText("D:\\Tracking\\" + "CheckBuildTrygenerations.TXT", "In Try");
                      //MMT
                      //buildexeClass f = new buildexeClass();
                      //f.CreateExe("BD" + BUILD.ToString() + "EDI","D:\\Tracking\\BUILDS\\" + product + "\\BD" + BUILD.ToString() + "EDI" + "\\");
                      parse.DoCmd("USE '" + "D:\\Tracking\\BUILDS\\" + product + "\\BD" + BUILD.ToString() + product + "\\genexe.pjx'");
                      parse.DoCmd("Locate");
                      parse.DoCmd("Replace HomeDir with '" + "D:\\Tracking\\BUILDS\\" + product + "\\BD" + BUILD.ToString() + product + "\\" + "'");
                      parse.DoCmd("Replace Reserved1 with '" + "D:\\Tracking\\BUILDS\\" + product + "\\BD" + BUILD.ToString() + product + "\\" + "GENEXE.PJX'");
                      parse.DoCmd("Replace Object with '" + "D:\\Tracking\\BUILDS\\" + product + "\\BD" + BUILD.ToString() + product + "\\" + "'");
                      parse.DoCmd("Replace Name with '" + "D:\\Tracking\\BUILDS\\" + product + "\\BD" + BUILD.ToString() + product + "\\" + "GENEXE.PJX'");
                      parse.DoCmd("USE");
                      //MMT
                      foxCommand = "DO D:\\FoxProProject\\ss.prg with '" + "BD" + BUILD.ToString() + "R13" + "','D:\\Tracking\\BUILDS\\" + product + "\\BD" + BUILD.ToString() + product + "\\'";
                      //System.IO.File.WriteAllText("D:\\Tracking\\" + "CheckBuildTrygenerations.TXT", foxCommand);
                      
                      parse.DefaultFilePath = @"D:\\FoxProProject\\";
                      parse.DoCmd(foxCommand);
                      Thread.Sleep(15000);
                      parse.Quit();
                    //  parse.DoCmd("SYS(2335,0)");
                     // parse.DoCmd(@"Build Exe 'D:\Tracking\Builds\EDI\BD142EDI\BD142EDI.EXE' From 'D:\Tracking\Builds\EDI\BD142EDI\genexe.pjx' ");
                     

                  }
                  catch (Exception ee)
                  {

                    //  System.IO.File.WriteAllText("D:\\Tracking\\" + "CheckBuildTrygenerations.TXT", "In Catch"+ee.Message);
                      //parse.DoCmd(@"Build Exe 'D:\Tracking\Builds\EDI\BD142EDI\BD142EDI.EXE' From 'D:\Tracking\Builds\EDI\BD142EDI\genexe.pjx' ");

                      foxCommand = "DO D:\\FoxProProject\\ss.prg with '" + "BD" + BUILD.ToString() + product + "','D:\\Tracking\\BUILDS\\" + product + "\\BD" + BUILD.ToString() + product + "\\'";
                      parse.DefaultFilePath = @"D:\\FoxProProject\\";
                      parse.DoCmd(foxCommand);
                      Thread.Sleep(15000);
                      parse.Quit();

                  /*    System.IO.File.WriteAllText("D:\\Tracking\\CheckBuildCatchygenerations.TXT", "In catxch");
                      var test1 = System.Diagnostics.Process.GetProcessById(parse.ProcessId);
                      test1.Close();
                      string tracking1 = product + BUILD;
                      var parse1 = new VisualFoxpro.FoxApplication();
                      string foxCommand1="";
                      //foxCommand1 = "DO D:\\FoxProProject\\ss.prg with '" + tracking1 + "','D:\\Tracking\\BUILDS\\" + product + "\\BD" + tracking1 + "R13" + "\\'";
                      foxCommand1 = "DO D:\\FoxProProject\\ss.prg with '" + "BD" + BUILD.ToString() + "EDI" + "','D:\\Tracking\\BUILDS\\" + product + "\\BD" + BUILD.ToString() + "EDI" + "\\'";
                      parse1.DefaultFilePath = @"D:\\FoxProProject\\";
                      parse1.DoCmd(foxCommand1);
                      var test = System.Diagnostics.Process.GetProcessById(parse1.ProcessId);
                      test.Close();
                      System.IO.File.WriteAllText("D:\\Tracking\\CheckBuildCatchygenerations.TXT", "In catxch");*/

                  }
                  finally
                  {

                  }
            //MMT
                  string NewWorkSpace = "";
                  string TfsUserName = "ProjectAdmin";
                  string TfsPassWord = "aria_123";
                  Uri TfsUri = new Uri("http://tf_server:8080/tfs/Aria-Fox");
                  if (ProjectName.Contains("4"))
                      NewWorkSpace = @"D:\Aria4xp\" + "Aria4xp";
                  else
                      NewWorkSpace = @"D:\EDI";

                  NetworkCredential cre = new NetworkCredential(TfsUserName, TfsPassWord, "TF_Server");
                  TfsTeamProjectCollection tfs = new TfsTeamProjectCollection(TfsUri, cre);
                  VersionControlServer vcs = tfs.GetService(typeof(VersionControlServer)) as VersionControlServer;
                  
                  if (vcs.TryGetWorkspace(NewWorkSpace) == null)
                  {
                      if (ProjectName.Contains("4"))
                      {
                          vcs.CreateWorkspace("Aria4XP", TfsUserName);
                      }
                      else {
                          vcs.CreateWorkspace("EDI", TfsUserName);
                      }

                  }
                  Workspace workspace;
                  if (ProjectName.Contains("4"))
                  {
                      workspace = vcs.GetWorkspace("Aria4XP", TfsUserName);
                      workspace.Map("$//Aria4XP", NewWorkSpace);
                      var LatestRequest = new GetRequest(new ItemSpec("$//Aria4xp", RecursionType.Full), VersionSpec.Latest);
                      // var results = workspace.Get(LatestRequest, GetOptions.GetAll | GetOptions.Overwrite);
                      PendingChanges = workspace.GetPendingChanges();
                  }
                  else
                  {
                      workspace = vcs.GetWorkspace("EDI", TfsUserName);
                      workspace.Map("$//EDI", NewWorkSpace);
                      var LatestRequest = new GetRequest(new ItemSpec("$//EDI", RecursionType.Full), VersionSpec.Latest);
                      // var results = workspace.Get(LatestRequest, GetOptions.GetAll | GetOptions.Overwrite);
                      PendingChanges = workspace.GetPendingChanges();
                  }

                  if (PendingChanges.Count() > 0)
                      workspace.Undo(PendingChanges);

                  if (ProjectName.Contains("4"))
                      vcs.DeleteWorkspace("Aria4xp", TfsUserName);
                  else
                      vcs.DeleteWorkspace("EDI", TfsUserName); 
            //MMT


                  //newinst.clearentryfolder("D:\\Tracking\\BUILDS\\" + product + "\\BD" + BUILD.ToString() + "R13" + "\\");
                  newinst.clearentryfolder("D:\\Tracking\\BUILDS\\" + product + "\\BD" + BUILD.ToString() + product + "\\");
                  newinst = null;
                  return true;
              }
        [WebMethod]
          public bool CreateBuildattachmenttable(Attachments[] Attachments, string BNUM,string PNAME)
        {

            createfixproject.createnewfix newinst = new createfixproject.createnewfix();
            //ATA create attachment table using fox code
                 if (PNAME.Contains("4"))
                  {
                      PNAME = "A40";  
                  }
                  else
                  {
                      PNAME = "EDI";
                  }
           // newinst.createattachmenttable("D:\\Tracking\\Builds\\"+PNAME+"\\BD" + BNUM + PNAME + "\\Temp\\Attachment.dbf", "Crep_id");
            newinst.createattachmenttable("D:\\Tracking\\Builds\\" + PNAME + "\\BD" + BNUM + PNAME + "\\Temp\\Attachment.dbf", "Crep_id");

            System.Data.OleDb.OleDbConnection OleConn = new System.Data.OleDb.OleDbConnection();
            OleConn.ConnectionString = @"Provider=VFPOLEDB.1;Data Source=D:\";
            OleConn.Open();


            for (int i = 0; i < Attachments.GetLength(0); i++)
            {
                if (Attachments[i] != null)//&& Attachments[i].Type.ToString() == "System")
                {
                    if (Attachments[i].Type.ToString() == "System")
                    {
                        Attachments[i].Key = newinst.GetKey("D:\\Tracking\\Builds\\" + PNAME + "\\BD" + BNUM + PNAME + "\\attachments\\sysfiles\\" + Attachments[i].Name + "", Attachments[i].cKeyExpression.ToString());
                    }
                    else
                    {
                        var di = new DirectoryInfo(Attachments[i].Source);
                        di.Attributes &= ~FileAttributes.ReadOnly;
                    }
                    string insertquery = "Insert into D:\\Tracking\\Builds\\" + PNAME + "\\BD" + BNUM + PNAME + "\\Temp\\Attachment.dbf (Name,Source,Dest,Tag,Key,Type,Transfer,Verify,ldontcomp,lSysData,cObjType) Values('" + Attachments[i].Name + "','" + Attachments[i].Source + "','" + Attachments[i].Dest + "','" + Attachments[i].Tag + "','" + Attachments[i].Key + "','" + Attachments[i].Type + "',.F.,.T.,.F.,.T.,'s')";

                    //  System.IO.File.WriteAllText("D:\\Tracking\\Fixes\\" + TNUM + "\\Checkfixgenerations2.TXT", insertquery);
                    System.Data.OleDb.OleDbCommand OleComm1 = new System.Data.OleDb.OleDbCommand(insertquery, OleConn);
                    OleComm1.ExecuteNonQuery();
                   // string entrytype = BNUM.Substring(0, 1);
                    //string entryid = BNUM.Substring(1, TNUM.Length - 1);
                    var result = newinst.checktheattachment("D", BNUM, Attachments[i].cObjType, Attachments[i].Name, "", Attachments[i].Tag, Attachments[i].Key);
                    if (!result)
                    {
                        newinst.addrosuautoprc(BNUM, "D", Attachments[i].cObjType, Attachments[i].Source, Attachments[i].Dest, Attachments[i].Name, Attachments[i].cObjType, Attachments[i].Tag, Attachments[i].Key, false, false, "U");
                        //string insertinstuatuoprc = "insert into 'D:\\Tracking\\SUAUTPRC.DBF'(centryid,centrytype,cobjecttyp,csource,cdisten,cobjname,cobjtype,ctag,ckey,ldontcomp,lsys_data,CPROSSTYPE,MCONTENT,MUPDMODULS,CADD_USER) Values ('" + entryid + "','" + entrytype + "','" + Attachments[i].cObjType + "','" + Attachments[i].Source + "','" + Attachments[i].Dest + "','" + Attachments[i].Name + "','" + Attachments[i].cObjType + "','" + Attachments[i].Tag + "','" + Attachments[i].Key + "',.F.,.F.,'U','','','')";
                        //  System.Data.OleDb.OleDbCommand OleComm2 = new System.Data.OleDb.OleDbCommand(insertinstuatuoprc, OleConn);
                        // OleComm2.ExecuteNonQuery();
                    }

                }

            }
            OleConn.Close();
            newinst = null;


            return true;
        }
        // Sara.n Handle FOX Build 11/06/2018  [End]
       // #endregion
       
private void EmptyFolder(DirectoryInfo directoryInfo)
{
    foreach (FileInfo file in directoryInfo.GetFiles())
    {       
       file.Delete();
     }

    foreach (DirectoryInfo subfolder in directoryInfo.GetDirectories())
    {
      EmptyFolder(subfolder);
    }
}
    }
}

