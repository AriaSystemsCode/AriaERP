using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Data.Filtering;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Aria5SystemAdmin.Module.Managers
{
    public class ApplicationPackageManager
    {
        private static TfsManager tfsManager;
        public ApplicationPackageManager()
        {
            TextWriterTraceListener writer = new TextWriterTraceListener(System.Console.Out);
            Debug.Listeners.Add(writer);
            tfsManager = new TfsManager();
        }
        public enum Buildtype
        {
            Staging,
            Production
        };

        public static bool GetSplitMetaData(ApplicationBuild_T build)
        {
          //  tfsManager.CreateWorkSpace(build);
            Application_T App = build.Application_T;
            string maxRevNo = "000000";
            Guid maxRevGuid = Guid.Empty;
            AriaObjectRevision maxAriaObjectRevision = null;
            //Read output path from application settings collection for setting id is "Resource Folder".
          //  ApplicationSetting resourceFolder = build.Application_T.ApplicationSettings.First(r => r.SettingID == "Resource Folder");
           // string resourceValue = resourceFolder.SettingValue;
            //Loop on Application Entities for all entities belong to this application.
            foreach (AriaObject ariaObject in App.AriaObjects)
            {

                if( ariaObject.ObjectType.Name.ToString().ToUpper().Trim() == "ENTITY" )
                {                    
                    //Loop on revisions of an entity.
                    foreach (AriaObjectRevision ariaObjectRevision in ariaObject.AriaObjectRevisions)
                    {
                        //Check if rivision tracking number in the current build tracking entries.                   
                       // if (build.ApplicationBuildEntries_Ts.Where(x => x.TrackingEntry == ariaObjectRevision.TrackingNo ).Count() > 0)
                            if (build.ApplicationTrackingEntries.Where(x => x == ariaObjectRevision.TrackingNo).Count() > 0)
                            {
                            //get this revision as Max entity revision. 
                            if(Convert.ToInt32(ariaObjectRevision.ObjectRevision) > Convert.ToInt32(maxRevNo)) 
                            {
                                maxRevGuid = ariaObjectRevision.Oid;
                                maxRevNo = ariaObjectRevision.ObjectRevision;
                                maxAriaObjectRevision = ariaObjectRevision;                              
                            }
                        }
                        //If revision all tracking numbers are not in the current build
                        else
                        {
                            //Get previous Application builds and sort them in descending order.
                            List<Aria5SystemAdmin.Module.BusinessObjects.ApplicationBuild_T> buildList = new List<ApplicationBuild_T>();
                            
                            //If build number is null then it is the greatest build.
                            if (build.ApplicationBuildId == null) 
                            {
                                buildList = App.ApplicationBuild_Ts.Where(r => r.Oid != build.Oid).OrderByDescending(r => r.ApplicationBuildId).ToList();
                            }
                            //If build number is not null then there may be other build greater than current build.
                            else 
                            {
                                buildList = App.ApplicationBuild_Ts.Where(r => int.Parse(r.ApplicationBuildId) < int.Parse(build.ApplicationBuildId)).OrderByDescending(r => r.ApplicationBuildId).ToList();
                            }

                            //Loop on previous Application builds.
                            foreach (ApplicationBuild_T ApplicationBuild_t in buildList)
                            {
                                //Check if rivision tracking number in the this build tracking entries.
                               // if (ApplicationBuild_t.ApplicationBuildEntries_Ts.Where(x => x.TrackingEntry == ariaObjectRevision.TrackingNo).Count() > 0)
                                if (ApplicationBuild_t.ApplicationTrackingEntries.Where(x => x == ariaObjectRevision.TrackingNo).Count() > 0)

                                    {
                                        maxRevGuid = ariaObjectRevision.Oid;
                                    maxRevNo = ariaObjectRevision.ObjectRevision;
                                    maxAriaObjectRevision = ariaObjectRevision;
                                    break;                            
                                }                           
                            }
                        }                      
                    }                       
                }

                //Check if maxAriaObjectRevision object is not null if its null then no max revision is found and do nothing
                if (maxAriaObjectRevision != null)
                {
                    //Call splitMetaData function with Entity and max revision we got before.

                    if (maxAriaObjectRevision.AriaObjectSettings.Where(r => r.SettingType != null && r.SettingType.Name != null && r.SettingType.Name.ToUpper().Trim() == "FilePath".ToUpper().Trim()).Count() > 0)
                    {
                        string xmlstring = ariaObject.SpliteMetadata(ariaObject.Oid, maxAriaObjectRevision.Oid);

                        var settingFilePath = maxAriaObjectRevision.AriaObjectSettings.First(r => r.SettingType != null && r.SettingType.Name != null && r.SettingType.Name.ToUpper().Trim() == "FilePath".ToUpper().Trim()).Value.ToString();

                       // tfsManager.UpdateMetadataTFS(xmlstring, settingFilePath, build);
                    }

                    
                    //Create a file named with <EntityName>_MetaData.XML and save it under Output path.
                    //System.IO.File.WriteAllText(resourceValue + ariaObject.ObjectName + "_MetaData.XML", xmlstring);
                    maxAriaObjectRevision = null;
                    maxRevNo = "000000";
                    maxRevGuid = Guid.Empty;
                }
            }
            return true;
        }

        public static bool GetBuildFiles(ApplicationBuild_T build)
        {
           
            //Get the currnet application from application build object
            Application_T App = build.Application_T;


            //Read TFS Main Project name, from application settings collection for setting id is ""TFS Main Project Name"".
            ApplicationSetting TFSProjectName = build.Application_T.ApplicationSettings.First(r => r.SettingID == "TeamProject");
            string TFSProjectNameValue = TFSProjectName.SettingValue;

            //Loop on Application Build tracking entries.
           // foreach (ApplicationBuildEntries_T buildEntry in build.ApplicationBuildEntries_Ts)
                foreach (TrackingEntry buildEntry in build.ApplicationTrackingEntries)

                {
                //loop on all related shelves
                  // foreach (AriaObjectShelve shelve in buildEntry.TrackingEntry.AriaObjectShelves)
                    foreach (AriaObjectShelve shelve in buildEntry.AriaObjectShelves)
                 {
                     //Get the attribute change set, call the TFSManager with the name of the application project, the object name , object extension from object setting and the ChangeSet #.
                     //Debug.WriteLine("call the TFSManager with the name of the application project, the object name , object extension from object setting and the ChangeSet # which is:" + shelve.ChangeSet);
                   //  tfsManager.GetSpecificChangeset(build, int.Parse(shelve.ChangeSet));
                 }
            }
      //      tfsManager.CreateWorkSpaceLabel(build);          
          
            return true;
        }


        public static bool Generate(ApplicationBuild_T build, Buildtype BuildType)
        {
            //- Retreive Application build data, using passed parameter.
            //- Get the currnet application from application build object.
            Application_T App = build.Application_T;
            //Call method SplitMetaData with build as parameter.
            bool SplitMetaDataResult = GetSplitMetaData(build);
            //if GetSplitMetaData() function return false throw exception.
            if (!SplitMetaDataResult)
            {
                throw new Exception("Error");              
            }
            ApplicationBuild_T lastBuild = new ApplicationBuild_T(build.Session);
            //Get last Build from Application collection.
            if (build.ApplicationBuildId == null)
            {
                List<ApplicationBuild_T> buildList = App.ApplicationBuild_Ts.Where(r => r.ApplicationBuildId != null).OrderByDescending(r => r.ApplicationBuildId).ToList();
                if(buildList.Count >0)
                {
                    lastBuild = buildList.First();
                }
            }
            else
            {
                List<ApplicationBuild_T> buildList = App.ApplicationBuild_Ts.Where(r => int.Parse(r.ApplicationBuildId) < int.Parse(build.ApplicationBuildId)).OrderByDescending(r => r.ApplicationBuildId).ToList();
                if (buildList.Count > 0)
                {
                    lastBuild = buildList.First();
                }
            }
            Debug.WriteLine("Call TFS Manager to extract the application objects in that TFS label(*) with Application name, TFSLabel as parameters which is: " + lastBuild.TFSLabel);
            
            //Call method GetBuildFiles with build as parameter.
            bool BuildFilesResult = GetBuildFiles(build);

            //If GetBuildFiles() function returns false throw exception message.
            if (!BuildFilesResult)
            {
                throw new Exception("Error");
            }
            
            //Read output path from application settings collection for setting id is ""Resource Folder"".
         //   ApplicationSetting resourceFolder = build.Application_T.ApplicationSettings.First(r => r.SettingID == "Resource Folder");

            Debug.WriteLine("Create a file named with BuildType.XML and save it under package drop folder.");

            //If the build Type parameter is sent as Test,  append the BuildType.XML file with ""Test"".
            if(BuildType == Buildtype.Staging)
            {
                System.IO.File.WriteAllText(@"D:\BuildType.XML", "Test");
            }
            //- If the build Type parameter is sent as Production,  append the BuildType.XML file with ""Production"".
            else if (BuildType == Buildtype.Production)
            {
                System.IO.File.WriteAllText(@"D:\BuildType.XML", "Production");
            }

            ApplicationSetting buildDef = build.Application_T.ApplicationSettings.First(r => r.SettingID == "BuildDefintion");

            //tfsManager.BuildDeploy(build, buildDef.SettingValue);

            UploadGeneraedBuild(build);

            return true;
        }

        public static bool UploadGeneraedBuild(ApplicationBuild_T build)
        {
            // As for now this will be just stub 
            // In the future, will be upload based on the application type and settings to different App stores, or FTP.... 
            return true;
        }
    }
}
