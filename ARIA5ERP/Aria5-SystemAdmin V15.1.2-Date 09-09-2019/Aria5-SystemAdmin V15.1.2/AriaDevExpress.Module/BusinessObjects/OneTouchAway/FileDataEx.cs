using System;
using System.IO;
using DevExpress.Xpo;
using System.ComponentModel;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;
using Microsoft.WindowsAzure;
using Microsoft.WindowsAzure.StorageClient;
using System.Web;
using System.Net;
using DevExpress.Data.Filtering;

//Dennis: the OTAWAYFileDataEx class allows you to store files in file system instead of the database. You can set the required path in the DefaultDirectory constant.
namespace AriaDevExpress.Module.BusinessObjects.OneTouchAway
{
    [DevExpress.Xpo.Custom("Caption", "Uploaded Files")]
    [Persistent]
    [DefaultProperty("FileName")]
    public class OTAWAYFileDataEx : FileAttachmentBase, IFileData, IEmptyCheckable// BaseObject, //,BaseObject //, IFileData, 
    {

        // Sara.N ,30/07/2015 Upload Staging Website Images to Live Website [Start]
        private static string _StaginguserName = System.Configuration.ConfigurationManager.AppSettings["StagingFTBUsername"];
        // Sara.N, 30/07/2015 Upload Staging Website Images to Live Website [END]

        // Sara.N 26/04/2015 [Tracking# + Aria5-DevExpress-Build]_Programming [Start] 
        private static string DefaultDirectory = System.Configuration.ConfigurationManager.AppSettings["FTBDefaultDirctory"];
        private static string DefaultDirectory1 = System.Configuration.ConfigurationManager.AppSettings["FTBDefaultDirctory1"];
        private static string uploadDirectory = System.Configuration.ConfigurationManager.AppSettings["FTBUploadDirctory"];
        private static string _userName = System.Configuration.ConfigurationManager.AppSettings["FTBUsername"];
       
        private static string _password = System.Configuration.ConfigurationManager.AppSettings["FTBPassword"];
        private static string fileNameCopy = "";
        protected OTAWAYFileDataEx UploadImage;
        private static int attempt = 1;
        protected override void OnSaving()
        {
            if (this.File != null)
            {
                this.File.FileName = this.FileName;
                this.File.Content = this.Content;

               // UploadFileFTP("ftp://waws-prod-ch1-001", _userName, _password, FileName, Content, @".ftp.azurewebsites.windows.net/site/wwwroot/images/");
                UploadFileFTP(DefaultDirectory1, _userName, _password, FileName, Content, uploadDirectory);
                FileName = fileNameCopy;
            }
            base.OnSaving();
        }
        // Sara.N 26/04/2015 [Tracking# + Aria5-DevExpress-Build]_Programming[END]

        private const int ReadBytesSize = 0x1000;
        private Stream tempStream = null;
        private string tempFileName = string.Empty;
        [Persistent("Size")]
        private int _Size;
        [PersistentAlias("_Size")]
        public int Size
        {
            get { return _Size; }
        }
        public OTAWAYFileDataEx(Session session) : base(session) { }
        public override string ToString()
        {
            return FileName;
        }
        private byte[] _Content;
        [Browsable(false)]
        [MemberDesignTimeVisibility(false)]
        [EditorBrowsable(EditorBrowsableState.Never)]
        public byte[] Content
        {
            get { return _Content; }
            set { SetPropertyValue("Content", ref _Content, value); }
        }
        [NonPersistent, MemberDesignTimeVisibility(false)]
        public bool IsEmpty
        {
            get { return string.IsNullOrEmpty(_FileName) && !Exists; }
        }
        public override void AfterConstruction()
        {
            base.AfterConstruction();
        }
        private string _FileName = string.Empty;
        [Size(260)]
        public string FileName
        {
            get { return _FileName; }
            set { SetPropertyValue("FileName", ref _FileName, value); }
        }
        public string RealFileName
        {
            get
            {

                return _FileName != string.Empty ? Path.Combine(DefaultDirectory, _FileName) : string.Empty;
            }
        }
        protected virtual string GetUniqueFileName(string shortFileName)
        {
            return string.Format("{0}-{1}", Oid, shortFileName);
        }
        protected virtual string GetShortFileName(string uniqueFileName)
        {
            return uniqueFileName.Replace(string.Format("{0}-", Oid), string.Empty).Replace(DefaultDirectory, string.Empty);
        }

        public bool Exists
        {
            get
            {
                return System.IO.File.Exists(RealFileName);
            }
        }

        //Dennis: fires when assigning a file.
        public void LoadFromStream(string fileName, Stream source)
        {
      
            tempStream = source;
            //Dennis: when assigning a new file we need to save the name of the old file to remove it from the store in the future.
            if (string.IsNullOrEmpty(tempFileName))
            {
                tempFileName = RealFileName;
            }
            //sara.N 26/04/2015 [Tracking# + Aria5-DevExpress-Build]_Programming[Start]
            FileName = fileName;


            using (BinaryReader br = new BinaryReader(source))
            {
                Content = br.ReadBytes((int)source.Length);
            }

            this.File = new FileData(this.Session);
            this.File.FileName = FileName;
            this.File.LoadFromStream(fileName, source);
          
            //sara.N[END]
        }
        //Dennis: fires when saving or opening a file.
        public void test()
        {
        }
        public void SaveToStream(Stream destination)
        {
            try
            {
                FtpWebRequest fileReq = (FtpWebRequest)HttpWebRequest.Create(RealFileName);

                fileReq.Credentials = new NetworkCredential(_userName, _password);

                //Create a response for this request
                FtpWebResponse fileResp = (FtpWebResponse)fileReq.GetResponse();

                if (fileReq.ContentLength > 0)
                    fileResp.ContentLength = fileReq.ContentLength;

                //Get the Stream returned from the response
                Stream source = fileResp.GetResponseStream();

                byte[] buffer = new byte[ReadBytesSize];
                int read = 0;
                while ((read = source.Read(buffer, 0, buffer.Length)) > 0)
                {
                    destination.Write(buffer, 0, read);
                }


            }
            catch (DirectoryNotFoundException exc)
            {
                throw new Exception(string.Format("Cannot access the '{0}' store because it is not available.", DefaultDirectory), exc);
            }
            catch (FileNotFoundException exc)
            {
                throw new Exception(string.Format("Cannot access the '{0}' file because it is not found in the store.", FileName), exc);
            }



        }
        // Sara.N  26/04/2015 [Tracking# + Aria5-DevExpress-Build]_Programming [Start]
        //protected override void OnSaving()
        //{

        //    base.OnSaving();
        //    if (tempStream != null && !string.IsNullOrEmpty(RealFileName))
        //    {
        //        Stream source = null;
        //        //Dennis: for Windows Forms applications.
        //        if (tempStream is FileStream)
        //        {
        //            try
        //            {
        //                source = File.OpenRead(((FileStream)tempStream).Name);
        //            }
        //            catch (FileNotFoundException exc)
        //            {
        //                throw new Exception(string.Format("Cannot read the '{0}' file from a temporary store.", FileName), exc);
        //            }
        //        }
        //        else
        //        {
        //            //Dennis: for Web Forms applications.
        //            source = tempStream;
        //        }
        //        try
        //        {

        //            UpdateSize(source);
        //            SaveFile_Azure(source);

        //        }
        //        catch (DirectoryNotFoundException exc)
        //        {
        //            throw new Exception(string.Format("Cannot access the '{0}' store because it is not available.", DefaultDirectory), exc);
        //        }
        //        finally
        //        {
        //            source.Close();
        //            source = null;
        //        }

        //    }
        //    ////Dennis: removing the old file from the store when saving the current object.
        //    //if (!string.IsNullOrEmpty(tempFileName)) {
        //    //    try {
        //    //        File.Delete(tempFileName);
        //    //        tempFileName = string.Empty;
        //    //    } catch (DirectoryNotFoundException exc) {
        //    //        throw new Exception(string.Format("Cannot access the '{0}' store because it is not available.", DefaultDirectory), exc);
        //    //    }
        //    //}
        //}
        //protected override void OnDeleting()
        //{
        //    //Dennis: first we need to remove the old file from the store.
        //    Clear();
        //    base.OnDeleting();
        //}

        // Sara.N 26/04/2015 [Tracking# + Aria5-DevExpress-Build]_Programming [END]
        private void UpdateSize(Stream stream)
        {
            _Size = stream == null ? 0 : (int)stream.Length;
        }
        public void Clear()
        {
            //Dennis: when clearing the file name property we need to save the name of the old file to remove it from the store in the future.
            if (string.IsNullOrEmpty(tempFileName))
            {
                tempFileName = RealFileName;
            }
            //DeleteFile_Azure();
            FileName = string.Empty;
            UpdateSize(null);
        }
        //Sara.N 26/04/2015 [Tracking# + Aria5-DevExpress-Build]_Programming [ Start]
        //private void SaveFile_Azure(Stream FileStream)
        //{
        //    // Setup the connection to Windows Azure Storage
        //    var storageAccount = CloudStorageAccount.Parse("DefaultEndpointsProtocol=https;AccountName=ariaftp;AccountKey=OFtTaMXef67OwHa06zqI2MBVlBzq5D8tMMHG69Zcf1dzJ+RZGVO4OkvvsYjY88O/h2CCEJepNWFt3MH4pCMt9A==");
        //    _BlobClient = storageAccount.CreateCloudBlobClient();

        //    // For large file copies you need to set up a custom timeout period
        //    // and using parallel settings appears to spread the copy across multiple threads
        //    // if you have big bandwidth you can increase the thread number below
        //    // because Azure accepts blobs broken into blocks in any order of arrival.
        //    _BlobClient.Timeout = new System.TimeSpan(1, 0, 0);
        //    _BlobClient.ParallelOperationThreadCount = 2;

        //    // Get and create the container
        //    _BlobContainer = _BlobClient.GetContainerReference("ariany");
        //    _BlobContainer.CreateIfNotExist();

        //    // Setup the permissions on the container to be public
        //    var permissions = new BlobContainerPermissions();
        //    permissions.PublicAccess = BlobContainerPublicAccessType.Container;
        //    _BlobContainer.SetPermissions(permissions);

        //    // Make a unique blob name
        //    string extension = System.IO.Path.GetExtension(_FileName);




        //    // Create the Blob and upload the file
        //    var blob = _BlobContainer.GetBlobReference(_FileName);
        //    blob.DeleteIfExists();
        //    blob.UploadFromStream(FileStream);

        //    // Set the metadata into the blob
        //    blob.Metadata["FileName"] = _FileName;
        //    blob.Metadata["Submitter"] = "Automated Encoder";
        //    blob.SetMetadata();

        //    // Set the properties
        //    blob.Properties.ContentType = "Text/x-ms-pdf";
        //    blob.SetProperties();


        //}
        //private void DeleteFile_Azure()
        //{
        //    // Setup the connection to Windows Azure Storage
        //    var storageAccount = CloudStorageAccount.Parse("DefaultEndpointsProtocol=https;AccountName=ariaftp;AccountKey=OFtTaMXef67OwHa06zqI2MBVlBzq5D8tMMHG69Zcf1dzJ+RZGVO4OkvvsYjY88O/h2CCEJepNWFt3MH4pCMt9A==");
        //    _BlobClient = storageAccount.CreateCloudBlobClient();

        //    // For large file copies you need to set up a custom timeout period
        //    // and using parallel settings appears to spread the copy across multiple threads
        //    // if you have big bandwidth you can increase the thread number below
        //    // because Azure accepts blobs broken into blocks in any order of arrival.
        //    _BlobClient.Timeout = new System.TimeSpan(1, 0, 0);
        //    _BlobClient.ParallelOperationThreadCount = 2;

        //    // Get and create the container
        //    _BlobContainer = _BlobClient.GetContainerReference("ariany");
        //    _BlobContainer.CreateIfNotExist();

        //    // Setup the permissions on the container to be public
        //    var permissions = new BlobContainerPermissions();
        //    permissions.PublicAccess = BlobContainerPublicAccessType.Container;
        //    _BlobContainer.SetPermissions(permissions);

        //    // Make a unique blob name
        //    string extension = System.IO.Path.GetExtension(_FileName);

        //    //Open the stream and read it back.


        //    // Create the Blob and upload the file
        //    var blob = _BlobContainer.GetBlobReference(_FileName);
        //    //var x = _BlobContainer.ListBlobs();
        //    //foreach (var xx in x)
        //    //{
        //    //    string cc = xx.Uri.AbsolutePath;
        //    //}
        //    blob.DeleteIfExists();
        //}

        public static string UploadFileFTP(string FtpUrl, string userName, string password, string fileName, byte[] Content, string UploadDirectory = "")
        {  // Sara.N ,30/07/2015 Error When upload file and Delete it before save  [Start]
            string file = fileName;

            if (!string.IsNullOrWhiteSpace(file))
            {
            // Sara.N ,30/07/2015 Error When upload file and Delete it before save  [END]
                string fileNamebf;
                attempt = 1;
                string destination = FtpUrl;

                string extention = Path.GetExtension(file);

                fileNamebf = file.Remove(file.Length - extention.Length);



                fileNameCopy = fileName;

                string PureFileName = fileName;
                var request = (FtpWebRequest)WebRequest.Create(String.Format("{0}{1}{2}", FtpUrl, UploadDirectory, PureFileName));
                request.Method = WebRequestMethods.Ftp.GetFileSize;
                request.Credentials = new NetworkCredential(userName, password);


                while (!checkFileExists(request))
                {
                    fileNameCopy = fileNamebf + " (" + attempt.ToString() + ")" + "." + extention;
                    request = (FtpWebRequest)WebRequest.Create(String.Format("{0}{1}{2}", FtpUrl, UploadDirectory, fileNameCopy));
                }

                PureFileName = fileNameCopy;

                String uploadUrl = String.Format("{0}{1}{2}", FtpUrl, UploadDirectory, PureFileName);
                FtpWebRequest req = (FtpWebRequest)FtpWebRequest.Create(uploadUrl);
                req.Proxy = null;
                req.Method = WebRequestMethods.Ftp.UploadFile;
                req.Credentials = new NetworkCredential(userName, password);
                req.UseBinary = true;
                req.UsePassive = true;
                byte[] data = Content;


                req.ContentLength = data.Length;
                Stream stream = req.GetRequestStream();
                stream.Write(data, 0, data.Length);
                stream.Close();
                FtpWebResponse res = (FtpWebResponse)req.GetResponse();


                return res.StatusDescription;
            }
            else
            {
                return "";
            }
        }

        private static bool checkFileExists(WebRequest request)
        {
            FtpWebResponse response;
            request.Credentials = new NetworkCredential(_userName, _password);
            try
            {
                response = (FtpWebResponse)request.GetResponse();

                if (response.StatusCode == FtpStatusCode.ActionNotTakenFileUnavailable)
                {
                    return true;
                }

                else { attempt++; return false; }

            }
            catch
            {
                return true;
            }

        }
        //sara.N 26/04/2015 [Tracking# + Aria5-DevExpress-Build]_Programming [END]


    }
}