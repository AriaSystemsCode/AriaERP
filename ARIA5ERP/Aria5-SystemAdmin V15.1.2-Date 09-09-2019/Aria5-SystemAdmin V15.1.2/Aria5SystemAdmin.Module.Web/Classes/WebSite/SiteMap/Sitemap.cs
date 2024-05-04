using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.Net;
using Microsoft.WindowsAzure.ServiceRuntime;
using Microsoft.WindowsAzure.StorageClient;
using Microsoft.WindowsAzure;
using System.IO;

namespace AriaDevExpress.Module.Web.Classes.WebSite.SiteMap
{
    class Sitemap
    {
        private CloudBlobClient _BlobClient = null;
        private CloudBlobContainer _BlobContainer = null;
        private string _filename;
        private string _FileName = "AriaNY.xml";
        private XmlTextWriter _sitemapxml;
        private string _xmlns = "http://www.sitemaps.org/schemas/sitemap/0.9";
        private MemoryStream stream = new MemoryStream();
        public string Xmlns
        {
            get
            {
                return _xmlns;
            }

            set
            {
                _xmlns = value;
            }
        }

        //Constructor
        public Sitemap(string filename)
        {
            this._filename = filename;


            _sitemapxml = new XmlTextWriter(stream, UTF8Encoding.UTF8);
            _sitemapxml.WriteStartDocument();
            _sitemapxml.WriteStartElement("urlset");
            _sitemapxml.WriteAttributeString("xmlns", _xmlns);


            _sitemapxml.Flush();


            // byte[] byteArray = Encoding.ASCII.GetBytes(test);
            // MemoryStream stream = new MemoryStream(byteArray);

        }

        public void Add(Url _url)
        {
            _sitemapxml.WriteStartElement("url");
            _sitemapxml.WriteElementString("loc", _url.Loc);

            if (_url.LastModifiedString != "")
            {
                _sitemapxml.WriteElementString("lastmod", _url.LastModifiedString);
            }

            if (_url.ChangeFreq != "")
            {
                _sitemapxml.WriteElementString("changefreq", _url.ChangeFreq);
            }

            if (_url.Priority != "")
            {
                _sitemapxml.WriteElementString("priority", _url.Priority);
            }

            _sitemapxml.WriteEndElement();
            _sitemapxml.Flush();

        }
        public void Write()
        {
            _sitemapxml.WriteEndElement();
            _sitemapxml.WriteEndDocument();

            string result;


            StreamReader reader = new StreamReader(stream, Encoding.UTF8, true);
            stream.Seek(0, SeekOrigin.Begin);
            result = reader.ReadToEnd();
            result += "</urlset>";
            SaveFile_Azure(result);
            _sitemapxml.Close();

        }
        private void SaveFile_Azure(string Result)
        {
            // Setup the connection to Windows Azure Storage
            var storageAccount = CloudStorageAccount.Parse("DefaultEndpointsProtocol=https;AccountName=ariaftp;AccountKey=OFtTaMXef67OwHa06zqI2MBVlBzq5D8tMMHG69Zcf1dzJ+RZGVO4OkvvsYjY88O/h2CCEJepNWFt3MH4pCMt9A==");
            _BlobClient = storageAccount.CreateCloudBlobClient();

            // For large file copies you need to set up a custom timeout period
            // and using parallel settings appears to spread the copy across multiple threads
            // if you have big bandwidth you can increase the thread number below
            // because Azure accepts blobs broken into blocks in any order of arrival.
            _BlobClient.Timeout = new System.TimeSpan(1, 0, 0);
            _BlobClient.ParallelOperationThreadCount = 2;

            // Get and create the container
            _BlobContainer = _BlobClient.GetContainerReference("ariany");
            _BlobContainer.CreateIfNotExist();

            // Setup the permissions on the container to be public
            var permissions = new BlobContainerPermissions();
            permissions.PublicAccess = BlobContainerPublicAccessType.Container;
            _BlobContainer.SetPermissions(permissions);

            // Make a unique blob name
            string extension = System.IO.Path.GetExtension(_FileName);




            // Create the Blob and upload the file
            var blob = _BlobContainer.GetBlobReference(_FileName);
            blob.DeleteIfExists();
            blob.UploadText(Result);

            // Set the metadata into the blob
            blob.Metadata["FileName"] = _FileName;
            blob.Metadata["Submitter"] = "Automated Encoder";
            blob.SetMetadata();

            // Set the properties
            blob.Properties.ContentType = "text/xml";
            blob.SetProperties();


        }
        public static string Ping(String sitemapFileURL)
        {
            string Response = "";
            string STD_PING_PATH = "/ping?sitemap=" + sitemapFileURL;

            // GOOGLE
            try
            {
                WebRequest request = System.Net.HttpWebRequest.Create("http://www.google.com/webmasters/tools" + STD_PING_PATH);
                WebResponse response = request.GetResponse();
                Response += "\n Google: Pass";
            }
            catch (Exception e)
            {
                // TODO: handle this error!
                Response += "\n Google: " + e.Message;
            }

            // YAHOO
            try
            {
                // notice how they are not following the standard....
                WebRequest request = System.Net.HttpWebRequest.Create("http://www.bing.com/webmaster" + sitemapFileURL);
                WebResponse response = request.GetResponse();
                Response += "\n Bing: Pass";
            }
            catch (Exception e2)
            {
                // TODO: handle error
                Response += "\n Bing: " + e2.Message;
            }

            // ASK.COM
            try
            {
                WebRequest request = System.Net.HttpWebRequest.Create("http://submissions.ask.com" + STD_PING_PATH);
                WebResponse response = request.GetResponse();
                Response += "\n ASK.com: Pass";
            }
            catch (Exception e3)
            {
                // TODO: handle error!
                Response += "\n ASK.com: " + e3.Message;
            }

            return Response;
        }
    }
}
