namespace AWS
{
    using System;
    using System.Collections.Generic;
    using System.Data.SqlClient;
    using System.Diagnostics;
    using System.IO;
    using System.Xml;
    using MarketplaceWebService;
    using MarketplaceWebService.Model;
    using System.Threading;
    public class Main
    {
        string marketplaceId, merchantId, CompanyId, accessKeyId, secretAccessKey, inboxPath, outboxPath, OrignalInName, InName, OutName, lcXMLFileName, Aria4Con;
        const string applicationName = "Amazon MWS";
        const string applicationVersion = "1";
        const string OrderAckFeed = "OrderAcknowledgement", OrderFulfillFeed = "OrderFulfillment";
        int NameSequence;
        MarketplaceWebService service;
        AMAZON_TRANSMISSION_LOG_T.AMAZON_TRANSMISSION_LOG_TRow currentlog;
        AmazonLog amazonLog = new AmazonLog();
        //DateTime LastGetReportList;
        IdList SuccessfullyReceivedReports;
        FileStream AppRuning = null;

        public void Init(string Requestid, string lcXMLFileName, string ClientId)
        {
            try
            {
                if (IsAlreadyRunning())
                    return;
                this.lcXMLFileName = lcXMLFileName;
                InitNewLogRow();
                ReadSettings();
                amazonLog.con = new SqlConnection(Aria4Con);
                InitSettings();
                CreateService(); ;
                ManageOrderSchdule("_GET_ORDERS_DATA_", Schedule._8_HOURS_);
                GetReports(ReportType.OrderReports);
                GetReports(ReportType.Settlement);
                UploadFeed();
                AppRuning.Close();
            }
            catch (MarketplaceWebServiceException ex)
            {
                currentlog.Error += "MarketplaceWebServiceException Exception occured. .\r\n";
                currentlog.Error += "Caught Exception: " + ex.Message + " .\r\n";
                currentlog.Error += "Response Status Code: " + ex.StatusCode + " .\r\n";
                currentlog.Error += "Error Code: " + ex.ErrorCode + " .\r\n";
                currentlog.Error += "Error Type: " + ex.ErrorType + " .\r\n";
                currentlog.Error += "Request ID: " + ex.RequestId + " .\r\n";
                currentlog.Error += "XML: " + ex.XML + " .\r\n";
                currentlog.STATUS = Status.Failed.ToString();
                amazonLog.insert(currentlog);
            }
            catch (Exception ex)
            {
                currentlog.Error += "Exception occured. .\r\n";
                currentlog.Error += "Caught Exception: " + ex.Message + " .\r\n";
                currentlog.Error += ex.InnerException != null ? "Inner Exception: " + ex.InnerException.Message + " .\r\n" : "";
                currentlog.STATUS = Status.Failed.ToString();
                amazonLog.insert(currentlog);
            }
        }

        private bool IsAlreadyRunning()
        {
            try
            {
                AppRuning = File.Open(System.AppDomain.CurrentDomain.BaseDirectory.ToString() + @"\RuningNow.txt", FileMode.OpenOrCreate);
            }
            catch (Exception)
            {
                return true;
            }
            return false;
        }

        private void InitSettings()
        {
            // LastGetReportList = DateTime.MinValue;
            if (!Directory.Exists(inboxPath)) Directory.CreateDirectory(inboxPath);
            if (!Directory.Exists(outboxPath)) Directory.CreateDirectory(outboxPath);
            OrignalInName = InName;
            InName = GetInFileName(InName);
            NameSequence = 1;
            SuccessfullyReceivedReports = new IdList();
            AddXMltoLog();
        }

        private void InitNewLogRow()
        {
            currentlog = new AMAZON_TRANSMISSION_LOG_T.AMAZON_TRANSMISSION_LOG_TDataTable().NewAMAZON_TRANSMISSION_LOG_TRow();
            currentlog.DATE = DateTime.Now;
            currentlog.Downloaded = 0;
            currentlog.Error = "";
        }

        private void AddXMltoLog()
        {
            currentlog.InputXML = File.ReadAllText(lcXMLFileName);
            currentlog.Progress = "Init Settings Completed .\r\n";
        }

        public void ReadSettings()
        {
            if (File.Exists(lcXMLFileName))
            {
                XmlDocument xmldoc = new XmlDocument();
                xmldoc.Load(lcXMLFileName);
                merchantId = xmldoc.SelectSingleNode(@"//*/row[Name='lcrpMrchID']")["Value"].InnerText.Trim();
                marketplaceId = xmldoc.SelectSingleNode(@"//*/row[Name='lcrpMkplID']")["Value"].InnerText.Trim();
                accessKeyId = xmldoc.SelectSingleNode(@"//*/row[Name='lcrpAcsKey']")["Value"].InnerText.Trim();
                secretAccessKey = xmldoc.SelectSingleNode(@"//*/row[Name='lcrpSecKey']")["Value"].InnerText.Trim();
                inboxPath = xmldoc.SelectSingleNode(@"//*/row[Name='lcrpinPth']")["Value"].InnerText.Trim();
                inboxPath = inboxPath.EndsWith("\\") ? inboxPath : inboxPath + "\\";
                InName = xmldoc.SelectSingleNode(@"//*/row[Name='lcrpInNam']")["Value"].InnerText.Trim();
                outboxPath = xmldoc.SelectSingleNode(@"//*/row[Name='lcrpoutPth']")["Value"].InnerText.Trim();
                outboxPath = outboxPath.EndsWith("\\") ? outboxPath : outboxPath + "\\";
                OutName = xmldoc.SelectSingleNode(@"//*/row[Name='lcrpOutNam']")["Value"].InnerText.Trim();
                CompanyId = xmldoc.SelectSingleNode(@"//*/row[Name='lcRpActCom']")["Value"].InnerText.Trim();
                Aria4Con = xmldoc.SelectSingleNode(@"//*/row[Name='lcRpConStr']")["Value"].InnerText.Trim();
                Aria4Con = Aria4Con.IndexOf(";") > 0 ? Aria4Con.Remove(0, Aria4Con.IndexOf(";") + 1) : "";
            }
            else
                throw new Exception("XML file " + lcXMLFileName + " Couldn't be found !!");
        }

        private void CreateService()
        {
            MarketplaceWebServiceConfig config = new MarketplaceWebServiceConfig();
            config.ServiceURL = "https://mws.amazonservices.com";
            config.SetUserAgentHeader(applicationName, applicationVersion, "C#", "<Parameter 1>", "<Parameter 2>");
            service = new MarketplaceWebServiceClient(accessKeyId, secretAccessKey, config);
            currentlog.Progress += "Service Created  .\r\n";
        }

        private string GetInFileName(string inName)
        {
            FileInfo fi = new FileInfo(inName);
            string fn = fi.Name.Replace(fi.Extension, "") + "_{0}" + fi.Extension;
            return fn;
        }

        private string GetInNextFile(string infile, int sequence)
        {
            while (File.Exists(string.Format(infile, sequence)))
            {
                sequence++;
            }
            return string.Format(infile, sequence);
        }

        private void GetReports(ReportType reporttype)
        {
            GetReportListResponse response = new GetReportListResponse();
            response = GetReportList(reporttype);
            DownloadReports(response.GetReportListResult.ReportInfo);
            currentlog.STATUS = Status.Completed.ToString();
            amazonLog.insert(currentlog);
            InitNewLogRow();
            bool hasNext = response.GetReportListResult.HasNext;
            string NextToken = response.GetReportListResult.NextToken;
            GetReportListByNextTokenResponse NextTokenResponse;
            while (hasNext)
            {
                NextTokenResponse = GetReportListByNextToken(NextToken);
                DownloadReports(NextTokenResponse.GetReportListByNextTokenResult.ReportInfo);
                currentlog.STATUS = Status.Completed.ToString();
                amazonLog.insert(currentlog);
                hasNext = NextTokenResponse.GetReportListByNextTokenResult.HasNext;
                NextToken = NextTokenResponse.GetReportListByNextTokenResult.NextToken;
            }
        }

        private void DownloadReports(List<ReportInfo> reportsInfo)
        {
            SuccessfullyReceivedReports.Id.Clear();
            string reportname = "";
            foreach (ReportInfo reportInfo in reportsInfo)
            {
                currentlog.Progress += "Start downloading report " + reportInfo.ReportType + " id:" + reportInfo.ReportId + " Acknowleged:" + reportInfo.Acknowledged.ToString() + " Avaliable:" + reportInfo.AvailableDate.ToString() + " Now :" + DateTime.Now.ToString() + " .\r\n";
                System.Threading.Thread.Sleep(60000);
                reportname = GetInNextFile(inboxPath + InName, NameSequence);
                GetReportResponse res = GetReport(reportInfo.ReportId, reportname);
                CheckMD5(reportname, reportInfo.ReportId, res);
                SuccessfullyReceivedReports.Id.Add(reportInfo.ReportId);
                currentlog.Downloaded++;
                currentlog.Progress += "Finish downloading report " + reportInfo.ReportType + " id:" + reportInfo.ReportId + " Acknowleged:" + reportInfo.Acknowledged.ToString() + " Avaliable:" + reportInfo.AvailableDate.ToString() + " Now :" + DateTime.Now.ToString() + " .\r\n";
                List<string> report = new List<string>();
                report.Add(reportInfo.ReportId);
                AcknowledgeReports(report);
            }
            currentlog.Progress += SuccessfullyReceivedReports.Id.Count.ToString() + " Reports Downloaded Successfully. .\r\n";
            // AcknowledgeReports();
            //currentlog.Progress += SuccessfullyReceivedReports.Id.Count.ToString() + " Reports Acknowledged Successfully. .\r\n";
        }

        public GetReportResponse GetReport(string ReportId, string filename)
        {
            GetReportRequest GetReportrequest = new GetReportRequest();
            GetReportrequest.Merchant = merchantId;
            GetReportrequest.Marketplace = marketplaceId;
            GetReportrequest.ReportId = ReportId;
            GetReportrequest.Report = File.Open(filename, FileMode.OpenOrCreate, FileAccess.ReadWrite);
            GetReportResponse res = null;
            try
            {
                res = service.GetReport(GetReportrequest);
            }
            catch (MarketplaceWebServiceException ex)
            {
                GetReportrequest.Report.Close();
                File.Delete(filename);
                if (ex.ErrorCode == "RequestThrottled")
                {
                    Thread.Sleep(10000);
                    res = GetReport(ReportId, filename);
                }
                else
                    throw ex;
            }
            finally
            {
                GetReportrequest.Report.Close();
            }
            return res;
        }

        private GetReportListResponse GetReportList(ReportType reporttype)
        {
            //if (LastGetReportList.AddMinutes(1).CompareTo(DateTime.Now) > 0)
            //{
            //    Thread.Sleep(LastGetReportList - DateTime.Now);
            //}

            AMAZON_TRANSMISSION_LOG_T.AMAZON_TRANSMISSION_LOG_TRow latest = amazonLog.GetLatest();

            GetReportListRequest request = new GetReportListRequest();
            request.Merchant = merchantId;
            request.Marketplace = marketplaceId;
            if (latest != null && latest.DATE != null)
                request.AvailableFromDate = latest.DATE.ToUniversalTime();
            request.ReportTypeList = new TypeList();
            request.ReportTypeList.Type = new List<string>();
            if (reporttype == ReportType.OrderReports)
                request.ReportTypeList.Type.Add("_GET_ORDERS_DATA_");
            if (reporttype == ReportType.Settlement)
                request.ReportTypeList.Type.Add("_GET_PAYMENT_SETTLEMENT_DATA_");
            request.Acknowledged = false;
            request.MaxCount = 100;
            GetReportListResponse res = service.GetReportList(request);
            DateTime last = DateTime.Now;
            currentlog.Progress += "Get Report List Completed with " + res.GetReportListResult.ReportInfo.Count.ToString() + " report. .\r\n";
            for (int x = 0; x < res.GetReportListResult.ReportInfo.Count; x++)
            {
                ReportInfo rep = res.GetReportListResult.ReportInfo[x];
                currentlog.Progress += x.ToString() + " - Report " + rep.ReportType + " with ID:" + rep.ReportId + " Avaliable from : " + rep.AvailableDate + " Acknowledged : " + rep.Acknowledged.ToString() + " on " + rep.AcknowledgedDate + " .\r\n";
            }
            currentlog.HasNext = res.GetReportListResult.HasNext;
            currentlog.NextToken = res.GetReportListResult.NextToken;
            return res;
        }

        public GetReportListByNextTokenResponse GetReportListByNextToken(string nextToken)
        {
            GetReportListByNextTokenRequest request = new GetReportListByNextTokenRequest();
            request.Merchant = merchantId;
            request.Marketplace = marketplaceId;
            request.NextToken = nextToken;
            GetReportListByNextTokenResponse res = service.GetReportListByNextToken(request);
            currentlog.Progress += "Get Report List by next token Completed with " + res.GetReportListByNextTokenResult.ReportInfo.Count.ToString() + " report. .\r\n";
            currentlog.HasNext = res.GetReportListByNextTokenResult.HasNext;
            currentlog.NextToken = res.GetReportListByNextTokenResult.NextToken;

            return res;
        }

        private void AcknowledgeAllReports(ReportType reportType)
        {
            GetReportListResponse response = new GetReportListResponse();
            response = GetReportList(reportType);
            if (response.GetReportListResult.ReportInfo.Count > 0)
            {
                List<string> reports = new List<string>();
                foreach (ReportInfo reportInfo in response.GetReportListResult.ReportInfo)
                    reports.Add(reportInfo.ReportId);
                AcknowledgeReports(reports);
                bool hasNext = response.GetReportListResult.HasNext;
                string NextToken = response.GetReportListResult.NextToken;
                GetReportListByNextTokenResponse NextTokenResponse;
                while (true)
                {
                    NextTokenResponse = GetReportListByNextToken(NextToken);
                    if (NextTokenResponse.GetReportListByNextTokenResult.ReportInfo.Count == 0)
                        break;
                    reports.Clear();
                    foreach (ReportInfo reportInfo in NextTokenResponse.GetReportListByNextTokenResult.ReportInfo)
                        reports.Add(reportInfo.ReportId);
                    AcknowledgeReports(reports);
                    hasNext = NextTokenResponse.GetReportListByNextTokenResult.HasNext;
                    NextToken = NextTokenResponse.GetReportListByNextTokenResult.NextToken;
                }
            }
        }

        private void AcknowledgeReports(List<string> ReportIdList)
        {
            string ReportIdstr = "";
            foreach (string reportId in ReportIdList)
                ReportIdstr += reportId + ",";
            ReportIdstr = ReportIdstr.EndsWith(",") ? ReportIdstr.Remove(ReportIdstr.LastIndexOf(",")) : ReportIdstr;
            currentlog.Progress += "Start Acknowledging report(s):" + ReportIdstr + " .\r\n";
            IdList ack = new IdList();
            ack.Id.AddRange(ReportIdList);
            UpdateReportAcknowledgementsRequest request = new UpdateReportAcknowledgementsRequest();
            request.Merchant = merchantId;
            request.Marketplace = marketplaceId;
            request.WithReportIdList(ack);
            UpdateReportAcknowledgementsResponse respon = service.UpdateReportAcknowledgements(request);
            string AckResult = "";
            foreach (ReportInfo reportinfo in respon.UpdateReportAcknowledgementsResult.ReportInfo)
                AckResult += reportinfo.ReportId + "(" + reportinfo.Acknowledged.ToString() + ")" + " , ";
            AckResult = AckResult.EndsWith(",") ? AckResult.Remove(AckResult.LastIndexOf(",")) : AckResult;
            currentlog.Progress += "Finish Acknowledging report(s):" + AckResult + " .\r\n";
        }

        public void AcknowledgeAllOrderReports()
        {
            AcknowledgeAllReports(ReportType.OrderReports);
        }

        private void CheckMD5(string fileName, string ReportId, GetReportResponse res)
        {

            int md5check = 0;
            while (GetMD5HashFromFile(fileName) != res.GetReportResult.ContentMD5 && md5check < 4)
            {
                res = GetReport(ReportId, fileName);
                md5check++;
            }
            if (md5check == 4)
            {
                File.Delete(fileName);
                throw new Exception("Corrupted File receveid more than 3 times");
            }
        }

        private string GetMD5HashFromFile(string fileName)
        {
            FileStream file = new FileStream(fileName, FileMode.Open);
            string md5 = MarketplaceWebServiceClient.CalculateContentMD5(file);
            file.Close();
            return md5;
        }

        public void ManageOrderSchdule(string reportType, Schedule schdulePeriod)
        {
            List<ReportSchedule> schduled = GetReportSchdule();
            bool AlreadyConfigured = false;
            foreach (ReportSchedule schduleded in schduled)
            {
                if (schduleded.ReportType == reportType && schduleded.Schedule == schdulePeriod.ToString())
                {
                    AlreadyConfigured = true;
                    currentlog.Progress += "Order schdule Already setuped to: " + schduleded.Schedule + " and will fire next time at " + schduleded.ScheduledDate.ToShortDateString() + " " + schduleded.ScheduledDate.ToShortTimeString() + " . \r\n";
                    break;
                }
            }
            if (!AlreadyConfigured)
            {
                ManageReportScheduleRequest request = new ManageReportScheduleRequest();
                request.Merchant = merchantId;
                request.Marketplace = marketplaceId;
                request.ReportType = reportType;
                request.Schedule = schdulePeriod.ToString();
                service.ManageReportSchedule(request);
                currentlog.Progress += "Manage Order schdule to: " + request.Schedule + " Compeleted .\r\n";
            }
        }

        public List<ReportSchedule> GetReportSchdule()
        {
            GetReportScheduleListRequest request = new GetReportScheduleListRequest();
            request.Merchant = merchantId;
            request.Marketplace = marketplaceId;
            GetReportScheduleListResponse response = service.GetReportScheduleList(request);
            return response.GetReportScheduleListResult.ReportSchedule;
        }

        private void UploadFeed()
        {
            InitNewLogRow();
            AddXMltoLog();
            string[] files = GetFilesToUpload();
            foreach (string file in files)
            {
                if (File.Exists(file))
                {
                    SubmitFeed(file);
                    InvokeArchive(file);
                }
            }
            currentlog.STATUS = Status.Completed.ToString();
            amazonLog.insert(currentlog);
        }

        private string[] GetFilesToUpload()
        {
            FileInfo fi = new FileInfo(OutName);
            string nameOnly = fi.Name.Replace(fi.Extension, "");
            string SearchPattern = nameOnly + "*" + fi.Extension;
            string[] files = Directory.GetFiles(outboxPath, SearchPattern, SearchOption.TopDirectoryOnly);
            currentlog.Progress += "Found " + files.Length + " file to upload. .\r\n";
            return files;
        }

        private void InvokeArchive(string file)
        {
            string ArchiveExe = outboxPath.ToUpper().Replace(@"EDI\OUTBOX\", "Archive.exe");
            string Companypath = outboxPath.ToUpper().Replace(@"EDI\OUTBOX\", "DBFS\\" + CompanyId);
            string AriaRoot = outboxPath.ToUpper().Replace(@"EDI\OUTBOX\", "");
            string History = outboxPath.ToUpper().Replace(@"EDI\OUTBOX\", "FTP_HISTORY");
            FileInfo fi = new FileInfo(file);
            Process ArchiveProcess = new Process();
            ArchiveProcess.StartInfo.FileName = ArchiveExe;
            ArchiveProcess.StartInfo.Arguments = fi.Name + " " +
                                                Companypath + " " +
                                                outboxPath.Remove(outboxPath.Length - 1) + " " +
                                                CompanyId + " " +
                                                OrignalInName + " " +
                                                History + " " +
                                                AriaRoot;
            ArchiveProcess.StartInfo.UseShellExecute = false;
            ArchiveProcess.Start();
            currentlog.Progress += "Archive for file:" + file + " Executed Successfully .\r\n";
        }

        public void SubmitFeed(string FilePath)
        {
            if (File.Exists(FilePath))
            {
                SubmitFeedRequest request = new SubmitFeedRequest();
                request.Merchant = merchantId;
                request.Marketplace = marketplaceId;
                request.FeedType = GetFeedType(FilePath).ToString();
                request.FeedContent = File.Open(FilePath, FileMode.Open, FileAccess.Read);
                request.ContentMD5 = MarketplaceWebServiceClient.CalculateContentMD5(request.FeedContent);
                request.FeedContent.Position = 0;
                SubmitFeedResponse res = service.SubmitFeed(request);
                request.FeedContent.Close();
                currentlog.Progress += FilePath + " submitted , FeedSubmissionId:" + res.SubmitFeedResult.FeedSubmissionInfo.FeedSubmissionId + ",FeedProcessingStatus:" + res.SubmitFeedResult.FeedSubmissionInfo.FeedProcessingStatus + " .\r\n";
            }
        }

        private FeedType GetFeedType(string FilePath)
        {
            XmlDocument xmldoc = new XmlDocument();
            xmldoc.Load(FilePath);
            string MessageType = xmldoc.SelectSingleNode(@"//*/MessageType").InnerText.Trim();
            if (MessageType == OrderAckFeed)
                return FeedType._POST_ORDER_ACKNOWLEDGEMENT_DATA_;
            else if (MessageType == OrderFulfillFeed)
                return FeedType._POST_ORDER_FULFILLMENT_DATA_;
            else
                throw new Exception("Unknown upload file type " + MessageType);
        }
    }
}
public enum Status
{
    Completed,
    Failed
}
public enum Schedule
{
    _15_MINUTES_,
    _30_MINUTES_,
    _1_HOUR_,
    _2_HOURS_,
    _4_HOURS_,
    _8_HOURS_,
    _12_HOURS_,
    _1_DAY_,
    _2_DAYS_,
    _72_HOURS_,
    _7_DAYS_,
    _14_DAYS_,
    _15_DAYS_,
    _30_DAYS_,
    _NEVER_
}
public enum FeedType
{
    _POST_PRODUCT_DATA_,
    _POST_PRODUCT_RELATIONSHIP_DATA_,
    _POST_ITEM_DATA_,
    _POST_PRODUCT_OVERRIDES_DATA_,
    _POST_PRODUCT_IMAGE_DATA_,
    _POST_PRODUCT_PRICING_DATA_,
    _POST_INVENTORY_AVAILABILITY_DATA_,
    _POST_ORDER_ACKNOWLEDGEMENT_DATA_,
    _POST_ORDER_FULFILLMENT_DATA_,
    _POST_FULFILLMENT_ORDER_REQUEST_DATA_,
    _POST_FULFILLMENT_ORDER_CANCELLATION_REQUEST_DATA_,
    _POST_PAYMENT_ADJUSTMENT_DATA_,
    _POST_FLAT_FILE_LISTINGS_DATA_,
    _POST_FLAT_FILE_ORDER_ACKNOWLEDGEMENT_DATA_,
    _POST_FLAT_FILE_FULFILLMENT_DATA_,
    _POST_FLAT_FILE_PAYMENT_ADJUSTMENT_DATA_,
    _POST_FLAT_FILE_INVLOADER_DATA_,
    _POST_FLAT_FILE_CONVERGENCE_LISTINGS_DATA_,
    _POST_FLAT_FILE_BOOKLOADER_DATA_,
    _POST_FLAT_FILE_PRICEANDQUANTITYONLY_UPDATE_DATA_,
    _POST_UIEE_BOOKLOADER_DATA_
}
public enum ReportType
{
    OrderReports,
    Settlement
}