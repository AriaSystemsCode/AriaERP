// Decompiled with JetBrains decompiler
// Type: UPS.UPS
// Assembly: UPS, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null
// MVID: FE2C527D-E7B1-4D14-9A59-DD5F5EB261A9
// Assembly location: D:\Aria4xp\DLLS\UPS.dll

using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Web.Services.Protocols;
using System.Xml;
using UPS.Properties;
using UPS.UPSShip;
using UPS.UPSTrack;
using UPS.UPSVoid;

namespace UPS
{
    public class UPS
    {
        private string ConnectionXSLT = "UPS_Connection.xslt";
        private string ShipXSLT = "UPS_Ship.xslt";
        private string ReturnXSLT = "UPS_Return.xslt";
        private string VoidXSLT = "UPS_Void.xslt";
        public string clientId { get; set; }
        public string clientSecret { get; set; }
        public AuthTokenHeader AuthenticationToken { set; get; }
        public ShipAcceptRequest shipAcceptRequest { get; set; }

        public ShipAcceptResponse shipAcceptResponse { get; set; }

        public ShipConfirmRequest shipConfirmRequest { get; set; }

        public ShipConfirmResponse shipConfirmResponse { get; set; }

        public ShipmentRequest shipmentRequest { get; set; }

        public ShipmentResponse shipmentResponse { get; set; }

        public ShipService shipService { get; set; }

        public TrackRequest trackRequest { get; set; }

        public TrackResponse trackResponse { get; set; }

        public TrackService trackService { get; set; }

        public VoidService voidService { get; set; }

        public VoidShipmentRequest voidShipmentRequest { get; set; }

        public VoidShipmentResponse voidShipmentResponse { get; set; }

        public string DLLLocation => Path.GetDirectoryName(this.GetType().Assembly.Location);

        public void Connect(string paramterXml)
        {
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
            Response<string> responseObj = new Response<string>();
            try
            {
                Helper.RefreshSettings((object)this, Settings.Default.Properties);
                string str1 = $"{this.DLLLocation}\\{this.ConnectionXSLT}";
                string str2 = $"{this.DLLLocation}\\Temp\\UPS_Connect_{(object)System.DateTime.Now.Ticks}.xml";
                if (System.IO.File.Exists(str1))
                {
                    Helper.XSLTransform(paramterXml, str1, str2, (Dictionary<string, object>)null);
                }
                else
                {
                    Response<string> response = responseObj;
                    response.ErrorMessage = $"{response.ErrorMessage}XSLT File Not Found :{str1} ";
                    str2 = paramterXml;
                }
                //mmt
                XmlDocument xmlDocument = new XmlDocument();
                xmlDocument.Load(paramterXml);
                XmlNode xmlNode = xmlDocument.SelectSingleNode("//ClientId");
                if (xmlNode != null && !string.IsNullOrEmpty(xmlNode.InnerText))
                {
                    XmlNode xmlNodeSec = xmlDocument.SelectSingleNode("//ClientSecret");
                    if (xmlNodeSec != null && !string.IsNullOrEmpty(xmlNodeSec.InnerText))
                    {
                        //UpsOAuthClient authClient = new UpsOAuthClient();
                        UpsAuthService authClient= new UpsAuthService(new HttpClient());
                        try
                        {
                            this.clientId = xmlNode.InnerText.Trim();
                            this.clientSecret = xmlNodeSec.InnerText.Trim();
                            var token = authClient.GetAccessTokenAsync(xmlNode.InnerText.Trim(),
                                xmlNodeSec.InnerText.Trim(), true);
                            if (!string.IsNullOrEmpty(token.Result) && token.Result.Contains("Failed to get token"))
                            {
                                responseObj.ErrorMessage += token.Result;
                            }
                            else
                            {
                                this.AuthenticationToken = new AuthTokenHeader();
                                this.AuthenticationToken.Token = token.Result;
                            }


                        }
                        catch (Exception ex)
                        {

                            responseObj.ErrorMessage += Helper.GetExecptionMessage(ex);
                        }
                    }
                }
                //mmt
                this.shipService = Helper.FillProperties(str2, "//Root", (new ShipService()).GetType()) as ShipService;
                this.shipService = Helper.FillProperties(str2, "//Root", typeof(ShipService)) as ShipService;
                this.voidService = Helper.FillProperties(str2, "//Root", (new VoidService()).GetType()) as VoidService;
                this.trackService = Helper.FillProperties(str2, "//Root", (new TrackService()).GetType()) as TrackService;
                if (this.shipService == null || this.voidService == null || this.trackService == null)
                {
                    Response<string> response = responseObj;
                    response.ErrorMessage = $"{response.ErrorMessage}Error while reading connection File {paramterXml}";
                }
                else
                {

                    this.shipService.AuthToken = this.AuthenticationToken;
                    this.voidService.AuthToken = this.AuthenticationToken;
                    this.trackService.AuthToken = this.AuthenticationToken;
                    this.shipService.ClientId = this.clientId?.Trim();
                    this.shipService.ClientSecret = this.clientSecret?.Trim();
                }
            }
            catch (Exception ex)
            {
                responseObj.ErrorMessage += Helper.GetExecptionMessage(ex);
            }
            Helper.SaveResponse((object)responseObj, paramterXml, "Response");
        }

        public void Ship(string paramterXml)
        {
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
            if (!System.IO.File.Exists(paramterXml))
                return;
            //this.clientId ,this.clientSecret 
            XmlDocument xmlDocument = new XmlDocument();
            xmlDocument.Load(paramterXml);
            XmlNode xmlNode = xmlDocument.SelectSingleNode("//Test");

            UpsAuthService authClient = new UpsAuthService(new HttpClient());
            this.AuthenticationToken.Token = null;
            var tokenShip = authClient.GetAccessTokenAsync(this.clientId, this.clientSecret, (xmlNode != null && xmlNode.InnerText.ToUpper().Trim() == "F" ?  true:false)).Result;
            this.AuthenticationToken.Token = tokenShip;
            Helper.TestCheck(paramterXml, (SoapHttpClientProtocol)this.shipService, Settings.Default.UPS_UPSShip_ShipService_Test, Settings.Default.UPS_UPSShip_ShipService);
            //XmlDocument xmlDocument = new XmlDocument();
            //xmlDocument.Load(paramterXml);

            int count = xmlDocument.SelectNodes("//ROW").Count;
            xmlNode = xmlDocument.SelectSingleNode("//Return_Flag");
            string str = xmlNode != null ? Convert.ToString(xmlNode.InnerText).ToUpper().Trim() : "";
            if (str == "S" || str == "B")
                this.ProcessShip(paramterXml, false, 1);
            if (str == "R" || str == "B")
            {
                for (int index = 0; index < count; ++index)
                    this.ProcessShip(paramterXml, true, index + 1);
            }
        }

        public static void TestCheck(
          string paramterXml,
          SoapHttpClientProtocol service,
          string TestService,
          string LiveService)
        {
            XmlDocument xmlDocument = new XmlDocument();
            xmlDocument.Load(paramterXml);
            XmlNode xmlNode = xmlDocument.SelectSingleNode("//Test");
            if (xmlNode != null && xmlNode.InnerText.ToUpper().Trim() == "T")
                service.Url = TestService;
            else
                service.Url = LiveService;
        }
        //[SoapHeader("AuthToken")]
        private async Task ProcessShip(string paramterXml, bool IsReturnShipment, int RowIndex)
        {
            string str1 = $"{this.DLLLocation}\\{(IsReturnShipment ? this.ReturnXSLT : this.ShipXSLT)}";
            string str2 = $"{this.DLLLocation}\\Temp\\UPS_Ship_{(object)System.DateTime.Now.Ticks}.xml";
            Response<ShipmentResponse> responseObj = new Response<ShipmentResponse>();
            try
            {
                if (System.IO.File.Exists(str1))
                {
                    Dictionary<string, object> XSLTParamters = new Dictionary<string, object>();
                    if (IsReturnShipment)
                        XSLTParamters.Add(nameof(RowIndex), (object)RowIndex);
                    Helper.XSLTransform(paramterXml, str1, str2, XSLTParamters);
                }
                else
                {
                    Response<ShipmentResponse> response = responseObj;
                    response.ErrorMessage = $"{response.ErrorMessage}XSLT File Not Found :{str1} ";
                    str2 = paramterXml;
                }
               
                this.shipmentRequest = Helper.FillProperties(str2, "//Root", new ShipmentRequest().GetType()) as ShipmentRequest;
                //MMt
                string accessToken =this.AuthenticationToken.Token;

                HttpClient client = new HttpClient();

                // UPS authentication token
                client.DefaultRequestHeaders.Authorization =
                    new AuthenticationHeaderValue("Bearer", accessToken);

                // Required UPS headers
                client.DefaultRequestHeaders.Add("transId", Guid.NewGuid().ToString());
                client.DefaultRequestHeaders.Add("transactionSrc", "MyApp");
                //MMt
                string json = JsonConvert.SerializeObject(this.shipmentRequest,
                 Newtonsoft.Json.Formatting.None);
                // string json = JsonSerializer.Serialize(this.shipmentRequest, Newtonsoft.Json.Formatting.Indented);
                json = "{\"ShipmentRequest\":" + json + "}";
                             var content = new StringContent(json, Encoding.UTF8, "application/json");
                
                XmlDocument xmlDocument = new XmlDocument();
                xmlDocument.Load(paramterXml);
                XmlNode xmlNode = xmlDocument.SelectSingleNode("//Test");
                HttpResponseMessage responseR = null;
                if (xmlNode.InnerText.ToUpper().Trim() == "F")
                {
                    responseR = await client.PostAsync(
                                "https://onlinetools.ups.com/api/shipments/v2403/ship",
                    content
                    );

                }
                else
                {
                     responseR = await client.PostAsync(
                        "https://wwwcie.ups.com/api/shipments/v2403/ship",
                        content
                    );
                }
                string resultRes = await responseR.Content.ReadAsStringAsync();

                //this.shipmentResponse =  this.shipService.ProcessShipment(this.shipmentRequest);

                //this.shipmentResponse = responseR.;
                //var shipmentResponse =
                //   JsonConvert.DeserializeObject<ShipmentResponse>(resultRes);
                if (resultRes.ToUpper().Contains("ERRORS"))
                {
                    JObject doc = JObject.Parse(resultRes);
                    foreach (var error in doc["response"]["errors"])
                    {
                        int result = 0;
                        Response<ShipmentResponse> response = responseObj;
                        response.ErrorMessage = response.ErrorMessage + error["message"].ToString().TrimEnd() + Environment.NewLine;
                        int.TryParse(error["code"].ToString(), out result);
                        responseObj.ErrorNumber = result;
                    }
                    //Helper.SaveError(paramterXml, responseObj.ErrorMessage, RowIndex);
                    this.SaveError(paramterXml, responseObj.ErrorMessage, RowIndex);
                    Helper.SaveResponse((object)responseObj, str2, "Response");
                    Thread.Sleep(5000);
                    return;
                }
                else
                {
                    JObject doc = JObject.Parse(resultRes);
                    ShipmentResponse responseObject = new ShipmentResponse();
                    responseObject.ShipmentResults = new ShipmentResultsType();
                    //responseObject.ShipmentResults.ShipmentCharges = new ShipmentChargesType();
                    responseObject.ShipmentResults = JsonConvert.DeserializeObject<ShipmentResultsType>(doc["ShipmentResponse"]["ShipmentResults"].ToString());
                    responseObject.Response = new UPSShip.ResponseType();
                    //    .ShipmentCharges.TotalCharges = doc["response"]["errors"];
                    responseObject.Response = JsonConvert.DeserializeObject<UPSShip.ResponseType>(doc["ShipmentResponse"]["Response"].ToString());
                    this.shipmentResponse = responseObject;
                    
                    //if (this.shipmentResponse.Response.Alert != null && this.shipmentResponse.Response.Alert.Length > 0)
                    //{
                    //    foreach (UPSShip.CodeDescriptionType codeDescriptionType in this.shipmentResponse.Response.Alert)
                    //    {
                    //        int result = 0;
                    //        Response<ShipmentResponse> response = responseObj;
                    //        response.ErrorMessage = response.ErrorMessage + codeDescriptionType.Description + Environment.NewLine;
                    //        int.TryParse(this.shipmentResponse.Response.Alert[0].Code, out result);
                    //        responseObj.ErrorNumber = result;
                    //    }
                    //}
                    string pathFromXml = this.GetPathFromXml(paramterXml);
                    if (this.shipmentResponse.ShipmentResults != null && this.shipmentResponse.ShipmentResults.PackageResults != null && !string.IsNullOrEmpty(pathFromXml))
                    {
                        foreach (PackageResultsType packageResult in this.shipmentResponse.ShipmentResults.PackageResults)
                        {
                            using (Image image = Helper.Base64ToImage(packageResult.ShippingLabel.GraphicImage))
                            {
                                image.RotateFlip(RotateFlipType.Rotate90FlipNone);
                                image.Save($"{pathFromXml}{packageResult.TrackingNumber}.jpg", ImageFormat.Jpeg);
                                image.Dispose();
                            }
                        }
                    }
                    if (IsReturnShipment)
                        this.SaveAriaReturnInfo(paramterXml, this.shipmentResponse, RowIndex);
                    else
                        this.SaveAriaShipmentInfo(paramterXml, this.shipmentResponse);
                }
            }
            catch (Exception ex)
            {
                responseObj.ErrorMessage += Helper.GetExecptionMessage(ex);
            }
            Helper.SaveError(paramterXml, responseObj.ErrorMessage, RowIndex);
            Helper.SaveResponse((object)responseObj, str2, "Response");
            //    //responseObj.WSResponse = this.shipmentResponse;
            //    //if (this.shipmentResponse.Response.Alert != null &&
            //    //  this.shipmentResponse.Response.Alert.Length > 0)
            //    if (!string.IsNullOrEmpty(resultRes) && resultRes.ToUpper().Contains("ERRORS"))
            //    {
            //        foreach (UPSShip.CodeDescriptionType codeDescriptionType in this.shipmentResponse.Response.Alert)
            //        {
            //            int result = 0;
            //            Response<ShipmentResponse> response = responseObj;
            //            response.ErrorMessage = response.ErrorMessage + codeDescriptionType.Description + Environment.NewLine;
            //            int.TryParse(this.shipmentResponse.Response.Alert[0].Code, out result);
            //            responseObj.ErrorNumber = result;
            //            /*var indexError = resultRes.ToUpper().IndexOf("ERRORS");
            //            if (indexError>0)
            //            responseObj.ErrorMessage = resultRes.Substring(indexError);*/
            //        }
            //    }
            //    string pathFromXml = this.GetPathFromXml(paramterXml);
            //    if (this.shipmentResponse.ShipmentResults != null && this.shipmentResponse.ShipmentResults.PackageResults != null && !string.IsNullOrEmpty(pathFromXml))
            //    {
            //        foreach (PackageResultsType packageResult in this.shipmentResponse.ShipmentResults.PackageResults)
            //        {
            //            using (Image image = Helper.Base64ToImage(packageResult.ShippingLabel.GraphicImage))
            //            {
            //                image.RotateFlip(RotateFlipType.Rotate90FlipNone);
            //                image.Save($"{pathFromXml}{packageResult.TrackingNumber}.jpg", ImageFormat.Jpeg);
            //                image.Dispose();
            //            }
            //        }
            //    }
            //    if (IsReturnShipment)
            //        this.SaveAriaReturnInfo(paramterXml, this.shipmentResponse, RowIndex);
            //    else
            //        this.SaveAriaShipmentInfo(paramterXml, this.shipmentResponse);
            //}
            //catch (Exception ex)
            //{
            //    responseObj.ErrorMessage += Helper.GetExecptionMessage(ex);
            //}
            //Helper.SaveError(paramterXml, responseObj.ErrorMessage, RowIndex);
            //Helper.SaveResponse((object)responseObj, str2, "Response");
        }
        public void SaveError(string XMLPath, string Error, int errorRowIndex)
        {
            string finalFile = XMLPath;
            XMLPath = XMLPath+".tmp";
            System.IO.File.Copy(finalFile, XMLPath, true);
            System.IO.File.Delete(finalFile);
            XmlDocument xmlDocument = new XmlDocument();
            xmlDocument.Load(XMLPath);
            using (FileStream fs = new FileStream(
    XMLPath,
    FileMode.Create,
    FileAccess.Write,
    FileShare.Read))
            {
                XmlNode xmlNode = xmlDocument.SelectSingleNode("//ROW[position() = " + errorRowIndex + "]/ERRORMESSAGE");
                if (xmlNode != null)
                {
                    xmlNode.InnerText = Error;
                }
                xmlDocument.Save(fs);
                fs.Flush();       // flush stream buffers
                fs.Flush(true);   // force write to disk
            }
            //File.Replace(XMLPath, finalFile, null);
            System.IO.File.Copy(XMLPath, finalFile, true);
            System.IO.File.Delete(XMLPath);
            //            XmlNode xmlNode = xmlDocument.SelectSingleNode("//ROW[position() = " + errorRowIndex + "]/ERRORMESSAGE");
            //          if (xmlNode != null)
            //{
            //    xmlNode.InnerText = Error;
            //}

            //lock (xmlDocument)
            //{
            //    xmlDocument.Save(XMLPath);
            //}

        }
        public void Rate(string paramterXml)
        {
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
            Response<ShipConfirmResponse> responseObj = new Response<ShipConfirmResponse>();
            try
            {
                Helper.TestCheck(paramterXml, (SoapHttpClientProtocol)this.shipService, Settings.Default.UPS_UPSShip_ShipService_Test, Settings.Default.UPS_UPSShip_ShipService);
                string str1 = $"{this.DLLLocation}\\{this.ShipXSLT}";
                string str2 = $"{this.DLLLocation}\\Temp\\UPS_Rate_{(object)System.DateTime.Now.Ticks}.xml";
                if (System.IO.File.Exists(str1))
                {
                    Helper.XSLTransform(paramterXml, str1, str2, (Dictionary<string, object>)null);
                }
                else
                {
                    Response<ShipConfirmResponse> response = responseObj;
                    response.ErrorMessage = $"{response.ErrorMessage}XSLT File Not Found :{str1} ";
                    str2 = paramterXml;
                }
                this.shipConfirmRequest = Helper.FillProperties(str2, "//Root", new ShipConfirmRequest().GetType()) as ShipConfirmRequest;
                this.shipConfirmResponse = this.shipService.ProcessShipConfirm(this.shipConfirmRequest);
                responseObj.WSResponse = this.shipConfirmResponse;
                if (this.shipConfirmResponse.Response.Alert != null && this.shipConfirmResponse.Response.Alert.Length > 0)
                {
                    foreach (UPSShip.CodeDescriptionType codeDescriptionType in this.shipConfirmResponse.Response.Alert)
                    {
                        int result = 0;
                        Response<ShipConfirmResponse> response = responseObj;
                        response.ErrorMessage = response.ErrorMessage + codeDescriptionType.Description + Environment.NewLine;
                        int.TryParse(this.shipConfirmResponse.Response.Alert[0].Code, out result);
                        responseObj.ErrorNumber = result;
                    }
                }
            }
            catch (Exception ex)
            {
                responseObj.ErrorMessage += Helper.GetExecptionMessage(ex);
            }
            Helper.SaveResponse((object)responseObj, paramterXml, "Response");
        }

        public void RateAccept(string paramterXml)
        {
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
            Response<ShipAcceptResponse> responseObj = new Response<ShipAcceptResponse>();
            try
            {
                this.shipAcceptRequest = Helper.FillProperties(paramterXml, "//Root", new ShipAcceptRequest().GetType()) as ShipAcceptRequest;
                if (this.shipAcceptRequest != null && !string.IsNullOrEmpty(this.shipAcceptRequest.ShipmentDigest))
                    this.shipAcceptRequest.ShipmentDigest = this.shipAcceptRequest.ShipmentDigest.Trim();
                this.shipAcceptResponse = this.shipService.ProcessShipAccept(this.shipAcceptRequest);
                responseObj.WSResponse = this.shipAcceptResponse;
                if (this.shipAcceptResponse.Response.Alert != null && this.shipAcceptResponse.Response.Alert.Length > 0)
                {
                    foreach (UPSShip.CodeDescriptionType codeDescriptionType in this.shipAcceptResponse.Response.Alert)
                    {
                        int result = 0;
                        Response<ShipAcceptResponse> response = responseObj;
                        response.ErrorMessage = response.ErrorMessage + codeDescriptionType.Description + Environment.NewLine;
                        int.TryParse(this.shipAcceptResponse.Response.Alert[0].Code, out result);
                        responseObj.ErrorNumber = result;
                    }
                }
                string pathFromXml = this.GetPathFromXml(paramterXml);
                if (this.shipAcceptResponse.ShipmentResults != null && this.shipAcceptResponse.ShipmentResults.PackageResults != null && !string.IsNullOrEmpty(pathFromXml))
                {
                    foreach (PackageResultsType packageResult in this.shipAcceptResponse.ShipmentResults.PackageResults)
                    {
                        using (Image image = Helper.Base64ToImage(packageResult.ShippingLabel.GraphicImage))
                        {
                            image.RotateFlip(RotateFlipType.Rotate90FlipNone);
                            image.Save($"{pathFromXml}{packageResult.TrackingNumber}.jpg", ImageFormat.Jpeg);
                            image.Dispose();
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                responseObj.ErrorMessage += Helper.GetExecptionMessage(ex);
            }
            Helper.SaveResponse((object)responseObj, paramterXml, "Response");
        }

        public void SaveAriaShipmentInfo(string paramterXml, ShipmentResponse replay)
        {
            XmlDocument xmlDocument = new XmlDocument();
            xmlDocument.Load(paramterXml);
            XmlNodeList xmlNodeList1 = xmlDocument.SelectNodes("//NFREIGHT");
            xmlDocument.SelectNodes("//CDECL_VAL");
            XmlNodeList xmlNodeList2 = xmlDocument.SelectNodes("//CCOD");
            xmlDocument.SelectNodes("//CCOD_AMT");
            XmlNodeList xmlNodeList3 = xmlDocument.SelectNodes("//TRACKING_NO");
            XmlNodeList xmlNodeList4 = xmlDocument.SelectNodes("//CARRIER_SHIPMENT_ID");
            xmlDocument.SelectNodes("//CARRIER_SHIPMENT_DIGEST");
            XmlNodeList xmlNodeList5 = xmlDocument.SelectNodes("//BILLING_WEIGHT");
            if (xmlNodeList2.Count > 0)
                xmlNodeList2[0].InnerText = "0";
            foreach (XmlNode xmlNode in xmlNodeList4)
                xmlNode.InnerText = replay.ShipmentResults.ShipmentIdentificationNumber;
            foreach (XmlNode xmlNode in xmlNodeList1)
                xmlNode.InnerText = replay.ShipmentResults.ShipmentCharges.TotalCharges.MonetaryValue;
            if (xmlNodeList5.Count > 0)
                xmlNodeList5[0].InnerText = replay.ShipmentResults.BillingWeight.Weight;
            for (int i = 0; i < replay.ShipmentResults.PackageResults.Length; ++i)
            {
                if (xmlNodeList3[i] != null)
                    xmlNodeList3[i].InnerText = replay.ShipmentResults.PackageResults[i].TrackingNumber;
            }
            lock (this)
                xmlDocument.Save(paramterXml);
        }

        public void SaveAriaReturnInfo(string paramterXml, ShipmentResponse replay, int RowIndex)
        {
            XmlDocument xmlDocument = new XmlDocument();
            xmlDocument.Load(paramterXml);
            XmlNode xmlNode = xmlDocument.SelectSingleNode($"//ROW[position()={(object)RowIndex}]/RETURN_TRACKING_NO");
            if (replay != null && replay.ShipmentResults != null && replay.ShipmentResults.PackageResults != null && replay.ShipmentResults.PackageResults.Length > 0)
                xmlNode.InnerText = replay.ShipmentResults.PackageResults[0].TrackingNumber;
            lock (this)
                xmlDocument.Save(paramterXml);
        }

        public void Track(string paramterXml)
        {
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
            Response<TrackResponse> responseObj = new Response<TrackResponse>();
            try
            {
                Helper.TestCheck(paramterXml, (SoapHttpClientProtocol)this.trackService, Settings.Default.UPS_UPSTrack_TrackService_Test, Settings.Default.UPS_UPSTrack_TrackService);
                this.trackRequest = Helper.FillProperties(paramterXml, "//Root", new TrackRequest().GetType()) as TrackRequest;
                this.trackResponse = this.trackService.ProcessTrack(this.trackRequest);
                responseObj.WSResponse = this.trackResponse;
                if (this.trackResponse.Response.Alert != null && this.trackResponse.Response.Alert.Length > 0)
                {
                    foreach (UPSTrack.CodeDescriptionType codeDescriptionType in this.trackResponse.Response.Alert)
                    {
                        int result = 0;
                        responseObj.ErrorMessage = responseObj.ErrorMessage + codeDescriptionType.Description + Environment.NewLine;
                        int.TryParse(this.trackResponse.Response.Alert[0].Code, out result);
                        responseObj.ErrorNumber = result;
                    }
                }
            }
            catch (Exception ex)
            {
                responseObj.ErrorMessage += Helper.GetExecptionMessage(ex);
            }
            Helper.SaveResponse((object)responseObj, paramterXml, "Response");
        }

        public async Task Void(string paramterXml)
        {
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
            //MMT-2026[Start]
            UpsAuthService authClient = new UpsAuthService(new HttpClient());

            this.AuthenticationToken.Token = null;

            XmlDocument xmlDocument = new XmlDocument();
            xmlDocument.Load(paramterXml);
            string trackingNumber = "";
            //int count = xmlDocument.SelectNodes("//ROW").Count;
            XmlNode xmlNode = xmlDocument.SelectSingleNode("//TRACKING_NO");
            if (xmlNode != null && !string.IsNullOrEmpty(xmlNode.InnerText))
            {
                trackingNumber = xmlNode.InnerText.Trim();
            }
            var client = new HttpClient();
            xmlNode = xmlDocument.SelectSingleNode("//Test");
            var tokenVoid =
                authClient.GetAccessTokenAsync(this.clientId,
                this.clientSecret, ((xmlNode.InnerText.ToUpper().Trim() == "F")? true: false)).Result;

            this.AuthenticationToken.Token = tokenVoid;

            client.DefaultRequestHeaders.Authorization =
                new AuthenticationHeaderValue("Bearer", tokenVoid);

            client.DefaultRequestHeaders.Add("transId", Guid.NewGuid().ToString());
            client.DefaultRequestHeaders.Add("transactionSrc", "MyApp");

            string testingUrl =
               $"https://wwwcie.ups.com/api/shipments/{trackingNumber}/void/cancel/{trackingNumber}";
            string productionUrl =
                $"https://onlinetools.ups.com/api/shipments/{trackingNumber}/void/cancel/{trackingNumber}";
            /*$"https://onlinetools.ups.com/api/shipments/{trackingNumber}/void/cancel";*/
            /*$"https://onlinetools.ups.com/api/shipments/{trackingNumber}/void";*/


            //HttpResponseMessage responseR = null;
            string urlToUse = testingUrl;
            if (xmlNode.InnerText.ToUpper().Trim() == "F")
                urlToUse = productionUrl;

                var response =await client.DeleteAsync(urlToUse);
            //var response = await client.PostAsync(urlToUse, new StringContent("{}", Encoding.UTF8, "application/json"));
            //$"https://onlinetools.ups.com/api/shipments/{shipmentId}/void/cancel",

            Response<VoidShipmentResponse> response1 = new Response<VoidShipmentResponse>();
            var content = await response.Content.ReadAsStringAsync();
            //JObject doc = JObject.Parse(content);
            if (response.IsSuccessStatusCode == false)
            {

                //foreach (var error in doc["response"]["errors"])
                {
                    response1.ErrorMessage = response1.ErrorMessage + response.ReasonPhrase.ToString().TrimEnd() + Environment.NewLine;
                    //response1.ErrorNumber = int.Parse(response.Result.StatusCode.ToString());
                    response1.WSResponse = new VoidShipmentResponse();
                    response1.WSResponse.Response = new UPSVoid.ResponseType();
                    response1.WSResponse.Response.ResponseStatus = new UPSVoid.CodeDescriptionType();
                    response1.WSResponse.Response.ResponseStatus.Code = "1";
                    response1.WSResponse.Response.ResponseStatus.Description = response.ReasonPhrase;
                }
            }
            else
            {
                response1.WSResponse = new VoidShipmentResponse();
                response1.WSResponse.Response = new UPSVoid.ResponseType();
                response1.WSResponse.Response.ResponseStatus = new UPSVoid.CodeDescriptionType();
                response1.WSResponse.Response.ResponseStatus.Code = "1";
              //if (!string.IsNullOrEmpty(content))    
              //  response1.WSResponse = JsonConvert.DeserializeObject<VoidShipmentResponse>(doc["VoidShipmentResponse"].ToString());
            }
            this.SaveAriaVoidInfo(paramterXml, response1);
            Helper.SaveResponse((object)response1, paramterXml, "Response");
            Thread.Sleep(5000);
            return;
            //MMT-2026[End]

            //Response<VoidShipmentResponse> response1 = new Response<VoidShipmentResponse>();
            //try
            //{
            //    Helper.TestCheck(paramterXml, (SoapHttpClientProtocol)this.voidService, Settings.Default.UPS_UPSVoid_VoidService_Test, Settings.Default.UPS_UPSVoid_VoidService);
            //    string str1 = $"{this.DLLLocation}\\{this.VoidXSLT}";
            //    string str2 = $"{this.DLLLocation}\\Temp\\UPS{(object)System.DateTime.Now.Ticks}.xml";
            //    if (System.IO.File.Exists(str1))
            //    {
            //        Helper.XSLTransform(paramterXml, str1, str2, (Dictionary<string, object>)null);
            //    }
            //    else
            //    {
            //        Response<VoidShipmentResponse> response2 = response1;
            //        response2.ErrorMessage = $"{response2.ErrorMessage}XSLT File Not Found :{str1} ";
            //        str2 = paramterXml;
            //    }
            //    this.voidShipmentRequest = Helper.FillProperties(str2, "//Root", new VoidShipmentRequest().GetType()) as VoidShipmentRequest;
            //    this.voidShipmentResponse = this.voidService.ProcessVoid(this.voidShipmentRequest);
            //    response1.WSResponse = this.voidShipmentResponse;
            //    if (this.voidShipmentResponse.Response.Alert != null && this.voidShipmentResponse.Response.Alert.Length > 0)
            //    {
            //        foreach (UPSVoid.CodeDescriptionType codeDescriptionType in this.voidShipmentResponse.Response.Alert)
            //        {
            //            int result = 0;
            //            Response<VoidShipmentResponse> response3 = response1;
            //            response3.ErrorMessage = response3.ErrorMessage + codeDescriptionType.Description + Environment.NewLine;
            //            int.TryParse(this.voidShipmentResponse.Response.Alert[0].Code, out result);
            //            response1.ErrorNumber = result;
            //        }
            //    }
            //}
            //catch (Exception ex)
            //{
            //    response1.ErrorMessage += Helper.GetExecptionMessage(ex);
            //}
            //this.SaveAriaVoidInfo(paramterXml, response1);
            //Helper.SaveResponse((object)response1, paramterXml, "Response");
        }

        public void SaveAriaVoidInfo(string paramterXml, Response<VoidShipmentResponse> replay)
        {
            XmlDocument xmlDocument = new XmlDocument();
            xmlDocument.Load(paramterXml);
            XmlNodeList xmlNodeList1 = xmlDocument.SelectNodes("//STATUS");
            XmlNodeList xmlNodeList2 = xmlDocument.SelectNodes("//DESCRIPTION");
            if (xmlNodeList1.Count <= 0 || xmlNodeList2.Count <= 0)
                return;
            if (replay.WSResponse != null)
            {
                xmlNodeList1[0].InnerText = !(replay.WSResponse.Response.ResponseStatus.Code == "1") ? replay.WSResponse.Response.ResponseStatus.Code.ToString() : "V";
                if (!string.IsNullOrEmpty(replay.WSResponse.Response.ResponseStatus.Description))
                    xmlNodeList2[0].InnerText = replay.WSResponse.Response.ResponseStatus.Description;
            }
            else
            {
                xmlNodeList2[0].InnerText = replay.ErrorMessage;
                xmlNodeList1[0].InnerText = replay.ErrorNumber.ToString();
            }
            lock (this)
                xmlDocument.Save(paramterXml);
        }

        private string GetPathFromXml(string paramterXml)
        {
            XmlDocument xmlDocument = new XmlDocument();
            xmlDocument.Load(paramterXml);
            string str = (xmlDocument.SelectSingleNode("//CLABELPATH") ?? throw new Exception("OutPut Path is empty in the XML File " + paramterXml)).InnerText.Trim();
            string path = str.EndsWith("\\") ? str : str + "\\";
            return Directory.Exists(path) ? path : throw new Exception("Invalid OutPut folder " + path);
        }
    }
    //oAuth[Start]
    public class UpsOAuthClient
    {
        private static readonly HttpClient client = new HttpClient();

        public async Task<string> GetAccessToken(string clientId, string clientSecret, bool test)
        {
            // 1. Prepare the authorization header (Basic Base64(ID:Secret))
            var authString = Convert.ToBase64String(Encoding.UTF8.GetBytes($"{clientId}:{clientSecret}"));

            //var request = new HttpRequestMessage(HttpMethod.Post, "https://ups.com");
            HttpRequestMessage request;
            if (test == false)
            {
                request = new HttpRequestMessage(HttpMethod.Post, "https://onlinetools.ups.com/security/v1/oauth/token");
                request.Headers.Authorization = new AuthenticationHeaderValue("Basic", authString);
            }
            else
            {
                request = new HttpRequestMessage(HttpMethod.Post, "https://wwwcie.ups.com/security/v1/oauth/token");
                request.Headers.Authorization = new AuthenticationHeaderValue("Basic", authString);
            }
            //
            //var request = new HttpRequestMessage(HttpMethod.Post, "https://wwwcie.ups.com/security/v1/oauth/token");



            // 2. Set the grant type in the request body
            var keyValues = new List<KeyValuePair<string, string>>
        {
            new KeyValuePair<string, string>("grant_type", "client_credentials")
        };
            request.Content = new FormUrlEncodedContent(keyValues);

            // 3. Execute the request
            var response = await client.SendAsync(request);
            var content = await response.Content.ReadAsStringAsync();

            if (response.IsSuccessStatusCode)
            {

                //using (var doc = JsonConvert.DeserializeObject(content))
                //var doc = JsonConvert.SerializeObject(content);
                JObject doc = JObject.Parse(content);
                string token = (string)doc["access_token"];
                //JsonElement root = doc.RootElement;
                return token;//doc.GetType().GetProperty("access_token").GetValue(doc).ToString();
            }
            var index = content.IndexOf("message");
            if (index != -1)
            {
                var msg = content.Substring(index);
                var splt = msg.Split(':');
                return $"Failed to get token: {splt[1].Replace('}', ' ').Replace(']', ' ').Replace('"', ' ')}";
            }

            //var error = JsonConvert.SerializeObject(content);
            //var msgn = error.GetType().GetProperty("message").GetValue(error).ToString();
            return $"Failed to get token";
        }
    }
    //oAuth[End]
    //MMT2026[Start]
    public class AuthTokenHeader : SoapHeader
    {
        public string Token;
    }
    public class UpsAuthService
    {
        private readonly HttpClient _httpClient;

        public UpsAuthService(HttpClient httpClient)
        {
            _httpClient = httpClient;
        }

        public async Task<string> GetAccessTokenAsync(
            string clientId,
            string clientSecret,
            bool isProduction)
        {
            var tokenUrl = isProduction
                ? "https://onlinetools.ups.com/security/v1/oauth/token"
                : "https://wwwcie.ups.com/security/v1/oauth/token";

            // 🔐 Basic Auth (clientId:clientSecret)
            var credentials = Convert.ToBase64String(
                Encoding.UTF8.GetBytes($"{clientId}:{clientSecret}")
            );

            var request = new HttpRequestMessage(HttpMethod.Post, tokenUrl);

            request.Headers.Authorization = new AuthenticationHeaderValue("Basic", credentials);

            request.Content = new StringContent(
                "grant_type=client_credentials",
                Encoding.UTF8,
                "application/x-www-form-urlencoded"
            );

            var response = await _httpClient.SendAsync(request);
            var content = await response.Content.ReadAsStringAsync();

            if (!response.IsSuccessStatusCode)
            {
                throw new Exception($"UPS Auth Failed: {response.StatusCode} - {content}");
            }

            //using var json = JsonDocument.Parse(content);
            //var token = json.RootElement.GetProperty("access_token").GetString();
            JObject doc = JObject.Parse(content);
            string token = (string)doc["access_token"];
            
            return token;
        }
    }
    //MMT2026[End]
}
