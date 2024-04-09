using Endicia.Properties;
using Endicia.WS;
using System;
using System.Collections.Generic;
using System.Configuration;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Xml;
using System.Net;

namespace Endicia
{
	public class Endicia
	{
		private string ConnectionXSLT = "Endicia_Connection.xslt";

		private string ShipXSLT = "Endicia_Ship.xslt";

		private string ChangePasswordXSLT = "Endicia_ChangePassword.xslt";

		private string RecreditAmountXSLT = "Endicia_RecreditAmount.xslt";

		private AccountStatusRequest accountStatusRequest
		{
			get;
			set;
		}

		private AccountStatusResponse accountStatusResponse
		{
			get;
			set;
		}

		private ChangePassPhraseRequest changePassPhraseRequest
		{
			get;
			set;
		}

		private ChangePassPhraseRequestResponse changePassPhraseRequestResponse
		{
			get;
			set;
		}

		private Credentials credentials
		{
			get;
			set;
		}

		public string DLLLocation
		{
			get
			{
				return Path.GetDirectoryName(this.GetType().Assembly.Location);
			}
		}

		private EwsLabelService ewsLabelService
		{
			get;
			set;
		}

		private LabelRequest labelRequest
		{
			get;
			set;
		}

		private LabelRequestResponse labelRequestResponse
		{
			get;
			set;
		}

		private RecreditRequest recreditRequest
		{
			get;
			set;
		}

		private RecreditRequestResponse recreditRequestResponse
		{
			get;
			set;
		}

		public Endicia()
		{
            //MMT
            //ServicePointManager.Expect100Continue = true;
            ServicePointManager.SecurityProtocol = (SecurityProtocolType)3072;
            //MMT 
        }

		public void AccountStatus(string paramterXml)
		{
			Response<AccountStatusResponse> Response = new Response<AccountStatusResponse>();
			try
			{
				this.accountStatusRequest = new AccountStatusRequest()
				{
					CertifiedIntermediary = new CertifiedIntermediary()
					{
						AccountID = this.credentials.AccountID,
						PassPhrase = this.credentials.PassPhrase
					},
					RequesterID = this.credentials.RequesterID,
					RequestID = Guid.NewGuid().ToString()
				};
				this.accountStatusResponse = this.ewsLabelService.GetAccountStatus(this.accountStatusRequest);
				Response<AccountStatusResponse> response = Response;
				response.ErrorMessage = string.Concat(response.ErrorMessage, this.accountStatusResponse.ErrorMessage);
				Response.ErrorNumber = this.accountStatusResponse.Status;
				Response.WSResponse = this.accountStatusResponse;
				if (this.accountStatusResponse.Status == 0)
				{
					decimal postageBalance = this.accountStatusResponse.CertifiedIntermediary.PostageBalance;
					this.SaveResult(paramterXml, postageBalance.ToString(), "PostageBalance");
				}
				else
				{
					this.SaveResult(paramterXml, this.accountStatusResponse.ErrorMessage, "RESULT");
				}
			}
			catch (Exception exception)
			{
				Exception ex = exception;
				Response<AccountStatusResponse> response1 = Response;
				response1.ErrorMessage = string.Concat(response1.ErrorMessage, Helper.GetExecptionMessage(ex));
			}
			Helper.SaveResponse(Response, paramterXml, "Response");
		}

		public void ChangePassword(string paramterXml)
		{
			object[] dLLLocation = new object[] { this.DLLLocation, "\\Temp\\Endicia_ChangePassword_", null, null };
			dLLLocation[2] = DateTime.Now.Ticks;
			dLLLocation[3] = ".xml";
			string OutFile = string.Concat(dLLLocation);
			Response<ChangePassPhraseRequestResponse> Response = new Response<ChangePassPhraseRequestResponse>();
			try
			{
				string xsltfile = string.Concat(this.DLLLocation, "\\", this.ChangePasswordXSLT);
				if (!File.Exists(xsltfile))
				{
					Response<ChangePassPhraseRequestResponse> response = Response;
					response.ErrorMessage = string.Concat(response.ErrorMessage, "XSLT File Not Found :", xsltfile, " ");
					OutFile = paramterXml;
				}
				else
				{
					Helper.XSLTransform(paramterXml, xsltfile, OutFile, null);
				}

                //XXX
                ChallengeRequest challengeRequest = new ChallengeRequest();
                challengeRequest.AccountID = this.credentials.AccountID;
                challengeRequest.RequestID = Guid.NewGuid().ToString();
                challengeRequest.RequesterID = this.credentials.RequesterID;
                challengeRequest.Email = "hesham.e@ariasystems.biz";
                ChallengeQuestionResponse resp = this.ewsLabelService.ChallengeQuestion(challengeRequest);
                //xxxx

                this.changePassPhraseRequest = Helper.FillProperties(OutFile, "//Root", (new ChangePassPhraseRequest()).GetType()) as ChangePassPhraseRequest;
				this.changePassPhraseRequest.CertifiedIntermediary = new CertifiedIntermediary();
				this.changePassPhraseRequest.RequesterID = this.credentials.RequesterID;
				this.changePassPhraseRequest.RequestID = Guid.NewGuid().ToString();
				this.changePassPhraseRequest.CertifiedIntermediary.AccountID = this.credentials.AccountID;
				this.changePassPhraseRequest.CertifiedIntermediary.PassPhrase = this.credentials.PassPhrase;
				this.changePassPhraseRequestResponse = this.ewsLabelService.ChangePassPhrase(this.changePassPhraseRequest);
				Response<ChangePassPhraseRequestResponse> response1 = Response;
				response1.ErrorMessage = string.Concat(response1.ErrorMessage, this.changePassPhraseRequestResponse.ErrorMessage);
				Response.ErrorNumber = this.changePassPhraseRequestResponse.Status;
				Response.WSResponse = this.changePassPhraseRequestResponse;
				if (this.changePassPhraseRequestResponse.Status != 0)
				{
					this.SaveResult(paramterXml, this.changePassPhraseRequestResponse.ErrorMessage, "RESULT");
				}
			}
			catch (Exception exception)
			{
				Exception ex = exception;
				Response<ChangePassPhraseRequestResponse> response2 = Response;
				response2.ErrorMessage = string.Concat(response2.ErrorMessage, Helper.GetExecptionMessage(ex));
			}
			Helper.SaveResponse(Response, paramterXml, "Response");
		}

		public void Connect(string paramterXml)
		{
			Response<string> Response = new Response<string>()
			{
				WSResponse = ""
			};
			try
			{
				Helper.RefreshSettings(this, Settings.Default.Properties);
				string xsltfile = string.Concat(this.DLLLocation, "\\", this.ConnectionXSLT);
				object[] dLLLocation = new object[] { this.DLLLocation, "\\Temp\\Endicia_Connect_", null, null };
				dLLLocation[2] = DateTime.Now.Ticks;
				dLLLocation[3] = ".xml";
				string OutFile = string.Concat(dLLLocation);
				if (!File.Exists(xsltfile))
				{
					Response<string> response = Response;
					response.ErrorMessage = string.Concat(response.ErrorMessage, "XSLT File Not Found :", xsltfile, " ");
					OutFile = paramterXml;
				}
				else
				{
					Helper.XSLTransform(paramterXml, xsltfile, OutFile, null);
				}
				this.credentials = Helper.FillProperties(OutFile, "//Root", (new Credentials()).GetType()) as Credentials;
				this.ewsLabelService = new EwsLabelService();
				Helper.TestCheck(paramterXml, this.ewsLabelService, Settings.Default.Endicia_WS_EwsLabelService_Test, Settings.Default.Endicia_WS_EwsLabelService_Live);
				if (this.credentials == null)
				{
					Response<string> response1 = Response;
					response1.ErrorMessage = string.Concat(response1.ErrorMessage, "Error while reading connection File ", paramterXml);
				}
			}
			catch (Exception exception)
			{
				Exception ex = exception;
				Response<string> response2 = Response;
				response2.ErrorMessage = string.Concat(response2.ErrorMessage, Helper.GetExecptionMessage(ex));
			}
			Helper.SaveResponse(Response, paramterXml, "Response");
		}

		private string GetPathFromXml(string paramterXml)
		{
			string path = "";
			XmlDocument xml = new XmlDocument();
			xml.Load(paramterXml);
			XmlNode node = xml.SelectSingleNode("//CLABELPATH");
			if (node == null)
			{
				throw new Exception(string.Concat("OutPut Path is empty in the XML File ", paramterXml));
			}
			path = node.InnerText;
			path = path.Trim();
			path = (path.EndsWith("\\") ? path : string.Concat(path, "\\"));
			if (!Directory.Exists(path))
			{
				throw new Exception(string.Concat("Invalid OutPut folder ", path));
			}
			return path;
		}

		private void ProcessShip(string paramterXml, int RowIndex)
		{
            //MMT
            //ServicePointManager.Expect100Continue = true;
            ServicePointManager.SecurityProtocol  = (SecurityProtocolType)3072;
            //MMT 
            Response<LabelRequestResponse> Response = new Response<LabelRequestResponse>();
			object[] dLLLocation = new object[] { this.DLLLocation, "\\Temp\\Endicia_Ship_", null, null };
			dLLLocation[2] = DateTime.Now.Ticks;
			dLLLocation[3] = ".xml";
			string OutFile = string.Concat(dLLLocation);
			try
			{
				string xsltfile = string.Concat(this.DLLLocation, "\\", this.ShipXSLT);
				if (!File.Exists(xsltfile))
				{
					Response<LabelRequestResponse> response = Response;
					response.ErrorMessage = string.Concat(response.ErrorMessage, "XSLT File Not Found :", xsltfile, " ");
					OutFile = paramterXml;
				}
				else
				{
					Dictionary<string, object> XSLTparams = new Dictionary<string, object>()
					{
						{ "RowIndex", RowIndex }
					};
					Helper.XSLTransform(paramterXml, xsltfile, OutFile, XSLTparams);
				}
				this.labelRequest = Helper.FillProperties(OutFile, "//Root", (new LabelRequest()).GetType()) as LabelRequest;
				this.labelRequest.AccountID = this.credentials.AccountID;
				this.labelRequest.PassPhrase = this.credentials.PassPhrase;
				this.labelRequest.RequesterID = this.credentials.RequesterID;
				this.labelRequest.PartnerCustomerID = this.credentials.RequesterID;
				this.labelRequestResponse = this.ewsLabelService.GetPostageLabel(this.labelRequest);
				Response<LabelRequestResponse> response1 = Response;
				response1.ErrorMessage = string.Concat(response1.ErrorMessage, this.labelRequestResponse.ErrorMessage);
				Response.ErrorNumber = this.labelRequestResponse.Status;
				Response.WSResponse = this.labelRequestResponse;
				if (this.labelRequestResponse.Status == 0)
				{
					string path = this.GetPathFromXml(paramterXml);
					Image img = Helper.Base64ToImage(this.labelRequestResponse.Base64LabelImage);
					try
					{
						img.Save(string.Concat(path, this.labelRequestResponse.TrackingNumber, ".jpg"), ImageFormat.Jpeg);
						img.Dispose();
					}
					finally
					{
						if (img != null)
						{
							((IDisposable)img).Dispose();
						}
					}
				}
				this.SaveAriaShipmentInfo(paramterXml, this.labelRequestResponse, RowIndex);
			}
			catch (Exception exception)
			{
				Exception ex = exception;
				Response<LabelRequestResponse> response2 = Response;
				response2.ErrorMessage = string.Concat(response2.ErrorMessage, Helper.GetExecptionMessage(ex));
			}
			Helper.SaveError(paramterXml, Response.ErrorMessage, RowIndex);
			Helper.SaveResponse(Response, OutFile, "Response");
		}

		public void RecreditAmount(string paramterXml)
		{
			object[] dLLLocation = new object[] { this.DLLLocation, "\\Temp\\Endicia_Recredit_", null, null };
			dLLLocation[2] = DateTime.Now.Ticks;
			dLLLocation[3] = ".xml";
			string OutFile = string.Concat(dLLLocation);
			Response<RecreditRequestResponse> Response = new Response<RecreditRequestResponse>();
			try
			{
				string xsltfile = string.Concat(this.DLLLocation, "\\", this.RecreditAmountXSLT);
				if (!File.Exists(xsltfile))
				{
					Response<RecreditRequestResponse> response = Response;
					response.ErrorMessage = string.Concat(response.ErrorMessage, "XSLT File Not Found :", xsltfile, " ");
					OutFile = paramterXml;
				}
				else
				{
					Helper.XSLTransform(paramterXml, xsltfile, OutFile, null);
				}
				this.recreditRequest = Helper.FillProperties(OutFile, "//Root", (new RecreditRequest()).GetType()) as RecreditRequest;
				this.recreditRequest.CertifiedIntermediary = new CertifiedIntermediary()
				{
					AccountID = this.credentials.AccountID,
					PassPhrase = this.credentials.PassPhrase
				};
				this.recreditRequest.RequesterID = this.credentials.RequesterID;
				this.recreditRequest.RequestID = Guid.NewGuid().ToString();
				this.recreditRequestResponse = this.ewsLabelService.BuyPostage(this.recreditRequest);
				Response<RecreditRequestResponse> response1 = Response;
				response1.ErrorMessage = string.Concat(response1.ErrorMessage, this.recreditRequestResponse.ErrorMessage);
				Response.ErrorNumber = this.recreditRequestResponse.Status;
				Response.WSResponse = this.recreditRequestResponse;
				if (this.recreditRequestResponse.Status != 0)
				{
					this.SaveResult(paramterXml, this.recreditRequestResponse.ErrorMessage, "RESULT");
				}
			}
			catch (Exception exception)
			{
				Exception ex = exception;
				Response<RecreditRequestResponse> response2 = Response;
				response2.ErrorMessage = string.Concat(response2.ErrorMessage, Helper.GetExecptionMessage(ex));
			}
			Helper.SaveResponse(Response, paramterXml, "Response");
		}

		public void SaveAriaShipmentInfo(string paramterXml, LabelRequestResponse replay, int RowIndex)
		{
			XmlDocument xml = new XmlDocument();
			xml.Load(paramterXml);
			xml.SelectSingleNode(string.Concat("//ROW[position()=", RowIndex, "]/NFREIGHT"));
			xml.SelectSingleNode(string.Concat("//ROW[position()=", RowIndex, "]/CDECL_VAL"));
			XmlNode CCOD = xml.SelectSingleNode(string.Concat("//ROW[position()=", RowIndex, "]/CCOD"));
			xml.SelectSingleNode(string.Concat("//ROW[position()=", RowIndex, "]/CCOD_AMT"));
			XmlNode TRACKING_NO = xml.SelectSingleNode(string.Concat("//ROW[position()=", RowIndex, "]/TRACKING_NO"));
			xml.SelectSingleNode(string.Concat("//ROW[position()=", RowIndex, "]/CARRIER_SHIPMENT_ID"));
			xml.SelectSingleNode(string.Concat("//ROW[position()=", RowIndex, "]/CARRIER_SHIPMENT_DIGEST"));
			if (CCOD != null)
			{
				CCOD.InnerText = replay.FinalPostage.ToString();
			}
			if (TRACKING_NO != null)
			{
				TRACKING_NO.InnerText = replay.TrackingNumber;
			}
			lock (this)
			{
				xml.Save(paramterXml);
			}
		}

		public void SaveResult(string paramterXml, string Result, string NodeName)
		{
			if (File.Exists(paramterXml))
			{
				XmlDocument xmlDocument = new XmlDocument();
				xmlDocument.Load(paramterXml);
				XmlNode ResultNode = xmlDocument.SelectSingleNode(string.Concat("//", NodeName));
				if (ResultNode != null)
				{
					ResultNode.InnerText = Result;
					lock (xmlDocument)
					{
						xmlDocument.Save(paramterXml);
					}
				}
			}
		}

		public void Ship(string paramterXml)
		{ 
            XmlDocument xml = new XmlDocument();
			xml.Load(paramterXml);
			int PackageCount = xml.SelectNodes("//ROW").Count;
			for (int x = 0; x < PackageCount; x++)
			{
				this.ProcessShip(paramterXml, x + 1);
			}
		}
	}
}