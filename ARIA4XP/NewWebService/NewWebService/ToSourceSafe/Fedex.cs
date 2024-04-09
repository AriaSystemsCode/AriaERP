using Fedex.WebReference2;
using Fedex.FedexTrack;
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
using Fedex.WebReference;

namespace Fedex
{
	public class Fedex
	{
		private string ConnectionXSLT = "Fedex_Connection.xslt";

		private string RateXSLT = "Fedex_Rate.xslt";

		private string VoidXSLT = "Fedex_Void.xslt";

		private string ShipMasterXSLT = "Fedex_MasterShip.xslt";

		private string ShipChildXSLT = "Fedex_ChildShip.xslt";

		private string ReturnsXSLT = "Fedex_Return.xslt";

		private Credentials credentials
		{
			get;
			set;
		}

		private DeleteShipmentRequest deleteShipmentRequest
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

		private ProcessShipmentReply processShipmentChildReply
		{
			get;
			set;
		}

		private ProcessShipmentRequest processShipmentChildRequest
		{
			get;
			set;
		}

		private ProcessShipmentReply processShipmentReply
		{
			get;
			set;
		}

		private ProcessShipmentRequest processShipmentRequest
		{
			get;
			set;
		}

		private RateReply rateReply
		{
			get;
			set;
		}

		private RateRequest rateRequest
		{
			get;
			set;
		}

		private RateService rateService
		{
			get;
			set;
		}

		private ShipmentReply shipmentReply
		{
			get;
			set;
		}

		private ShipService shipService
		{
			get;
			set;
		}

		private TrackReply trackReply
		{
			get;
			set;
		}

		private TrackRequest trackRequest
		{
			get;
			set;
		}

		private TrackService trackService
		{
			get;
			set;
		}

		public Fedex()
		{
            ServicePointManager.Expect100Continue = true;
            ServicePointManager.SecurityProtocol = (SecurityProtocolType)3072;
        }

		public void Connect(string paramterXml)
		{
			Response<string> response = new Response<string>()
			{
				WSResponse = ""
			};
			try
			{
				Helper.RefreshSettings(this, Fedex_1.Properties.Settings.Default.Properties);
				string str = string.Concat(this.DLLLocation, "\\", this.ConnectionXSLT);
				object[] dLLLocation = new object[] { this.DLLLocation, "\\Temp\\Fedex_Connect_", null, null };
				dLLLocation[2] = DateTime.Now.Ticks;
				dLLLocation[3] = ".xml";
				string str1 = string.Concat(dLLLocation);
				if (!File.Exists(str))
				{
					Response<string> response1 = response;
					response1.ErrorMessage = string.Concat(response1.ErrorMessage, "XSLT File Not Found :", str, " ");
					str1 = paramterXml;
				}
				else
				{
					Helper.XSLTransform(paramterXml, str, str1, null);
				}
				this.shipService = new ShipService();
				this.trackService = new TrackService();
				this.rateService = new RateService();
				this.credentials = Helper.FillProperties(str1, "//Root", (new Credentials()).GetType()) as Credentials;
				if (this.credentials == null)
				{
					Response<string> response2 = response;
					response2.ErrorMessage = string.Concat(response2.ErrorMessage, "Error while reading connection File ", paramterXml);
				}
			}
			catch (Exception exception1)
			{
				Exception exception = exception1;
				Response<string> response3 = response;
				response3.ErrorMessage = string.Concat(response3.ErrorMessage, Helper.GetExecptionMessage(exception));
			}
			Helper.SaveResponse(response, paramterXml, "Response");
		}

		public void DeleteShipment(string paramterXml)
		{
			Exception exception;
			Response<ShipmentReply> response = new Response<ShipmentReply>();
			int count = 0;
			string str = string.Concat(this.DLLLocation, "\\", this.VoidXSLT);
			object[] dLLLocation = new object[] { this.DLLLocation, "\\Temp\\Fedex_Void_", null, null };
			dLLLocation[2] = DateTime.Now.Ticks;
			dLLLocation[3] = ".xml";
			string str1 = string.Concat(dLLLocation);
			try
			{
				if (!File.Exists(str))
				{
					Response<ShipmentReply> response1 = response;
					response1.ErrorMessage = string.Concat(response1.ErrorMessage, "XSLT File Not Found :", str, " ");
					str1 = paramterXml;
				}
				else
				{
					Helper.XSLTransform(paramterXml, str, str1, null);
				}
				XmlDocument xmlDocument = new XmlDocument();
				xmlDocument.Load(str1);
				count = xmlDocument.SelectNodes("//Request").Count + 1;
			}
			catch (Exception exception1)
			{
				exception = exception1;
				Response<ShipmentReply> response2 = response;
				response2.ErrorMessage = string.Concat(response2.ErrorMessage, Helper.GetExecptionMessage(exception));
				Helper.SaveResponse(response, paramterXml, "Response");
				return;
			}
			for (int i = 1; i < count; i++)
			{
				try
				{
					response = new Response<ShipmentReply>();
					this.deleteShipmentRequest = Helper.FillProperties(str1, string.Concat("//Request[Sequence = ", i, "]"), (new DeleteShipmentRequest()).GetType()) as DeleteShipmentRequest;
                    this.deleteShipmentRequest.WebAuthenticationDetail = new WebReference.WebAuthenticationDetail()
					{
                        UserCredential = new WebReference.WebAuthenticationCredential()
					};
                    this.deleteShipmentRequest.ClientDetail = new WebReference.ClientDetail();
					this.deleteShipmentRequest.WebAuthenticationDetail.UserCredential.Key = this.credentials.Key;
					this.deleteShipmentRequest.WebAuthenticationDetail.UserCredential.Password = this.credentials.Password;
					this.deleteShipmentRequest.ClientDetail.AccountNumber = this.credentials.AccountNumber;
					this.deleteShipmentRequest.ClientDetail.MeterNumber = this.credentials.MeterNumber;
                    this.deleteShipmentRequest.Version = new WebReference.VersionId();
					this.shipmentReply = this.shipService.deleteShipment(this.deleteShipmentRequest);
					response.WSResponse = this.shipmentReply;
					if ((this.shipmentReply.Notifications == null ? false : (int)this.shipmentReply.Notifications.Length > 0))
					{
                        WebReference.Notification[] notifications = this.shipmentReply.Notifications;
						for (int j = 0; j < (int)notifications.Length; j++)
						{
                            WebReference.Notification notification = notifications[j];
                            if (notification.Severity != WebReference.NotificationSeverityType.SUCCESS)
							{
								int num = 0;
								Response<ShipmentReply> response3 = response;
								response3.ErrorMessage = string.Concat(response3.ErrorMessage, notification.Message, Environment.NewLine);
								int.TryParse(this.shipmentReply.Notifications[0].Code, out num);
								response.ErrorNumber = num;
							}
						}
					}
				}
				catch (Exception exception2)
				{
					exception = exception2;
					Response<ShipmentReply> response4 = response;
					response4.ErrorMessage = string.Concat(response4.ErrorMessage, Helper.GetExecptionMessage(exception));
				}
				this.SaveAriaVoidInfo(paramterXml, response, i - 1);
				Helper.SaveResponse(response, str1, string.Concat("Response", i));
			}
		}

		private string GetPathFromXml(string paramterXml)
		{
			string innerText = "";
			XmlDocument xmlDocument = new XmlDocument();
			xmlDocument.Load(paramterXml);
			XmlNode xmlNodes = xmlDocument.SelectSingleNode("//CLABELPATH");
			if (xmlNodes == null)
			{
				throw new Exception(string.Concat("OutPut Path is empty in the XML File ", paramterXml));
			}
			innerText = xmlNodes.InnerText;
			innerText = innerText.Trim();
			innerText = (innerText.EndsWith("\\") ? innerText : string.Concat(innerText, "\\"));
			if (!Directory.Exists(innerText))
			{
				throw new Exception(string.Concat("Invalid OutPut folder ", innerText));
			}
			return innerText;
		}

		private void ProcessShip(RequestType type, bool IsMaster, string paramterXml, WebReference.TrackingId MasterTrackingNumber, int RowIndex, int PackageCount, int SequenceNumber, bool IsMPS)
		{
			string shipMasterXSLT;
			Response<ProcessShipmentReply> response = new Response<ProcessShipmentReply>();
			try
			{
				(new XmlDocument()).Load(paramterXml);
				ProcessShipmentRequest webAuthenticationDetail = (type == RequestType.ShipMaster ? this.processShipmentRequest : this.processShipmentChildRequest);
				string dLLLocation = this.DLLLocation;
				if (type == RequestType.ShipMaster)
				{
					shipMasterXSLT = this.ShipMasterXSLT;
				}
				else
				{
					shipMasterXSLT = (type == RequestType.Returns ? this.ReturnsXSLT : this.ShipChildXSLT);
				}
				string str = string.Concat(dLLLocation, "\\", shipMasterXSLT);
				object[] ticks = new object[] { this.DLLLocation, "\\Temp\\Fedex_Ship_", type.ToString(), null, null };
				ticks[3] = DateTime.Now.Ticks;
				ticks[4] = ".xml";
				string str1 = string.Concat(ticks);
				Dictionary<string, object> strs = new Dictionary<string, object>()
				{
					{ "RowIndex", RowIndex },
					{ "PackageCount", PackageCount },
					{ "SequenceNumber", SequenceNumber }
				};
				if (!File.Exists(str))
				{
					Response<ProcessShipmentReply> response1 = response;
					response1.ErrorMessage = string.Concat(response1.ErrorMessage, "XSLT File Not Found :", str, " ");
					str1 = paramterXml;
				}
				else
				{
					Helper.XSLTransform(paramterXml, str, str1, strs);
				}
				string pathFromXml = this.GetPathFromXml(paramterXml);
				webAuthenticationDetail = Helper.FillProperties(str1, "//Root", (new ProcessShipmentRequest()).GetType()) as ProcessShipmentRequest;
                webAuthenticationDetail.WebAuthenticationDetail = new WebReference.WebAuthenticationDetail()
				{
                    UserCredential = new WebReference.WebAuthenticationCredential()
					{
						Key = this.credentials.Key,
						Password = this.credentials.Password
					}
				};
                webAuthenticationDetail.ClientDetail = new WebReference.ClientDetail()
				{
					AccountNumber = this.credentials.AccountNumber,
					MeterNumber = this.credentials.MeterNumber
				};
                webAuthenticationDetail.Version = new WebReference.VersionId();
				if ((webAuthenticationDetail.RequestedShipment == null || webAuthenticationDetail.RequestedShipment.CustomsClearanceDetail == null || webAuthenticationDetail.RequestedShipment.CustomsClearanceDetail.DutiesPayment == null ? false : webAuthenticationDetail.RequestedShipment.CustomsClearanceDetail.DutiesPayment.Payor != null))
				{
					///////////////////////////////webAuthenticationDetail.RequestedShipment.CustomsClearanceDetail.DutiesPayment.Payor.ResponsibleParty = this.credentials.AccountNumber;
				}
				if (webAuthenticationDetail.RequestedShipment != null)
				{
					webAuthenticationDetail.RequestedShipment.ShipTimestamp = DateTime.Now;
				}
				if (!IsMaster)
				{
					webAuthenticationDetail.RequestedShipment.MasterTrackingId = MasterTrackingNumber;
				}
             

                ProcessShipmentReply processShipmentReply = this.shipService.processShipment(webAuthenticationDetail);
				if (type != RequestType.ShipMaster)
				{
					this.processShipmentChildReply = processShipmentReply;
				}
				else
				{
					this.processShipmentReply = processShipmentReply;
				}
				response = new Response<ProcessShipmentReply>()
				{
					WSResponse = this.processShipmentReply
				};
				if ((processShipmentReply.Notifications == null ? false : (int)processShipmentReply.Notifications.Length > 0))
				{

                    WebReference.Notification[] notifications = processShipmentReply.Notifications;
					for (int i = 0; i < (int)notifications.Length; i++)
					{
                        WebReference.Notification notification = notifications[i];
                        if (notification.Severity != WebReference.NotificationSeverityType.SUCCESS)
						{
							int num = 0;
							Response<ProcessShipmentReply> response2 = response;
							response2.ErrorMessage = string.Concat(response2.ErrorMessage, notification.Message, Environment.NewLine);
							int.TryParse(processShipmentReply.Notifications[0].Code, out num);
							response.ErrorNumber = num;
						}
					}
				}
				this.SaveLabelPaths(processShipmentReply, pathFromXml);
				Helper.SaveResponse(response, str1, "Response");
				if (type != RequestType.Returns)
				{
					this.SaveAriaShipmentInfo(paramterXml, processShipmentReply, RowIndex, IsMPS);
				}
				else
				{
					this.SaveAriaReturnInfo(paramterXml, processShipmentReply, RowIndex);
				}
			}
			catch (Exception exception1)
			{
				Exception exception = exception1;
				Response<ProcessShipmentReply> response3 = response;
				response3.ErrorMessage = string.Concat(response3.ErrorMessage, Helper.GetExecptionMessage(exception));
			}
			Helper.SaveError(paramterXml, response.ErrorMessage, RowIndex);
		}

		public void Rate(string paramterXml)
		{
			Response<RateReply> response = new Response<RateReply>();
			try
			{
				Helper.TestCheck(paramterXml, this.rateService, Fedex_1.Properties.Settings.Default.Fedex_FedexRate_RateService_TEST, Fedex_1.Properties.Settings.Default.Fedex_FedexRate_RateService);
				string str = string.Concat(this.DLLLocation, "\\", this.RateXSLT);
				object[] dLLLocation = new object[] { this.DLLLocation, "\\Temp\\Fedex_Rate_", null, null };
				dLLLocation[2] = DateTime.Now.Ticks;
				dLLLocation[3] = ".xml";
				string str1 = string.Concat(dLLLocation);
				if (!File.Exists(str))
				{
					Response<RateReply> response1 = response;
					response1.ErrorMessage = string.Concat(response1.ErrorMessage, "XSLT File Not Found :", str, " ");
					str1 = paramterXml;
				}
				else
				{
					Helper.XSLTransform(paramterXml, str, str1, null);
				}
				this.rateRequest = Helper.FillProperties(str1, "//Root", (new RateRequest()).GetType()) as RateRequest;
				this.rateRequest.WebAuthenticationDetail = new  WebReference2.WebAuthenticationDetail()
				{
                    UserCredential = new WebReference2.WebAuthenticationCredential()
				};
                this.rateRequest.ClientDetail = new WebReference2.ClientDetail();
				this.rateRequest.WebAuthenticationDetail.UserCredential.Key = this.credentials.Key;
				this.rateRequest.WebAuthenticationDetail.UserCredential.Password = this.credentials.Password;
				this.rateRequest.ClientDetail.AccountNumber = this.credentials.AccountNumber;
				this.rateRequest.ClientDetail.MeterNumber = this.credentials.MeterNumber;
                this.rateRequest.Version = new WebReference2.VersionId();
				if (this.rateRequest.RequestedShipment != null)
				{
					this.rateRequest.RequestedShipment.ShipTimestamp = DateTime.Now;
				}
				this.rateReply = this.rateService.getRates(this.rateRequest);
				response.WSResponse = this.rateReply;
				if ((this.rateReply.Notifications == null ? false : (int)this.rateReply.Notifications.Length > 0))
				{
                    WebReference2.Notification[] notifications = this.rateReply.Notifications;
					for (int i = 0; i < (int)notifications.Length; i++)
					{
                        WebReference2.Notification notification = notifications[i];
                        if (notification.Severity != WebReference2.NotificationSeverityType.SUCCESS)
						{
							int num = 0;
							Response<RateReply> response2 = response;
							response2.ErrorMessage = string.Concat(response2.ErrorMessage, notification.Message, Environment.NewLine);
							int.TryParse(this.rateReply.Notifications[0].Code, out num);
							response.ErrorNumber = num;
						}
					}
				}
				this.SaveAriaRateInfo(paramterXml, this.rateReply);
			}
			catch (Exception exception1)
			{
				Exception exception = exception1;
				Response<RateReply> response3 = response;
				response3.ErrorMessage = string.Concat(response3.ErrorMessage, Helper.GetExecptionMessage(exception));
			}
			Helper.SaveResponse(response, paramterXml, "Response");
		}

		private void SaveAriaRateInfo(string paramterXml, RateReply rateReply)
		{
			decimal amount;
			XmlDocument xmlDocument = new XmlDocument();
			xmlDocument.Load(paramterXml);
			int count = xmlDocument.SelectNodes("//ROW").Count;
			XmlNodeList xmlNodeLists = xmlDocument.SelectNodes("//NFREIGHT");
			XmlNodeList xmlNodeLists1 = xmlDocument.SelectNodes("//CDECL_VAL");
			XmlNodeList xmlNodeLists2 = xmlDocument.SelectNodes("//CCOD");
			for (int i = 0; i < count; i++)
			{
				if ((xmlNodeLists[i] == null || rateReply == null || rateReply.RateReplyDetails == null || (int)rateReply.RateReplyDetails.Length <= 0 || rateReply.RateReplyDetails[0].RatedShipmentDetails == null || (int)rateReply.RateReplyDetails[0].RatedShipmentDetails.Length <= 0 || rateReply.RateReplyDetails[0].RatedShipmentDetails[0].RatedPackages == null || (int)rateReply.RateReplyDetails[0].RatedShipmentDetails[0].RatedPackages.Length <= 0 || rateReply.RateReplyDetails[0].RatedShipmentDetails[0].RatedPackages[0].PackageRateDetail == null ? false : rateReply.RateReplyDetails[0].RatedShipmentDetails[0].RatedPackages[0].PackageRateDetail.NetFreight != null))
				{
					XmlNode itemOf = xmlNodeLists[i];
					amount = rateReply.RateReplyDetails[0].RatedShipmentDetails[0].RatedPackages[0].PackageRateDetail.NetFreight.Amount;
					itemOf.InnerText = amount.ToString();
				}
				if ((xmlNodeLists1[i] == null || rateReply == null || rateReply.RateReplyDetails == null || (int)rateReply.RateReplyDetails.Length <= 0 || rateReply.RateReplyDetails[0].RatedShipmentDetails == null || (int)rateReply.RateReplyDetails[0].RatedShipmentDetails.Length <= 0 || rateReply.RateReplyDetails[0].RatedShipmentDetails[0].RatedPackages == null || (int)rateReply.RateReplyDetails[0].RatedShipmentDetails[0].RatedPackages.Length <= 0 || rateReply.RateReplyDetails[0].RatedShipmentDetails[0].RatedPackages[0].PackageRateDetail == null ? false : rateReply.RateReplyDetails[0].RatedShipmentDetails[0].RatedPackages[0].PackageRateDetail.BaseCharge != null))
				{
					XmlNode str = xmlNodeLists1[i];
					amount = rateReply.RateReplyDetails[0].RatedShipmentDetails[0].RatedPackages[0].PackageRateDetail.BaseCharge.Amount;
					str.InnerText = amount.ToString();
				}
				if ((xmlNodeLists2[i] == null || rateReply == null || rateReply.RateReplyDetails == null || (int)rateReply.RateReplyDetails.Length <= 0 || rateReply.RateReplyDetails[0].RatedShipmentDetails == null || (int)rateReply.RateReplyDetails[0].RatedShipmentDetails.Length <= 0 || rateReply.RateReplyDetails[0].RatedShipmentDetails[0].RatedPackages == null || (int)rateReply.RateReplyDetails[0].RatedShipmentDetails[0].RatedPackages.Length <= 0 || rateReply.RateReplyDetails[0].RatedShipmentDetails[0].RatedPackages[0].PackageRateDetail == null ? false : rateReply.RateReplyDetails[0].RatedShipmentDetails[0].RatedPackages[0].PackageRateDetail.NetCharge != null))
				{
					XmlNode xmlNodes = xmlNodeLists2[i];
					amount = rateReply.RateReplyDetails[0].RatedShipmentDetails[0].RatedPackages[0].PackageRateDetail.NetCharge.Amount;
					xmlNodes.InnerText = amount.ToString();
				}
			}
		}

		private void SaveAriaReturnInfo(string paramterXml, ProcessShipmentReply Replay, int RowIndex)
		{
			XmlDocument xmlDocument = new XmlDocument();
			xmlDocument.Load(paramterXml);
			XmlNode trackingNumber = xmlDocument.SelectSingleNode(string.Concat("//ROW[position()=", RowIndex, "]/RETURN_TRACKING_NO"));
			if ((trackingNumber == null || Replay.CompletedShipmentDetail == null || Replay.CompletedShipmentDetail.CompletedPackageDetails[0] == null ? false : Replay.CompletedShipmentDetail.CompletedPackageDetails[0].TrackingIds[0] != null))
			{
				trackingNumber.InnerText = Replay.CompletedShipmentDetail.CompletedPackageDetails[0].TrackingIds[0].TrackingNumber;
			}
			lock (this)
			{
				xmlDocument.Save(paramterXml);
			}
		}

		public void SaveAriaShipmentInfo(string paramterXml, ProcessShipmentReply Replay, int RowIndex, bool IsMPS)
		{
			decimal amount;
			XmlDocument xmlDocument = new XmlDocument();
			xmlDocument.Load(paramterXml);
			XmlNode str = xmlDocument.SelectSingleNode(string.Concat("//ROW[position()=", RowIndex, "]/NFREIGHT"));
			XmlNode xmlNodes = xmlDocument.SelectSingleNode(string.Concat("//ROW[position()=", RowIndex, "]/CDECL_VAL"));
			XmlNode str1 = xmlDocument.SelectSingleNode(string.Concat("//ROW[position()=", RowIndex, "]/CCOD"));
			xmlDocument.SelectSingleNode(string.Concat("//ROW[position()=", RowIndex, "]/CCOD_AMT"));
			XmlNode trackingNumber = xmlDocument.SelectSingleNode(string.Concat("//ROW[position()=", RowIndex, "]/TRACKING_NO"));
			xmlDocument.SelectSingleNode(string.Concat("//ROW[position()=", RowIndex, "]/CARRIER_SHIPMENT_ID"));
			xmlDocument.SelectSingleNode(string.Concat("//ROW[position()=", RowIndex, "]/CARRIER_SHIPMENT_DIGEST"));
			XmlNode xmlNodes1 = xmlDocument.SelectSingleNode(string.Concat("//ROW[position()=", RowIndex, "]/TRACKING_ID_TYPE"));
			if (!IsMPS)
			{
				if ((str == null || Replay == null || Replay.CompletedShipmentDetail.ShipmentRating == null || Replay.CompletedShipmentDetail.ShipmentRating.ShipmentRateDetails[0] == null ? false : Replay.CompletedShipmentDetail.ShipmentRating.ShipmentRateDetails[0].TotalNetFreight != null))
				{
					amount = Replay.CompletedShipmentDetail.ShipmentRating.ShipmentRateDetails[0].TotalNetFreight.Amount;
					str.InnerText = amount.ToString();
				}
				if ((xmlNodes == null || Replay.CompletedShipmentDetail == null || Replay.CompletedShipmentDetail.ShipmentRating == null || Replay.CompletedShipmentDetail.ShipmentRating.ShipmentRateDetails[0] == null ? false : Replay.CompletedShipmentDetail.ShipmentRating.ShipmentRateDetails[0].TotalBaseCharge != null))
				{
					amount = Replay.CompletedShipmentDetail.ShipmentRating.ShipmentRateDetails[0].TotalBaseCharge.Amount;
					xmlNodes.InnerText = amount.ToString();
				}
				if ((str1 == null || Replay.CompletedShipmentDetail == null || Replay.CompletedShipmentDetail.ShipmentRating == null || Replay.CompletedShipmentDetail.ShipmentRating.ShipmentRateDetails[0] == null ? false : Replay.CompletedShipmentDetail.ShipmentRating.ShipmentRateDetails[0].TotalNetCharge != null))
				{
					amount = Replay.CompletedShipmentDetail.ShipmentRating.ShipmentRateDetails[0].TotalNetCharge.Amount;
					str1.InnerText = amount.ToString();
				}
				if ((trackingNumber == null || Replay.CompletedShipmentDetail == null || Replay.CompletedShipmentDetail.CompletedPackageDetails[0] == null ? false : Replay.CompletedShipmentDetail.CompletedPackageDetails[0].TrackingIds[0] != null))
				{
					trackingNumber.InnerText = Replay.CompletedShipmentDetail.CompletedPackageDetails[0].TrackingIds[0].TrackingNumber;
					if ((!Replay.CompletedShipmentDetail.CompletedPackageDetails[0].TrackingIds[0].TrackingIdTypeSpecified ? false : xmlNodes1 != null))
					{
						xmlNodes1.InnerText = Replay.CompletedShipmentDetail.CompletedPackageDetails[0].TrackingIds[0].TrackingIdType.ToString();
					}
				}
			}
			else
			{
				if ((str == null || Replay == null || Replay.CompletedShipmentDetail == null || Replay.CompletedShipmentDetail.CompletedPackageDetails == null || Replay.CompletedShipmentDetail.CompletedPackageDetails[0] == null || Replay.CompletedShipmentDetail.CompletedPackageDetails[0].PackageRating == null || Replay.CompletedShipmentDetail.CompletedPackageDetails[0].PackageRating.PackageRateDetails == null || Replay.CompletedShipmentDetail.CompletedPackageDetails[0].PackageRating.PackageRateDetails[0] == null ? false : Replay.CompletedShipmentDetail.CompletedPackageDetails[0].PackageRating.PackageRateDetails[0].NetFreight != null))
				{
					amount = Replay.CompletedShipmentDetail.CompletedPackageDetails[0].PackageRating.PackageRateDetails[0].NetFreight.Amount;
					str.InnerText = amount.ToString();
				}
				if ((xmlNodes == null || Replay == null || Replay.CompletedShipmentDetail == null || Replay.CompletedShipmentDetail.CompletedPackageDetails == null || Replay.CompletedShipmentDetail.CompletedPackageDetails[0] == null || Replay.CompletedShipmentDetail.CompletedPackageDetails[0].PackageRating == null || Replay.CompletedShipmentDetail.CompletedPackageDetails[0].PackageRating.PackageRateDetails == null || Replay.CompletedShipmentDetail.CompletedPackageDetails[0].PackageRating.PackageRateDetails[0] == null ? false : Replay.CompletedShipmentDetail.CompletedPackageDetails[0].PackageRating.PackageRateDetails[0].BaseCharge != null))
				{
					amount = Replay.CompletedShipmentDetail.CompletedPackageDetails[0].PackageRating.PackageRateDetails[0].BaseCharge.Amount;
					xmlNodes.InnerText = amount.ToString();
				}
				if ((str1 == null || Replay == null || Replay.CompletedShipmentDetail == null || Replay.CompletedShipmentDetail.CompletedPackageDetails == null || Replay.CompletedShipmentDetail.CompletedPackageDetails[0] == null || Replay.CompletedShipmentDetail.CompletedPackageDetails[0].PackageRating == null || Replay.CompletedShipmentDetail.CompletedPackageDetails[0].PackageRating.PackageRateDetails == null || Replay.CompletedShipmentDetail.CompletedPackageDetails[0].PackageRating.PackageRateDetails[0] == null ? false : Replay.CompletedShipmentDetail.CompletedPackageDetails[0].PackageRating.PackageRateDetails[0].NetCharge != null))
				{
					amount = Replay.CompletedShipmentDetail.CompletedPackageDetails[0].PackageRating.PackageRateDetails[0].NetCharge.Amount;
					str1.InnerText = amount.ToString();
				}
				if ((trackingNumber == null || Replay.CompletedShipmentDetail == null || Replay.CompletedShipmentDetail.CompletedPackageDetails[0] == null ? false : Replay.CompletedShipmentDetail.CompletedPackageDetails[0].TrackingIds[0] != null))
				{
					trackingNumber.InnerText = Replay.CompletedShipmentDetail.CompletedPackageDetails[0].TrackingIds[0].TrackingNumber;
					if ((!Replay.CompletedShipmentDetail.CompletedPackageDetails[0].TrackingIds[0].TrackingIdTypeSpecified ? false : xmlNodes1 != null))
					{
						xmlNodes1.InnerText = Replay.CompletedShipmentDetail.CompletedPackageDetails[0].TrackingIds[0].TrackingIdType.ToString();
					}
				}
			}
			lock (this)
			{
				xmlDocument.Save(paramterXml);
			}
		}

		public void SaveAriaVoidInfo(string paramterXml, Response<ShipmentReply> replay, int index)
		{
			XmlDocument xmlDocument = new XmlDocument();
			xmlDocument.Load(paramterXml);
			XmlNodeList str = xmlDocument.SelectNodes("//STATUS");
			XmlNodeList errorMessage = xmlDocument.SelectNodes("//DESCRIPTION");
			if ((str.Count <= 0 || errorMessage.Count <= 0 || str[index] == null ? false : errorMessage[index] != null))
			{
				if (replay.WSResponse == null)
				{
					errorMessage[index].InnerText = replay.ErrorMessage;
					str[index].InnerText = replay.ErrorNumber.ToString();
				}
				else
				{
                    if ((replay.WSResponse.HighestSeverity == WebReference.NotificationSeverityType.SUCCESS || replay.WSResponse.HighestSeverity == WebReference.NotificationSeverityType.NOTE ? false : replay.WSResponse.HighestSeverity != WebReference.NotificationSeverityType.WARNING))
					{
						str[index].InnerText = replay.WSResponse.HighestSeverity.ToString();
					}
					else
					{
						str[index].InnerText = "V";
					}
					if ((replay.WSResponse.Notifications == null ? false : (int)replay.WSResponse.Notifications.Length > 0))
					{
						string str1 = "";
                        WebReference.Notification[] notifications = replay.WSResponse.Notifications;
						for (int i = 0; i < (int)notifications.Length; i++)
						{
                            WebReference.Notification notification = notifications[i];
							str1 = string.Concat(str1, notification.Message, Environment.NewLine);
						}
						errorMessage[index].InnerText = str1;
					}
				}
				lock (this)
				{
					xmlDocument.Save(paramterXml);
				}
			}
		}

		public void SaveLabelPaths(ProcessShipmentReply replay, string Path)
		{
			if ((replay.CompletedShipmentDetail == null || replay.CompletedShipmentDetail.CompletedPackageDetails == null ? false : !string.IsNullOrEmpty(Path)))
			{
				CompletedPackageDetail[] completedPackageDetails = replay.CompletedShipmentDetail.CompletedPackageDetails;
				for (int i = 0; i < (int)completedPackageDetails.Length; i++)
				{
					CompletedPackageDetail completedPackageDetail = completedPackageDetails[i];
					ShippingDocumentPart[] parts = completedPackageDetail.Label.Parts;
					for (int j = 0; j < (int)parts.Length; j++)
					{
						Image image = Helper.BytesToImage(parts[j].Image);
						try
						{
							image.RotateFlip(RotateFlipType.Rotate90FlipNone);
							image.Save(string.Concat(Path, completedPackageDetail.TrackingIds[0].TrackingNumber, ".jpg"), ImageFormat.Jpeg);
							image.Dispose();
						}
						finally
						{
							if (image != null)
							{
								((IDisposable)image).Dispose();
							}
						}
					}
				}
			}
		}

		public void Ship(string paramterXml)
		{
			int i;
			XmlDocument xmlDocument = new XmlDocument();
			xmlDocument.Load(paramterXml);
			int count = xmlDocument.SelectNodes("//ROW").Count;
			XmlNode xmlNodes = xmlDocument.SelectSingleNode("//Return_Flag");
			string str = (xmlNodes != null ? Convert.ToString(xmlNodes.InnerText).ToUpper().Trim() : "");
			Helper.TestCheck(paramterXml, this.shipService, Fedex_1.Properties.Settings.Default.Fedex_FedexShip_ShipService_TEST, Fedex_1.Properties.Settings.Default.Fedex_FedexShip_ShipService);
			bool flag = true;
			if ((str == "S" ? true : str == "B"))
			{
				if (!flag)
				{
					for (i = 0; i < count; i++)
					{
						this.ProcessShip(RequestType.SingleShip, false, paramterXml, null, i + 1, 1, 1, false);
					}
				}
				else
				{
					this.ProcessShip(RequestType.ShipMaster, true, paramterXml, null, 1, count, 1, true);
                    try
                    {
                        if ((this.processShipmentReply.HighestSeverity == WebReference.NotificationSeverityType.ERROR ? false : this.processShipmentReply.HighestSeverity != WebReference.NotificationSeverityType.FAILURE))
                        {
                            for (i = 1; i < count; i++)
                            {
                                this.ProcessShip(RequestType.ShipChild, false, paramterXml, this.processShipmentReply.CompletedShipmentDetail.MasterTrackingId, i + 1, count, i + 1, true);
                            }
                        }
                    }
                    catch { }
				}
			}
			if ((str == "R" ? true : str == "B"))
			{
				for (i = 0; i < count; i++)
				{
					this.ProcessShip(RequestType.Returns, false, paramterXml, null, i + 1, 1, 1, false);
				}
			}
		}

		public void Track(string paramterXml)
		{
			Response<TrackReply> response = new Response<TrackReply>();
			try
			{
				Helper.TestCheck(paramterXml, this.trackService, Fedex_1.Properties.Settings.Default.Fedex_FedexTrack_TrackService_TEST, Fedex_1.Properties.Settings.Default.Fedex_FedexTrack_TrackService);
				this.trackRequest = Helper.FillProperties(paramterXml, "//Root", (new TrackRequest()).GetType()) as TrackRequest;
                this.trackRequest.WebAuthenticationDetail = new FedexTrack.WebAuthenticationDetail()
				{
                    UserCredential = new FedexTrack.WebAuthenticationCredential()
				};
                this.trackRequest.ClientDetail = new FedexTrack.ClientDetail();
				this.trackRequest.WebAuthenticationDetail.UserCredential.Key = this.credentials.Key;
				this.trackRequest.WebAuthenticationDetail.UserCredential.Password = this.credentials.Password;
				this.trackRequest.ClientDetail.AccountNumber = this.credentials.AccountNumber;
				this.trackRequest.ClientDetail.MeterNumber = this.credentials.MeterNumber;
                this.trackRequest.Version = new FedexTrack.VersionId();
				this.trackReply = this.trackService.track(this.trackRequest);
				response.WSResponse = this.trackReply;
				if ((this.trackReply.Notifications == null ? false : (int)this.trackReply.Notifications.Length > 0))
				{
                    FedexTrack.Notification[] notifications = this.trackReply.Notifications;
					for (int i = 0; i < (int)notifications.Length; i++)
					{
                        FedexTrack.Notification notification = notifications[i];
						int num = 0;
						Response<TrackReply> response1 = response;
						response1.ErrorMessage = string.Concat(response1.ErrorMessage, notification.Message, Environment.NewLine);
						int.TryParse(this.trackReply.Notifications[0].Code, out num);
						response.ErrorNumber = num;
					}
				}
			}
			catch (Exception exception1)
			{
				Exception exception = exception1;
				Response<TrackReply> response2 = response;
				response2.ErrorMessage = string.Concat(response2.ErrorMessage, Helper.GetExecptionMessage(exception));
			}
			Helper.SaveResponse(response, paramterXml, "Response");
		}
	}
}