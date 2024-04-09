using Endicia.Properties;
using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Threading;
using System.Web.Services;
using System.Web.Services.Description;
using System.Web.Services.Protocols;
using System.Xml.Serialization;
using Endicia.WS;
namespace Endicia
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Web.Services", "4.0.30319.1")]
	[WebServiceBinding(Name="EwsLabelServiceSoap", Namespace="www.envmgr.com/LabelService")]
	[XmlInclude(typeof(DataValidator))]
	public class EwsLabelService : SoapHttpClientProtocol
	{
		private SendOrPostCallback GetAccountStatusOperationCompleted;

		private SendOrPostCallback GetAccountStatusXMLOperationCompleted;

		private SendOrPostCallback CalculatePostageRateOperationCompleted;

		private SendOrPostCallback CalculatePostageRateXMLOperationCompleted;

		private SendOrPostCallback GetVersionOperationCompleted;

		private SendOrPostCallback GetVersionXMLOperationCompleted;

		private SendOrPostCallback CalculatePostageRatesOperationCompleted;

		private SendOrPostCallback CalculatePostageRatesXMLOperationCompleted;

		private SendOrPostCallback ChangePassPhraseOperationCompleted;

		private SendOrPostCallback ChangePassPhraseXMLOperationCompleted;

		private SendOrPostCallback BuyPostageOperationCompleted;

		private SendOrPostCallback BuyPostageXMLOperationCompleted;

		private SendOrPostCallback GetPostageLabelOperationCompleted;

		private SendOrPostCallback GetPostageLabelXMLOperationCompleted;

		private bool useDefaultCredentialsSetExplicitly;

		public new string Url
		{
			get
			{
				return base.Url;
			}
			set
			{
				if ((!this.IsLocalFileSystemWebService(base.Url) || this.useDefaultCredentialsSetExplicitly ? false : !this.IsLocalFileSystemWebService(value)))
				{
					base.UseDefaultCredentials = false;
				}
				base.Url = value;
			}
		}

		public new bool UseDefaultCredentials
		{
			get
			{
				return base.UseDefaultCredentials;
			}
			set
			{
				base.UseDefaultCredentials = value;
				this.useDefaultCredentialsSetExplicitly = true;
			}
		}

		public EwsLabelService()
		{
			this.Url = Settings.Default.Endicia_WS_EwsLabelService_Test;
			if (!this.IsLocalFileSystemWebService(this.Url))
			{
				this.useDefaultCredentialsSetExplicitly = true;
			}
			else
			{
				this.UseDefaultCredentials = true;
				this.useDefaultCredentialsSetExplicitly = false;
			}
		}

		[SoapDocumentMethod("www.envmgr.com/LabelService/BuyPostage", RequestNamespace="www.envmgr.com/LabelService", ResponseNamespace="www.envmgr.com/LabelService", Use=SoapBindingUse.Literal, ParameterStyle=SoapParameterStyle.Wrapped)]
		[return: XmlElement("RecreditRequestResponse")]
		public RecreditRequestResponse BuyPostage(RecreditRequest RecreditRequest)
		{
			object[] recreditRequest = new object[] { RecreditRequest };
			return (RecreditRequestResponse)base.Invoke("BuyPostage", recreditRequest)[0];
		}

		public void BuyPostageAsync(RecreditRequest RecreditRequest)
		{
			this.BuyPostageAsync(RecreditRequest, null);
		}

		public void BuyPostageAsync(RecreditRequest RecreditRequest, object userState)
		{
			if (this.BuyPostageOperationCompleted == null)
			{
				this.BuyPostageOperationCompleted = new SendOrPostCallback(this.OnBuyPostageOperationCompleted);
			}
			object[] recreditRequest = new object[] { RecreditRequest };
			base.InvokeAsync("BuyPostage", recreditRequest, this.BuyPostageOperationCompleted, userState);
		}

		[SoapDocumentMethod("www.envmgr.com/LabelService/BuyPostageXML", RequestNamespace="www.envmgr.com/LabelService", ResponseNamespace="www.envmgr.com/LabelService", Use=SoapBindingUse.Literal, ParameterStyle=SoapParameterStyle.Wrapped)]
		[return: XmlElement("RecreditRequestResponse")]
		public RecreditRequestResponse BuyPostageXML(string RecreditRequestXML)
		{
			object[] recreditRequestXML = new object[] { RecreditRequestXML };
			return (RecreditRequestResponse)base.Invoke("BuyPostageXML", recreditRequestXML)[0];
		}

		public void BuyPostageXMLAsync(string RecreditRequestXML)
		{
			this.BuyPostageXMLAsync(RecreditRequestXML, null);
		}

		public void BuyPostageXMLAsync(string RecreditRequestXML, object userState)
		{
			if (this.BuyPostageXMLOperationCompleted == null)
			{
				this.BuyPostageXMLOperationCompleted = new SendOrPostCallback(this.OnBuyPostageXMLOperationCompleted);
			}
			object[] recreditRequestXML = new object[] { RecreditRequestXML };
			base.InvokeAsync("BuyPostageXML", recreditRequestXML, this.BuyPostageXMLOperationCompleted, userState);
		}

		[SoapDocumentMethod("www.envmgr.com/LabelService/CalculatePostageRate", RequestNamespace="www.envmgr.com/LabelService", ResponseNamespace="www.envmgr.com/LabelService", Use=SoapBindingUse.Literal, ParameterStyle=SoapParameterStyle.Wrapped)]
		[return: XmlElement("PostageRateResponse")]
		public PostageRateResponse CalculatePostageRate(PostageRateRequest PostageRateRequest)
		{
			object[] postageRateRequest = new object[] { PostageRateRequest };
			return (PostageRateResponse)base.Invoke("CalculatePostageRate", postageRateRequest)[0];
		}

		public void CalculatePostageRateAsync(PostageRateRequest PostageRateRequest)
		{
			this.CalculatePostageRateAsync(PostageRateRequest, null);
		}

		public void CalculatePostageRateAsync(PostageRateRequest PostageRateRequest, object userState)
		{
			if (this.CalculatePostageRateOperationCompleted == null)
			{
				this.CalculatePostageRateOperationCompleted = new SendOrPostCallback(this.OnCalculatePostageRateOperationCompleted);
			}
			object[] postageRateRequest = new object[] { PostageRateRequest };
			base.InvokeAsync("CalculatePostageRate", postageRateRequest, this.CalculatePostageRateOperationCompleted, userState);
		}

		[SoapDocumentMethod("www.envmgr.com/LabelService/CalculatePostageRates", RequestNamespace="www.envmgr.com/LabelService", ResponseNamespace="www.envmgr.com/LabelService", Use=SoapBindingUse.Literal, ParameterStyle=SoapParameterStyle.Wrapped)]
		[return: XmlElement("PostageRatesResponse")]
		public PostageRatesResponse CalculatePostageRates(PostageRatesRequest PostageRatesRequest)
		{
			object[] postageRatesRequest = new object[] { PostageRatesRequest };
			return (PostageRatesResponse)base.Invoke("CalculatePostageRates", postageRatesRequest)[0];
		}

		public void CalculatePostageRatesAsync(PostageRatesRequest PostageRatesRequest)
		{
			this.CalculatePostageRatesAsync(PostageRatesRequest, null);
		}

		public void CalculatePostageRatesAsync(PostageRatesRequest PostageRatesRequest, object userState)
		{
			if (this.CalculatePostageRatesOperationCompleted == null)
			{
				this.CalculatePostageRatesOperationCompleted = new SendOrPostCallback(this.OnCalculatePostageRatesOperationCompleted);
			}
			object[] postageRatesRequest = new object[] { PostageRatesRequest };
			base.InvokeAsync("CalculatePostageRates", postageRatesRequest, this.CalculatePostageRatesOperationCompleted, userState);
		}

		[SoapDocumentMethod("www.envmgr.com/LabelService/CalculatePostageRatesXML", RequestNamespace="www.envmgr.com/LabelService", ResponseNamespace="www.envmgr.com/LabelService", Use=SoapBindingUse.Literal, ParameterStyle=SoapParameterStyle.Wrapped)]
		[return: XmlElement("PostageRatesResponse")]
		public PostageRatesResponse CalculatePostageRatesXML(string PostageRatesRequestXML)
		{
			object[] postageRatesRequestXML = new object[] { PostageRatesRequestXML };
			return (PostageRatesResponse)base.Invoke("CalculatePostageRatesXML", postageRatesRequestXML)[0];
		}

		public void CalculatePostageRatesXMLAsync(string PostageRatesRequestXML)
		{
			this.CalculatePostageRatesXMLAsync(PostageRatesRequestXML, null);
		}

		public void CalculatePostageRatesXMLAsync(string PostageRatesRequestXML, object userState)
		{
			if (this.CalculatePostageRatesXMLOperationCompleted == null)
			{
				this.CalculatePostageRatesXMLOperationCompleted = new SendOrPostCallback(this.OnCalculatePostageRatesXMLOperationCompleted);
			}
			object[] postageRatesRequestXML = new object[] { PostageRatesRequestXML };
			base.InvokeAsync("CalculatePostageRatesXML", postageRatesRequestXML, this.CalculatePostageRatesXMLOperationCompleted, userState);
		}

		[SoapDocumentMethod("www.envmgr.com/LabelService/CalculatePostageRateXML", RequestNamespace="www.envmgr.com/LabelService", ResponseNamespace="www.envmgr.com/LabelService", Use=SoapBindingUse.Literal, ParameterStyle=SoapParameterStyle.Wrapped)]
		[return: XmlElement("PostageRateResponse")]
		public PostageRateResponse CalculatePostageRateXML(string PostageRateRequestXML)
		{
			object[] postageRateRequestXML = new object[] { PostageRateRequestXML };
			return (PostageRateResponse)base.Invoke("CalculatePostageRateXML", postageRateRequestXML)[0];
		}

		public void CalculatePostageRateXMLAsync(string PostageRateRequestXML)
		{
			this.CalculatePostageRateXMLAsync(PostageRateRequestXML, null);
		}

		public void CalculatePostageRateXMLAsync(string PostageRateRequestXML, object userState)
		{
			if (this.CalculatePostageRateXMLOperationCompleted == null)
			{
				this.CalculatePostageRateXMLOperationCompleted = new SendOrPostCallback(this.OnCalculatePostageRateXMLOperationCompleted);
			}
			object[] postageRateRequestXML = new object[] { PostageRateRequestXML };
			base.InvokeAsync("CalculatePostageRateXML", postageRateRequestXML, this.CalculatePostageRateXMLOperationCompleted, userState);
		}

		public new void CancelAsync(object userState)
		{
			base.CancelAsync(userState);
		}

		[SoapDocumentMethod("www.envmgr.com/LabelService/ChangePassPhrase", RequestNamespace="www.envmgr.com/LabelService", ResponseNamespace="www.envmgr.com/LabelService", Use=SoapBindingUse.Literal, ParameterStyle=SoapParameterStyle.Wrapped)]
		[return: XmlElement("ChangePassPhraseRequestResponse")]
		public ChangePassPhraseRequestResponse ChangePassPhrase(ChangePassPhraseRequest ChangePassPhraseRequest)
		{
			object[] changePassPhraseRequest = new object[] { ChangePassPhraseRequest };
			return (ChangePassPhraseRequestResponse)base.Invoke("ChangePassPhrase", changePassPhraseRequest)[0];
		}

		public void ChangePassPhraseAsync(ChangePassPhraseRequest ChangePassPhraseRequest)
		{
			this.ChangePassPhraseAsync(ChangePassPhraseRequest, null);
		}

		public void ChangePassPhraseAsync(ChangePassPhraseRequest ChangePassPhraseRequest, object userState)
		{
			if (this.ChangePassPhraseOperationCompleted == null)
			{
				this.ChangePassPhraseOperationCompleted = new SendOrPostCallback(this.OnChangePassPhraseOperationCompleted);
			}
			object[] changePassPhraseRequest = new object[] { ChangePassPhraseRequest };
			base.InvokeAsync("ChangePassPhrase", changePassPhraseRequest, this.ChangePassPhraseOperationCompleted, userState);
		}

		[SoapDocumentMethod("www.envmgr.com/LabelService/ChangePassPhraseXML", RequestNamespace="www.envmgr.com/LabelService", ResponseNamespace="www.envmgr.com/LabelService", Use=SoapBindingUse.Literal, ParameterStyle=SoapParameterStyle.Wrapped)]
		[return: XmlElement("ChangePassPhraseRequestResponse")]
		public ChangePassPhraseRequestResponse ChangePassPhraseXML(string ChangePassPhraseRequestXML)
		{
			object[] changePassPhraseRequestXML = new object[] { ChangePassPhraseRequestXML };
			return (ChangePassPhraseRequestResponse)base.Invoke("ChangePassPhraseXML", changePassPhraseRequestXML)[0];
		}

		public void ChangePassPhraseXMLAsync(string ChangePassPhraseRequestXML)
		{
			this.ChangePassPhraseXMLAsync(ChangePassPhraseRequestXML, null);
		}

		public void ChangePassPhraseXMLAsync(string ChangePassPhraseRequestXML, object userState)
		{
			if (this.ChangePassPhraseXMLOperationCompleted == null)
			{
				this.ChangePassPhraseXMLOperationCompleted = new SendOrPostCallback(this.OnChangePassPhraseXMLOperationCompleted);
			}
			object[] changePassPhraseRequestXML = new object[] { ChangePassPhraseRequestXML };
			base.InvokeAsync("ChangePassPhraseXML", changePassPhraseRequestXML, this.ChangePassPhraseXMLOperationCompleted, userState);
		}

		[SoapDocumentMethod("www.envmgr.com/LabelService/GetAccountStatus", RequestNamespace="www.envmgr.com/LabelService", ResponseNamespace="www.envmgr.com/LabelService", Use=SoapBindingUse.Literal, ParameterStyle=SoapParameterStyle.Wrapped)]
		[return: XmlElement("AccountStatusResponse")]
		public AccountStatusResponse GetAccountStatus(AccountStatusRequest AccountStatusRequest)
		{
			object[] accountStatusRequest = new object[] { AccountStatusRequest };
			return (AccountStatusResponse)base.Invoke("GetAccountStatus", accountStatusRequest)[0];
		}

		public void GetAccountStatusAsync(AccountStatusRequest AccountStatusRequest)
		{
			this.GetAccountStatusAsync(AccountStatusRequest, null);
		}

		public void GetAccountStatusAsync(AccountStatusRequest AccountStatusRequest, object userState)
		{
			if (this.GetAccountStatusOperationCompleted == null)
			{
				this.GetAccountStatusOperationCompleted = new SendOrPostCallback(this.OnGetAccountStatusOperationCompleted);
			}
			object[] accountStatusRequest = new object[] { AccountStatusRequest };
			base.InvokeAsync("GetAccountStatus", accountStatusRequest, this.GetAccountStatusOperationCompleted, userState);
		}

		[SoapDocumentMethod("www.envmgr.com/LabelService/GetAccountStatusXML", RequestNamespace="www.envmgr.com/LabelService", ResponseNamespace="www.envmgr.com/LabelService", Use=SoapBindingUse.Literal, ParameterStyle=SoapParameterStyle.Wrapped)]
		[return: XmlElement("AccountStatusResponse")]
		public AccountStatusResponse GetAccountStatusXML(string AccountStatusRequestXML)
		{
			object[] accountStatusRequestXML = new object[] { AccountStatusRequestXML };
			return (AccountStatusResponse)base.Invoke("GetAccountStatusXML", accountStatusRequestXML)[0];
		}

		public void GetAccountStatusXMLAsync(string AccountStatusRequestXML)
		{
			this.GetAccountStatusXMLAsync(AccountStatusRequestXML, null);
		}

		public void GetAccountStatusXMLAsync(string AccountStatusRequestXML, object userState)
		{
			if (this.GetAccountStatusXMLOperationCompleted == null)
			{
				this.GetAccountStatusXMLOperationCompleted = new SendOrPostCallback(this.OnGetAccountStatusXMLOperationCompleted);
			}
			object[] accountStatusRequestXML = new object[] { AccountStatusRequestXML };
			base.InvokeAsync("GetAccountStatusXML", accountStatusRequestXML, this.GetAccountStatusXMLOperationCompleted, userState);
		}

		[SoapDocumentMethod("www.envmgr.com/LabelService/GetPostageLabel", RequestNamespace="www.envmgr.com/LabelService", ResponseNamespace="www.envmgr.com/LabelService", Use=SoapBindingUse.Literal, ParameterStyle=SoapParameterStyle.Wrapped)]
		[return: XmlElement("LabelRequestResponse")]
		public LabelRequestResponse GetPostageLabel(LabelRequest LabelRequest)
		{
			object[] labelRequest = new object[] { LabelRequest };
			return (LabelRequestResponse)base.Invoke("GetPostageLabel", labelRequest)[0];
		}

		public void GetPostageLabelAsync(LabelRequest LabelRequest)
		{
			this.GetPostageLabelAsync(LabelRequest, null);
		}

		public void GetPostageLabelAsync(LabelRequest LabelRequest, object userState)
		{
			if (this.GetPostageLabelOperationCompleted == null)
			{
				this.GetPostageLabelOperationCompleted = new SendOrPostCallback(this.OnGetPostageLabelOperationCompleted);
			}
			object[] labelRequest = new object[] { LabelRequest };
			base.InvokeAsync("GetPostageLabel", labelRequest, this.GetPostageLabelOperationCompleted, userState);
		}

		[SoapDocumentMethod("www.envmgr.com/LabelService/GetPostageLabelXML", RequestNamespace="www.envmgr.com/LabelService", ResponseNamespace="www.envmgr.com/LabelService", Use=SoapBindingUse.Literal, ParameterStyle=SoapParameterStyle.Wrapped)]
		[return: XmlElement("LabelRequestResponse")]
		public LabelRequestResponse GetPostageLabelXML(string LabelRequestXML)
		{
			object[] labelRequestXML = new object[] { LabelRequestXML };
			return (LabelRequestResponse)base.Invoke("GetPostageLabelXML", labelRequestXML)[0];
		}

		public void GetPostageLabelXMLAsync(string LabelRequestXML)
		{
			this.GetPostageLabelXMLAsync(LabelRequestXML, null);
		}

		public void GetPostageLabelXMLAsync(string LabelRequestXML, object userState)
		{
			if (this.GetPostageLabelXMLOperationCompleted == null)
			{
				this.GetPostageLabelXMLOperationCompleted = new SendOrPostCallback(this.OnGetPostageLabelXMLOperationCompleted);
			}
			object[] labelRequestXML = new object[] { LabelRequestXML };
			base.InvokeAsync("GetPostageLabelXML", labelRequestXML, this.GetPostageLabelXMLOperationCompleted, userState);
		}

		[SoapDocumentMethod("www.envmgr.com/LabelService/GetVersion", RequestNamespace="www.envmgr.com/LabelService", ResponseNamespace="www.envmgr.com/LabelService", Use=SoapBindingUse.Literal, ParameterStyle=SoapParameterStyle.Wrapped)]
		[return: XmlElement("GetVersionResults")]
		public GetVersionResults GetVersion()
		{
			object[] results = base.Invoke("GetVersion", new object[0]);
			return (GetVersionResults)results[0];
		}

		public void GetVersionAsync()
		{
			this.GetVersionAsync(null);
		}

		public void GetVersionAsync(object userState)
		{
			if (this.GetVersionOperationCompleted == null)
			{
				this.GetVersionOperationCompleted = new SendOrPostCallback(this.OnGetVersionOperationCompleted);
			}
			base.InvokeAsync("GetVersion", new object[0], this.GetVersionOperationCompleted, userState);
		}

		[SoapDocumentMethod("www.envmgr.com/LabelService/GetVersionXML", RequestNamespace="www.envmgr.com/LabelService", ResponseNamespace="www.envmgr.com/LabelService", Use=SoapBindingUse.Literal, ParameterStyle=SoapParameterStyle.Wrapped)]
		[return: XmlElement("GetVersionResults")]
		public GetVersionResults GetVersionXML()
		{
			object[] results = base.Invoke("GetVersionXML", new object[0]);
			return (GetVersionResults)results[0];
		}

		public void GetVersionXMLAsync()
		{
			this.GetVersionXMLAsync(null);
		}

		public void GetVersionXMLAsync(object userState)
		{
			if (this.GetVersionXMLOperationCompleted == null)
			{
				this.GetVersionXMLOperationCompleted = new SendOrPostCallback(this.OnGetVersionXMLOperationCompleted);
			}
			base.InvokeAsync("GetVersionXML", new object[0], this.GetVersionXMLOperationCompleted, userState);
		}

		private bool IsLocalFileSystemWebService(string url)
		{
			bool flag;
			if ((url == null ? false : !(url == string.Empty)))
			{
				System.Uri wsUri = new System.Uri(url);
				flag = ((wsUri.Port < 1024 ? true : string.Compare(wsUri.Host, "localHost", StringComparison.OrdinalIgnoreCase) != 0) ? false : true);
			}
			else
			{
				flag = false;
			}
			return flag;
		}

		private void OnBuyPostageOperationCompleted(object arg)
		{
			if (this.BuyPostageCompleted != null)
			{
				InvokeCompletedEventArgs invokeArgs = (InvokeCompletedEventArgs)arg;
				this.BuyPostageCompleted(this, new BuyPostageCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
			}
		}

		private void OnBuyPostageXMLOperationCompleted(object arg)
		{
			if (this.BuyPostageXMLCompleted != null)
			{
				InvokeCompletedEventArgs invokeArgs = (InvokeCompletedEventArgs)arg;
				this.BuyPostageXMLCompleted(this, new BuyPostageXMLCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
			}
		}

		private void OnCalculatePostageRateOperationCompleted(object arg)
		{
			if (this.CalculatePostageRateCompleted != null)
			{
				InvokeCompletedEventArgs invokeArgs = (InvokeCompletedEventArgs)arg;
				this.CalculatePostageRateCompleted(this, new CalculatePostageRateCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
			}
		}

		private void OnCalculatePostageRatesOperationCompleted(object arg)
		{
			if (this.CalculatePostageRatesCompleted != null)
			{
				InvokeCompletedEventArgs invokeArgs = (InvokeCompletedEventArgs)arg;
				this.CalculatePostageRatesCompleted(this, new CalculatePostageRatesCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
			}
		}

		private void OnCalculatePostageRatesXMLOperationCompleted(object arg)
		{
			if (this.CalculatePostageRatesXMLCompleted != null)
			{
				InvokeCompletedEventArgs invokeArgs = (InvokeCompletedEventArgs)arg;
				this.CalculatePostageRatesXMLCompleted(this, new CalculatePostageRatesXMLCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
			}
		}

		private void OnCalculatePostageRateXMLOperationCompleted(object arg)
		{
			if (this.CalculatePostageRateXMLCompleted != null)
			{
				InvokeCompletedEventArgs invokeArgs = (InvokeCompletedEventArgs)arg;
				this.CalculatePostageRateXMLCompleted(this, new CalculatePostageRateXMLCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
			}
		}

		private void OnChangePassPhraseOperationCompleted(object arg)
		{
			if (this.ChangePassPhraseCompleted != null)
			{
				InvokeCompletedEventArgs invokeArgs = (InvokeCompletedEventArgs)arg;
				this.ChangePassPhraseCompleted(this, new ChangePassPhraseCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
			}
		}

		private void OnChangePassPhraseXMLOperationCompleted(object arg)
		{
			if (this.ChangePassPhraseXMLCompleted != null)
			{
				InvokeCompletedEventArgs invokeArgs = (InvokeCompletedEventArgs)arg;
				this.ChangePassPhraseXMLCompleted(this, new ChangePassPhraseXMLCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
			}
		}

		private void OnGetAccountStatusOperationCompleted(object arg)
		{
			if (this.GetAccountStatusCompleted != null)
			{
				InvokeCompletedEventArgs invokeArgs = (InvokeCompletedEventArgs)arg;
				this.GetAccountStatusCompleted(this, new GetAccountStatusCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
			}
		}

		private void OnGetAccountStatusXMLOperationCompleted(object arg)
		{
			if (this.GetAccountStatusXMLCompleted != null)
			{
				InvokeCompletedEventArgs invokeArgs = (InvokeCompletedEventArgs)arg;
				this.GetAccountStatusXMLCompleted(this, new GetAccountStatusXMLCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
			}
		}

		private void OnGetPostageLabelOperationCompleted(object arg)
		{
			if (this.GetPostageLabelCompleted != null)
			{
				InvokeCompletedEventArgs invokeArgs = (InvokeCompletedEventArgs)arg;
				this.GetPostageLabelCompleted(this, new GetPostageLabelCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
			}
		}

		private void OnGetPostageLabelXMLOperationCompleted(object arg)
		{
			if (this.GetPostageLabelXMLCompleted != null)
			{
				InvokeCompletedEventArgs invokeArgs = (InvokeCompletedEventArgs)arg;
				this.GetPostageLabelXMLCompleted(this, new GetPostageLabelXMLCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
			}
		}

		private void OnGetVersionOperationCompleted(object arg)
		{
			if (this.GetVersionCompleted != null)
			{
				InvokeCompletedEventArgs invokeArgs = (InvokeCompletedEventArgs)arg;
				this.GetVersionCompleted(this, new GetVersionCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
			}
		}

		private void OnGetVersionXMLOperationCompleted(object arg)
		{
			if (this.GetVersionXMLCompleted != null)
			{
				InvokeCompletedEventArgs invokeArgs = (InvokeCompletedEventArgs)arg;
				this.GetVersionXMLCompleted(this, new GetVersionXMLCompletedEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
			}
		}

		public event BuyPostageCompletedEventHandler BuyPostageCompleted;

		public event BuyPostageXMLCompletedEventHandler BuyPostageXMLCompleted;

		public event CalculatePostageRateCompletedEventHandler CalculatePostageRateCompleted;

		public event CalculatePostageRatesCompletedEventHandler CalculatePostageRatesCompleted;

		public event CalculatePostageRatesXMLCompletedEventHandler CalculatePostageRatesXMLCompleted;

		public event CalculatePostageRateXMLCompletedEventHandler CalculatePostageRateXMLCompleted;

		public event ChangePassPhraseCompletedEventHandler ChangePassPhraseCompleted;

		public event ChangePassPhraseXMLCompletedEventHandler ChangePassPhraseXMLCompleted;

		public event GetAccountStatusCompletedEventHandler GetAccountStatusCompleted;

		public event GetAccountStatusXMLCompletedEventHandler GetAccountStatusXMLCompleted;

		public event GetPostageLabelCompletedEventHandler GetPostageLabelCompleted;

		public event GetPostageLabelXMLCompletedEventHandler GetPostageLabelXMLCompleted;

		public event GetVersionCompletedEventHandler GetVersionCompleted;

		public event GetVersionXMLCompletedEventHandler GetVersionXMLCompleted;
        //MMT
        [SoapDocumentMethod("www.envmgr.com/LabelService/GetChallengeQuestionXML", RequestNamespace = "www.envmgr.com/LabelService", ResponseNamespace = "www.envmgr.com/LabelService", Use = SoapBindingUse.Literal, ParameterStyle = SoapParameterStyle.Wrapped)]
        [return: XmlElement("ChallengeQuestionResponse")]
        public ChallengeQuestionResponse ChallengeQuestion(ChallengeRequest ChallengeRequest)
        {
            object[] challangeRequest = new object[] { ChallengeRequest };
            return (ChallengeQuestionResponse)base.Invoke("GetChallengeQuestionXML", challangeRequest)[0];
        }

        public void ChallengeQuestionAsync(ChallengeRequest challengeRequest)
        {
            this.ChallengeQuestionAsync(challengeRequest, null);
        }

        public void ChallengeQuestionAsync(ChallengeRequest challengeRequest, object userState)
        {
            if (this.ChallangeRequestXMLOperationCompleted == null)
            {
                this.ChallangeRequestXMLOperationCompleted = new SendOrPostCallback(this.OnChangePassPhraseOperationCompleted);
            }
            object[] challengeRequestobject = new object[] { challengeRequest };
            base.InvokeAsync("GetChallengeQuestionXML", challengeRequestobject, this.ChangePassPhraseOperationCompleted, userState);
        }

        [SoapDocumentMethod("www.envmgr.com/LabelService/GetChallengeQuestionXML", RequestNamespace = "www.envmgr.com/LabelService", ResponseNamespace = "www.envmgr.com/LabelService", Use = SoapBindingUse.Literal, ParameterStyle = SoapParameterStyle.Wrapped)]
        [return: XmlElement("ChangePassPhraseRequestResponse")]
        public ChallengeQuestionResponse GetChallengeQuestionXML(string GetChallengeQuestionXML)
        {
            object[] changePassPhraseRequestXML = new object[] { GetChallengeQuestionXML };
            return (ChallengeQuestionResponse)base.Invoke("GetChallengeQuestionXML", changePassPhraseRequestXML)[0];
        }

        public void GetChallengeQuestionXMLAsync(string GetChallengeQuestionXML)
        {
            this.GetChallengeQuestionXMLAsync(GetChallengeQuestionXML, null);
        }

        public void GetChallengeQuestionXMLAsync(string GetChallengeQuestionXML, object userState)
        {
            if (this.ChallangeRequestXMLOperationCompleted == null)
            {
                this.ChallangeRequestXMLOperationCompleted = new SendOrPostCallback(this.OnChallangeRequestXMLOperationCompleted);
            }
            object[] challangeRequesttXML = new object[] { GetChallengeQuestionXML };
            base.InvokeAsync("GetChallengeQuestionXML", challangeRequesttXML, this.ChallangeRequestXMLOperationCompleted, userState);
        }
        private SendOrPostCallback ChallangeRequestXMLOperationCompleted;
        private void OnChallangeRequestXMLOperationCompleted(object arg)
        {
            if (this.ChallangeRequestXMLCompleted != null)
            {
                InvokeCompletedEventArgs invokeArgs = (InvokeCompletedEventArgs)arg;
                this.ChallangeRequestXMLCompleted(this, new ChallangeQuestionRequestEventArgs(invokeArgs.Results, invokeArgs.Error, invokeArgs.Cancelled, invokeArgs.UserState));
            }
        }
        public event ChallangeQuestionXMLCompletedEventHandler ChallangeRequestXMLCompleted;
        //MMT

    }
}