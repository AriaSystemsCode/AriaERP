using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;

namespace Fedex.FedexTrack
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Xml", "4.0.30319.1")]
	[Serializable]
	[XmlType(Namespace="http://fedex.com/ws/track/v4")]
	public class Contact
	{
		private string personNameField;

		private string titleField;

		private string companyNameField;

		private string phoneNumberField;

		private string phoneExtensionField;

		private string pagerNumberField;

		private string faxNumberField;

		private string eMailAddressField;

		public string CompanyName
		{
			get
			{
				return this.companyNameField;
			}
			set
			{
				this.companyNameField = value;
			}
		}

		public string EMailAddress
		{
			get
			{
				return this.eMailAddressField;
			}
			set
			{
				this.eMailAddressField = value;
			}
		}

		public string FaxNumber
		{
			get
			{
				return this.faxNumberField;
			}
			set
			{
				this.faxNumberField = value;
			}
		}

		public string PagerNumber
		{
			get
			{
				return this.pagerNumberField;
			}
			set
			{
				this.pagerNumberField = value;
			}
		}

		public string PersonName
		{
			get
			{
				return this.personNameField;
			}
			set
			{
				this.personNameField = value;
			}
		}

		public string PhoneExtension
		{
			get
			{
				return this.phoneExtensionField;
			}
			set
			{
				this.phoneExtensionField = value;
			}
		}

		public string PhoneNumber
		{
			get
			{
				return this.phoneNumberField;
			}
			set
			{
				this.phoneNumberField = value;
			}
		}

		public string Title
		{
			get
			{
				return this.titleField;
			}
			set
			{
				this.titleField = value;
			}
		}

		public Contact()
		{
		}
	}
}