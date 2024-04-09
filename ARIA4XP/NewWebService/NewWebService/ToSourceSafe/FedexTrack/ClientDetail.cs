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
	public class ClientDetail
	{
		private string accountNumberField;

		private string meterNumberField;

		private string integratorIdField;

		private  Localization localizationField;

		public string AccountNumber
		{
			get
			{
				return this.accountNumberField;
			}
			set
			{
				this.accountNumberField = value;
			}
		}

		public string IntegratorId
		{
			get
			{
				return this.integratorIdField;
			}
			set
			{
				this.integratorIdField = value;
			}
		}

		public  Localization Localization
		{
			get
			{
				return this.localizationField;
			}
			set
			{
				this.localizationField = value;
			}
		}

		public string MeterNumber
		{
			get
			{
				return this.meterNumberField;
			}
			set
			{
				this.meterNumberField = value;
			}
		}

		public ClientDetail()
		{
		}
	}
}