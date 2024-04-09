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
	public class Address
	{
		private string[] streetLinesField;

		private string cityField;

		private string stateOrProvinceCodeField;

		private string postalCodeField;

		private string urbanizationCodeField;

		private string countryCodeField;

		private bool residentialField;

		private bool residentialFieldSpecified;

		public string City
		{
			get
			{
				return this.cityField;
			}
			set
			{
				this.cityField = value;
			}
		}

		public string CountryCode
		{
			get
			{
				return this.countryCodeField;
			}
			set
			{
				this.countryCodeField = value;
			}
		}

		public string PostalCode
		{
			get
			{
				return this.postalCodeField;
			}
			set
			{
				this.postalCodeField = value;
			}
		}

		public bool Residential
		{
			get
			{
				return this.residentialField;
			}
			set
			{
				this.residentialField = value;
			}
		}

		[XmlIgnore]
		public bool ResidentialSpecified
		{
			get
			{
				return this.residentialFieldSpecified;
			}
			set
			{
				this.residentialFieldSpecified = value;
			}
		}

		public string StateOrProvinceCode
		{
			get
			{
				return this.stateOrProvinceCodeField;
			}
			set
			{
				this.stateOrProvinceCodeField = value;
			}
		}

		[XmlElement("StreetLines")]
		public string[] StreetLines
		{
			get
			{
				return this.streetLinesField;
			}
			set
			{
				this.streetLinesField = value;
			}
		}

		public string UrbanizationCode
		{
			get
			{
				return this.urbanizationCodeField;
			}
			set
			{
				this.urbanizationCodeField = value;
			}
		}

		public Address()
		{
		}
	}
}