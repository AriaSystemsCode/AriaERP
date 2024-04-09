using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;

namespace Endicia.WS
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Xml", "4.0.30319.1")]
	[Serializable]
	[XmlType(Namespace="www.envmgr.com/LabelService")]
	public class PostageRatesRequest : DataValidator
	{
		private string requesterIDField;

		private CertifiedIntermediary certifiedIntermediaryField;

		private string mailClassField;

		private double weightOzField;

		private string mailpieceShapeField;

		private Dimensions mailpieceDimensionsField;

		private string machinableField;

		private string serviceLevelField;

		private SpecialServices servicesField;

		private double cODAmountField;

		private double insuredValueField;

		private double registeredMailValueField;

		private string fromPostalCodeField;

		private string toPostalCodeField;

		private string toCountryField;

		public CertifiedIntermediary CertifiedIntermediary
		{
			get
			{
				return this.certifiedIntermediaryField;
			}
			set
			{
				this.certifiedIntermediaryField = value;
			}
		}

		public double CODAmount
		{
			get
			{
				return this.cODAmountField;
			}
			set
			{
				this.cODAmountField = value;
			}
		}

		public string FromPostalCode
		{
			get
			{
				return this.fromPostalCodeField;
			}
			set
			{
				this.fromPostalCodeField = value;
			}
		}

		public double InsuredValue
		{
			get
			{
				return this.insuredValueField;
			}
			set
			{
				this.insuredValueField = value;
			}
		}

		public string Machinable
		{
			get
			{
				return this.machinableField;
			}
			set
			{
				this.machinableField = value;
			}
		}

		public string MailClass
		{
			get
			{
				return this.mailClassField;
			}
			set
			{
				this.mailClassField = value;
			}
		}

		public Dimensions MailpieceDimensions
		{
			get
			{
				return this.mailpieceDimensionsField;
			}
			set
			{
				this.mailpieceDimensionsField = value;
			}
		}

		public string MailpieceShape
		{
			get
			{
				return this.mailpieceShapeField;
			}
			set
			{
				this.mailpieceShapeField = value;
			}
		}

		public double RegisteredMailValue
		{
			get
			{
				return this.registeredMailValueField;
			}
			set
			{
				this.registeredMailValueField = value;
			}
		}

		public string RequesterID
		{
			get
			{
				return this.requesterIDField;
			}
			set
			{
				this.requesterIDField = value;
			}
		}

		public string ServiceLevel
		{
			get
			{
				return this.serviceLevelField;
			}
			set
			{
				this.serviceLevelField = value;
			}
		}

		public SpecialServices Services
		{
			get
			{
				return this.servicesField;
			}
			set
			{
				this.servicesField = value;
			}
		}

		public string ToCountry
		{
			get
			{
				return this.toCountryField;
			}
			set
			{
				this.toCountryField = value;
			}
		}

		public string ToPostalCode
		{
			get
			{
				return this.toPostalCodeField;
			}
			set
			{
				this.toPostalCodeField = value;
			}
		}

		public double WeightOz
		{
			get
			{
				return this.weightOzField;
			}
			set
			{
				this.weightOzField = value;
			}
		}

		public PostageRatesRequest()
		{
		}
	}
}