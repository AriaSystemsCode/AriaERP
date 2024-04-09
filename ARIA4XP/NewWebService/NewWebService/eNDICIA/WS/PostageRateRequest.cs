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
	public class PostageRateRequest : DataValidator
	{
		private string requesterIDField;

		private CertifiedIntermediary certifiedIntermediaryField;

		private string mailClassField;

		private string pricingField;

		private double weightOzField;

		private string mailpieceShapeField;

		private Dimensions mailpieceDimensionsField;

		private string automationRateField;

		private string machinableField;

		private string serviceLevelField;

		private string sundayHolidayDeliveryField;

		private string sortTypeField;

		private SpecialServices servicesField;

		private float valueField;

		private double cODAmountField;

		private string insuredValueField;

		private double registeredMailValueField;

		private string entryFacilityField;

		private string fromPostalCodeField;

		private string toPostalCodeField;

		private string toCountryField;

		private string shipDateField;

		private string shipTimeField;

		private ResponseOptions responseOptionsField;

		public string AutomationRate
		{
			get
			{
				return this.automationRateField;
			}
			set
			{
				this.automationRateField = value;
			}
		}

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

		[DefaultValue(0)]
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

		public string EntryFacility
		{
			get
			{
				return this.entryFacilityField;
			}
			set
			{
				this.entryFacilityField = value;
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

		public string InsuredValue
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

		public string Pricing
		{
			get
			{
				return this.pricingField;
			}
			set
			{
				this.pricingField = value;
			}
		}

		[DefaultValue(0)]
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

		public ResponseOptions ResponseOptions
		{
			get
			{
				return this.responseOptionsField;
			}
			set
			{
				this.responseOptionsField = value;
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

		public string ShipDate
		{
			get
			{
				return this.shipDateField;
			}
			set
			{
				this.shipDateField = value;
			}
		}

		public string ShipTime
		{
			get
			{
				return this.shipTimeField;
			}
			set
			{
				this.shipTimeField = value;
			}
		}

		public string SortType
		{
			get
			{
				return this.sortTypeField;
			}
			set
			{
				this.sortTypeField = value;
			}
		}

		public string SundayHolidayDelivery
		{
			get
			{
				return this.sundayHolidayDeliveryField;
			}
			set
			{
				this.sundayHolidayDeliveryField = value;
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

		public float Value
		{
			get
			{
				return this.valueField;
			}
			set
			{
				this.valueField = value;
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

		public PostageRateRequest()
		{
			this.cODAmountField = 0;
			this.registeredMailValueField = 0;
		}
	}
}