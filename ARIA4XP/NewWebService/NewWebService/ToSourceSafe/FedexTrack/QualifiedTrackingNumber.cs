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
	public class QualifiedTrackingNumber
	{
		private string trackingNumberField;

		private DateTime shipDateField;

		private bool shipDateFieldSpecified;

		private string accountNumberField;

		private CarrierCodeType carrierField;

		private bool carrierFieldSpecified;

		private Address destinationField;

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

		public CarrierCodeType Carrier
		{
			get
			{
				return this.carrierField;
			}
			set
			{
				this.carrierField = value;
			}
		}

		[XmlIgnore]
		public bool CarrierSpecified
		{
			get
			{
				return this.carrierFieldSpecified;
			}
			set
			{
				this.carrierFieldSpecified = value;
			}
		}

		public Address Destination
		{
			get
			{
				return this.destinationField;
			}
			set
			{
				this.destinationField = value;
			}
		}

		[XmlElement(DataType="date")]
		public DateTime ShipDate
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

		[XmlIgnore]
		public bool ShipDateSpecified
		{
			get
			{
				return this.shipDateFieldSpecified;
			}
			set
			{
				this.shipDateFieldSpecified = value;
			}
		}

		public string TrackingNumber
		{
			get
			{
				return this.trackingNumberField;
			}
			set
			{
				this.trackingNumberField = value;
			}
		}

		public QualifiedTrackingNumber()
		{
		}
	}
}