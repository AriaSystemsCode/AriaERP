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
	public class TrackNotificationPackage
	{
		private string trackingNumberField;

		private string trackingNumberUniqueIdentifiersField;

		private CarrierCodeType carrierCodeField;

		private bool carrierCodeFieldSpecified;

		private DateTime shipDateField;

		private bool shipDateFieldSpecified;

		private Address destinationField;

		private TrackNotificationRecipientDetail[] recipientDetailsField;

		public CarrierCodeType CarrierCode
		{
			get
			{
				return this.carrierCodeField;
			}
			set
			{
				this.carrierCodeField = value;
			}
		}

		[XmlIgnore]
		public bool CarrierCodeSpecified
		{
			get
			{
				return this.carrierCodeFieldSpecified;
			}
			set
			{
				this.carrierCodeFieldSpecified = value;
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

		[XmlElement("RecipientDetails")]
		public TrackNotificationRecipientDetail[] RecipientDetails
		{
			get
			{
				return this.recipientDetailsField;
			}
			set
			{
				this.recipientDetailsField = value;
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

		public string TrackingNumberUniqueIdentifiers
		{
			get
			{
				return this.trackingNumberUniqueIdentifiersField;
			}
			set
			{
				this.trackingNumberUniqueIdentifiersField = value;
			}
		}

		public TrackNotificationPackage()
		{
		}
	}
}