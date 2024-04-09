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
	public class TrackRequest
	{
		private  WebAuthenticationDetail webAuthenticationDetailField;

		private  ClientDetail clientDetailField;

		private  TransactionDetail transactionDetailField;

		private VersionId versionField;

		private CarrierCodeType carrierCodeField;

		private bool carrierCodeFieldSpecified;

		private TrackPackageIdentifier packageIdentifierField;

		private string trackingNumberUniqueIdentifierField;

		private DateTime shipDateRangeBeginField;

		private bool shipDateRangeBeginFieldSpecified;

		private DateTime shipDateRangeEndField;

		private bool shipDateRangeEndFieldSpecified;

		private string shipmentAccountNumberField;

		private Address destinationField;

		private bool includeDetailedScansField;

		private bool includeDetailedScansFieldSpecified;

		private string pagingTokenField;

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

		public  ClientDetail ClientDetail
		{
			get
			{
				return this.clientDetailField;
			}
			set
			{
				this.clientDetailField = value;
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

		public bool IncludeDetailedScans
		{
			get
			{
				return this.includeDetailedScansField;
			}
			set
			{
				this.includeDetailedScansField = value;
			}
		}

		[XmlIgnore]
		public bool IncludeDetailedScansSpecified
		{
			get
			{
				return this.includeDetailedScansFieldSpecified;
			}
			set
			{
				this.includeDetailedScansFieldSpecified = value;
			}
		}

		public TrackPackageIdentifier PackageIdentifier
		{
			get
			{
				return this.packageIdentifierField;
			}
			set
			{
				this.packageIdentifierField = value;
			}
		}

		public string PagingToken
		{
			get
			{
				return this.pagingTokenField;
			}
			set
			{
				this.pagingTokenField = value;
			}
		}

		[XmlElement(DataType="date")]
		public DateTime ShipDateRangeBegin
		{
			get
			{
				return this.shipDateRangeBeginField;
			}
			set
			{
				this.shipDateRangeBeginField = value;
			}
		}

		[XmlIgnore]
		public bool ShipDateRangeBeginSpecified
		{
			get
			{
				return this.shipDateRangeBeginFieldSpecified;
			}
			set
			{
				this.shipDateRangeBeginFieldSpecified = value;
			}
		}

		[XmlElement(DataType="date")]
		public DateTime ShipDateRangeEnd
		{
			get
			{
				return this.shipDateRangeEndField;
			}
			set
			{
				this.shipDateRangeEndField = value;
			}
		}

		[XmlIgnore]
		public bool ShipDateRangeEndSpecified
		{
			get
			{
				return this.shipDateRangeEndFieldSpecified;
			}
			set
			{
				this.shipDateRangeEndFieldSpecified = value;
			}
		}

		public string ShipmentAccountNumber
		{
			get
			{
				return this.shipmentAccountNumberField;
			}
			set
			{
				this.shipmentAccountNumberField = value;
			}
		}

		public string TrackingNumberUniqueIdentifier
		{
			get
			{
				return this.trackingNumberUniqueIdentifierField;
			}
			set
			{
				this.trackingNumberUniqueIdentifierField = value;
			}
		}

		public  TransactionDetail TransactionDetail
		{
			get
			{
				return this.transactionDetailField;
			}
			set
			{
				this.transactionDetailField = value;
			}
		}

		public VersionId Version
		{
			get
			{
				return this.versionField;
			}
			set
			{
				this.versionField = value;
			}
		}

		public  WebAuthenticationDetail WebAuthenticationDetail
		{
			get
			{
				return this.webAuthenticationDetailField;
			}
			set
			{
				this.webAuthenticationDetailField = value;
			}
		}

		public TrackRequest()
		{
		}
	}
}