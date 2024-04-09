using System;
using System.CodeDom.Compiler;
using System.Xml.Serialization;

namespace Fedex.FedexTrack
{
	[GeneratedCode("System.Xml", "4.0.30319.1")]
	[Serializable]
	[XmlType(Namespace="http://fedex.com/ws/track/v4")]
	public enum TrackDeliveryLocationType
	{
		FEDEX_LOCATION,
		GUARD_OR_SECURITY_STATION,
		IN_BOND_OR_CAGE,
		MAILROOM,
		OTHER,
		PHARMACY,
		RECEPTIONIST_OR_FRONT_DESK,
		RESIDENCE,
		SHIPPING_RECEIVING
	}
}