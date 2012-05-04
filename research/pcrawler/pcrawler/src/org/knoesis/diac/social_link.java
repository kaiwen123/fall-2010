package org.knoesis.diac;

/**
 * A class to represent the social link. 
 * @author Simon Guo.
 * @version 1.0
 */
class social_link {
	private String uid; 
	private String fromid; 
	private String desc; 
	
	social_link(String uid, String fromid, String desc) {
		this.uid = uid; 
		this.fromid = fromid; 
		this.desc = desc;
	}
	
	public String getUid() {
		return uid; 
	}
	public String getFromID() {
		return fromid; 
	}
	public String getDesc () {
		return desc; 
	}
}
