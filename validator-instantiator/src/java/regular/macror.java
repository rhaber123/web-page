// Author: René Haberland
// 2007/30.12.2018, Saint Petersburg, Russia
//  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
//  For more informations on the license terms, visit https://creativecommons.org

package Regular;

public class MacroR extends RegEx {
	private String callee;
	private RegEx hedge;

	public String getCallee() {
		return callee;
	}

	public void setCallee(String callee) {
		this.callee = callee;
	}

	public RegEx getHedge() {
		return hedge;
	}

	public void setHedge(RegEx hedge) {
		this.hedge = hedge;
	}

	// private univeral constructor
	private void create(String callee, RegEx hedge){
		this.callee = callee;
		this.hedge = hedge;
	}

	public MacroR(String callee) {
		create(callee,new Epsilon());
	}
	
	public MacroR(String callee, RegEx hedge){
		create(callee, hedge);
	}
	
	public boolean validate(RegEx expression){
		return false;
	}
	
	public int getIdentifier()
	{
		return MACROCALL;
	}
}
