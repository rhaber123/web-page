// Author: René Haberland
// 2007/30.12.2018, Saint Petersburg, Russia
//  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
//  For more informations on the license terms, visit https://creativecommons.org

package Regular;

public class TextR extends RegEx {
	
	private String select;
	 
	public String getSelect() {
		return select;
	}


	public void setSelect(String select) {
		this.select = select;
	}


	public TextR() {
		this.select = "";
	}
	
	public TextR(String select){
		this.select = select;
	}
	
	public boolean validate(RegEx expression){
		return false;
	}
	
	public int getIdentifier()
	{
		return XTLTEXT;
	}
}
