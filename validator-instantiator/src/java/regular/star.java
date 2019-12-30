// Author: René Haberland
// 2007/30.12.2018, Saint Petersburg, Russia
//  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
//  For more informations on the license terms, visit https://creativecommons.org

package Regular;

public class Star extends RegEx {
	private RegEx expression;

	public RegEx getExpression() {
		return expression;
	}

	public void setExpression(RegEx expression) {
		this.expression = expression;
	}

	public Star() {
		this.expression = new Epsilon();
	}

	public Star(RegEx expression) {
		this.expression = expression;
	}
	
	public boolean validate(RegEx expression){
		return false;
	}
	
	public int getIdentifier()
	{
		return STAR;
	}
}
