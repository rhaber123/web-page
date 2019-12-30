// Author: René Haberland
// 2007/30.12.2018, Saint Petersburg, Russia
//  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
//  For more informations on the license terms, visit https://creativecommons.org

package Regular;

public class Epsilon extends RegEx {
	public boolean validate(RegEx expression){
		int token = expression.getIdentifier();

		switch (token) {
		case (RegEx.TEXT):
			if (((TxtR) expression).getText().equals("")) {
				// (E1)
				return true;
			} else {
				// (E2)
				return false;
			}
		case (RegEx.EPSILON):
			// (E3)
			return true;
		case (RegEx.ELEMENT):
			// (E4)
			return false;
		case (RegEx.STAR):
			// (E5)
			return true;
		case (RegEx.XTLTEXT):
			// (E6)
			return true;
		case (RegEx.THEN):
			// (E7)
			RegEx r1 = ((Then) expression).getLeft();
			RegEx r2 = ((Then) expression).getRight();
			return this.validate(r1) && this.validate(r2);
		}
		return false;
	}

	public int getIdentifier()
	{
		return EPSILON;
	}
}
