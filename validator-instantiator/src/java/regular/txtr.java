// Author: René Haberland
// 2007/30.12.2018, Saint Petersburg, Russia
//  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
//  For more informations on the license terms, visit https://creativecommons.org

package Regular;

import java.util.Iterator;
import java.util.LinkedList;

public class TxtR extends RegEx {
	private String text;

	public String getText() {
		return text;
	}

	public void setText(String text) {
		this.text = text;
	}

	public TxtR() {
		this.text = "";
	}

	public TxtR(String text) {
		this.text = text;
	}

	public boolean validate(RegEx expression) {
		int token = expression.getIdentifier();
		String text = this.getText();

		switch (token) {
		case (RegEx.TEXT):
			// (# 7)
			return (text.equals(((TxtR)expression).getText()));
		case (RegEx.EPSILON):
			if (text.equals("")){
				// (# 2)
				return true;
			}
			else{
				// (# 3)
				return false;
			}
		case (RegEx.ELEMENT):
			// (# 8)
			return false;
		case (RegEx.STAR):
			if (text.equals("")){
				// (# 4)
				return true;
			}
			else{
				// (# 5)
				RegEx r = ((Star)expression).getExpression();
				LinkedList<TextSplitting> tupels = TxtR.frontSplitText(text);
				Iterator<TextSplitting> it = tupels.iterator();
				while (it.hasNext()){
					TextSplitting tupel = it.next();
					String s1 = tupel.left;
					String s2 = tupel.right;
					
					if ((new TxtR(s1)).validate(r) && (new TxtR(s2)).validate(expression)){
						return true;
					}
				}
				return this.validate(new Epsilon());
			}
		case (RegEx.XTLTEXT):
			// (# 6)
			return true;
		case (RegEx.THEN):
			// (# 1)
			RegEx r1 = ((Then)expression).getLeft();
			RegEx r2 = ((Then)expression).getRight();
			
			LinkedList<TextSplitting> tupels = TxtR.splitText(text);
			Iterator<TextSplitting> it = tupels.iterator();
			while (it.hasNext()){
				TextSplitting tupel = it.next();
				String s1 = tupel.left;
				String s2 = tupel.right;
				
				if ((new TxtR(s1)).validate(r1) && (new TxtR(s2)).validate(r2)){
					return true;
				}
			}
			return false;
		}
		
		return false;
	}

	// Identifier
	public int getIdentifier() {
		return TEXT;
	}

	// TODO
	public static LinkedList<TextSplitting> frontSplitText(String string) {
		return null;
	}

	// TODO 
	public static LinkedList<TextSplitting> splitText(String string) {
		return null;
	}
}
