// Author: René Haberland
// 2007/30.12.2018, Saint Petersburg, Russia
//  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
//  For more informations on the license terms, visit https://creativecommons.org

package Regular;

import java.util.Iterator;
import java.util.LinkedList;

public class Then extends RegEx {
	private RegEx left;

	private RegEx right;

	public RegEx getLeft() {
		return left;
	}

	public void setLeft(RegEx left) {
		this.left = left;
	}

	public RegEx getRight() {
		return right;
	}

	public void setRight(RegEx right) {
		this.right = right;
	}

	// private universal constructor
	private void create(RegEx left, RegEx right) {
		this.left = left;
		this.right = right;
	}

	public Then() {
		create(new Epsilon(), new Epsilon());
	}

	public Then(boolean left, RegEx n) {
		if (left == true) {
			create(n, new Epsilon());
		} else {
			create(new Epsilon(), n);
		}
	}

	public Then(RegEx left, RegEx right) {
		create(left, right);
	}

	public boolean validate(RegEx expression) {
		int token = expression.getIdentifier();

		RegEx r1 = this.getLeft();
		RegEx r2 = this.getRight();
		
		LinkedList<Splitting> tupels;
		Iterator<Splitting> it;
		RegEx s1, s2;
		RegEx t1, t2;

		switch (token) {
		case (RegEx.TEXT):
			// (Then 2)
			return r1.validate(expression) && r2.validate(new Epsilon());
		case (RegEx.EPSILON):
			// (Then 1)
			return false;
		case (RegEx.ELEMENT):
			if (r1.getIdentifier() == ELEMENT) {
				// (Then 3)
				return r1.validate(expression) && r2.validate(new Epsilon());
			} else {
				// (Then 4)
				return true;
			}
		case (RegEx.STAR):
			// (Then 5)
			tupels = Then.frontSplits(this);
			it = tupels.iterator();
			
			while (it.hasNext()) {
				Splitting tupel = it.next();
				s1 = tupel.left;
				s2 = tupel.right;

				if (s1.validate(((Star) expression).getExpression())
						&& s2.validate(expression)) {
					return true;
				}
			}
			return false;
		case (RegEx.XTLTEXT):
			if (r1.getIdentifier()==TEXT){
				// (Then 8)
				if (r2.getIdentifier()==EPSILON){
					return true;
				}
			}
			// (Then 9)
			return false;
		case (RegEx.THEN):
			if (r2.getIdentifier()==EPSILON){
				// (Then 6)
				return r1.validate(expression);
			}
			else{
				// (Then 7)
				s1 = ((Then)expression).getLeft();
				s2 = ((Then)expression).getRight();
				tupels = Then.splits(this);
				it = tupels.iterator();
				while (it.hasNext()){
					Splitting tupel = it.next();
					t1 = tupel.left;
					t2 = tupel.right;
					
					if (t1.validate(s1) && t2.validate(s2)){
						return true;
					}
				}
				return false;
			}
		}
		return false;
	}

	// Identifier
	public int getIdentifier() {
		return THEN;
	}

	// TODO
	public static LinkedList<Splitting> frontSplits(RegEx expression) {
		return null;
	}

	// TODO
	public static LinkedList<Splitting> splits(RegEx expression) {
		return null;
	}

}
