// Author: René Haberland
// 2007/30.12.2018, Saint Petersburg, Russia
//  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
//  For more informations on the license terms, visit https://creativecommons.org

package Regular;

import java.util.LinkedList;

public class ElR extends RegEx {
	private String name;

	private LinkedList<AttrEntry> attributes;

	private RegEx hedge;

	public LinkedList<AttrEntry> getAttributes() {
		return attributes;
	}

	public void setAttributes(LinkedList<AttrEntry> attributes) {
		this.attributes = attributes;
	}

	public RegEx getHedge() {
		return hedge;
	}

	public void setHedge(RegEx child) {
		this.hedge = child;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	// private universal constructor
	private void create(String name, LinkedList<AttrEntry> attributes,
			RegEx hedge) {
		this.name = name;
		this.attributes = attributes;
		this.hedge = hedge;
	}

	public ElR(String name) {
		create(name, null, null);
	}

	public ElR(String name, RegEx hedge) {
		create(name, null, hedge);
	}

	public ElR(String name, LinkedList<AttrEntry> attributes) {
		create(name, attributes, null);
	}

	public ElR(String name, LinkedList<AttrEntry> attributes, RegEx hedge) {
		create(name, attributes, hedge);
	}

	public boolean validate(RegEx expression) {
		int token = expression.getIdentifier();
		String name1 = this.getName();
		LinkedList<AttrEntry> atts1 = this.getAttributes();
		RegEx r1 = this.getHedge();

		switch (token) {
		case (RegEx.TEXT):
			// (ElR 10)
			return false;
		case (RegEx.EPSILON):
			// (ElR 9)
			return false;
		case (RegEx.ELEMENT):
			// (ElR 1)
			String name2 = ((ElR) expression).getName();

			// fuer
			ElR n = ElR.extractAttributes((ElR) expression);
			LinkedList<AttrEntry> atts3 = n.getAttributes();
			RegEx r3 = n.getHedge();

			if (name1.equals(name2) && ElR.qSort(atts1).equals(atts3)
					&& r1.validate(r3)) {
				return true;
			}
			return false;
		case (RegEx.STAR):
			// (ElR 7)
			return this.validate(((Star)expression).getExpression());
		case (RegEx.XTLTEXT):
			// (ElR 8)
			return false;
		case (RegEx.THEN):
			RegEx left = ((Then) expression).getLeft();
			RegEx right = ((Then) expression).getRight();
			int token2 = left.getIdentifier();

			switch (token2) {
			case (RegEx.ELEMENT):
				// (ElR 2)
				return this.validate(left) && (new Epsilon()).validate(right);

			case (RegEx.OR):
				// (ElR 3)
				if (this.validate(left) && (new Epsilon()).validate(right)) {
					return true;
				}
				if ((new Epsilon()).validate(left) && (this.validate(right))) {
					return true;
				}
				return false;

			case (RegEx.STAR):
				// (ElR 4)
				RegEx s1 = ((Star) left).getExpression();
				if ((this.validate(s1)) && ((new Epsilon()).validate(right))) {
					return true;
				}
				if (this.validate(right)) {
					return true;
				}
				return false;

			case (RegEx.MACROCALL):
				// (ElR 5)
				if ((this.validate(left)) && ((new Epsilon()).validate(right))) {
					return true;
				}
				if ((new Epsilon()).validate(left) && this.validate(right)) {
					return true;
				}
				return false;
			}
			// (ElR 6)
			return false;
		}
		return false;
	}

	public int getIdentifier() {
		return ELEMENT;
	}

	// TODO
	public static ElR extractAttributes(ElR input) {
		return input;
	}

	// TODO
	public static LinkedList<AttrEntry> qSort(LinkedList<AttrEntry> input) {
		return input;
	}

}
