// Author: René Haberland
// 2007/30.12.2018, Saint Petersburg, Russia
//  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
//  For more informations on the license terms, visit https://creativecommons.org

package Regular;

public class Or extends RegEx {
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

	public Or() {
		create(new Epsilon(), new Epsilon());
	}

	public Or(RegEx left) {
		create(left, new Epsilon());
	}

	public Or(boolean left, RegEx n) {
		if (left == true) {
			create(n, new Epsilon());
		} else {
			create(new Epsilon(), n);
		}
	}

	public Or(RegEx left, RegEx right) {
		create(left, right);
	}

	public boolean validate(RegEx expression) {
		return false;
	}

	public int getIdentifier() {
		return OR;
	}
}
