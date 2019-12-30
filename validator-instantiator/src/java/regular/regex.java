// Author: René Haberland
// 2007/30.12.2018, Saint Petersburg, Russia
//  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
//  For more informations on the license terms, visit https://creativecommons.org

package Regular;

import java.util.Iterator;
import java.util.LinkedList;

public abstract class RegEx {

	/**
	 * Role: Hook ... // (Macro 1) this.validateMacro(expression); ...
	 */
	public abstract boolean validate(RegEx expression);

	public abstract int getIdentifier();

	private LinkedList<MacroEntry> macros;

	public RegEx getMacro(String mName) {
		Iterator<MacroEntry> it = macros.iterator();
		while (it.hasNext()) {
			MacroEntry mEntry = it.next();
			if (mEntry.macroName.equals(mName)) {
				return mEntry.hedge;
			}
		}
		return null;
	}

	public void setMacros(LinkedList<MacroEntry> macros) {
		this.macros = macros;
	}

	/**
	 * Role: Template
	 */
	public boolean validateMacro(MacroR expression) {
		RegEx hedge = this.getMacro(expression.getCallee());
		return this.validate(hedge);
	}

	/**
	 * Rolle: Template
	 */
	public boolean validateOr(Or expression) {
		return (this.validate(expression.getLeft()) && this.validate(expression
				.getRight()));
	}

	/**
	 * Constants for IDs, match (no parsing)
	 */
	// Literals
	protected final static int ELEMENT = 1;

	protected final static int TEXT = 2;

	protected final static int XTLTEXT = 3;

	protected final static int MACROCALL = 4;

	// regular operators
	protected final static int EPSILON = 5;

	protected final static int THEN = 6;

	protected final static int OR = 7;

	protected final static int STAR = 8;
}
