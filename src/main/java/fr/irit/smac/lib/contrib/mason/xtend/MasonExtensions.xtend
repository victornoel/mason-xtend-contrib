package fr.irit.smac.lib.contrib.mason.xtend

import sim.util.Double2D
import sim.util.MutableDouble2D

import static extension fr.irit.smac.lib.contrib.xtend.JavaExtensions.*

class MasonExtensions {
	
	public static val PI_OVER_2 = Math.PI/2.0
	
	/**
	 * returns a normalised vector
	 */
	@Pure
	static def Double2D middleAngledVector(Double2D from, Double2D to) {
		// dot product:
		// <0 if they are opposite
		// >0 if they are in the same direction
		val d = from.dot(to)
		// wedge product: is 0 if they are co-linear
		val w = from.perpDot(to)
		val v = if (d < 0 && w == 0) {
			from.rotate(PI_OVER_2)
		} else if (d > 0 && w == 0) {
			from
		} else {
			// wedge sign gives us the correct direction
			from.add(to).multiply(w)
		}
		v.normalize
	}
	
	@Pure
	static def toShortString(Double2D d, int nbDigit) {
		"("+d.x.toShortString(nbDigit)+","+d.y.toShortString(nbDigit)+")"
	}
	
	@Pure
	@Inline("$1.multiply($2)")
	static def Double2D operator_multiply(Double2D a, double s) {
		a.multiply(s)
	}
	
	@Pure
	@Inline("$1.negate()")
	static def Double2D operator_minus(Double2D a) {
		a.negate
	}
	
	@Pure
	@Inline("$1")
	static def Double2D operator_plus(Double2D a) {
		a
	}
	
	@Inline("$1.addIn($2)")
	static def MutableDouble2D operator_add(MutableDouble2D a, Double2D b) {
		a.addIn(b)
	}
	
	@Inline("$1.addIn($2)")
	static def MutableDouble2D operator_add(MutableDouble2D a, MutableDouble2D b) {
		a.addIn(b)
	}
	
	@Inline("$1.subtractIn($2)")
	static def MutableDouble2D operator_remove(MutableDouble2D a, Double2D b) {
		a.subtractIn(b)
	}
	
	@Inline("$1.subtractIn($2)")
	static def MutableDouble2D operator_remove(MutableDouble2D a, MutableDouble2D b) {
		a.subtractIn(b)
	}
	
	@Pure
	@Inline("$1.multiply(1.0/$2)")
	static def Double2D operator_divide(Double2D a, double s) {
		a.multiply(1.0/s)
	}
	
	@Pure
	@Inline("$1.add($2)")
	static def Double2D operator_plus(Double2D a, Double2D b) {
		a.add(b)
	}
	
	@Pure
	@Inline("$1.subtract($2)")
	static def Double2D operator_minus(Double2D a, Double2D b) {
		a.subtract(b)
	}
	
	/**
	 * from https://github.com/mikolalysenko/compare-slope/blob/master/slope.js
	 * > 0 if b is clockwise from a
	 * < 0 if a is clockwise from b
	 * 0 if a and b are collinear
	 */
	@Pure
	static def compare(Double2D a, Double2D b) {
		val d = quadrant(a) - quadrant(b)
		if (d != 0) { // different quadrants
			d
		} else {
			// p-q is the wedge product
			val p = a.x * b.y
			val q = a.y * b.x
			if (p > q) -1
			else if (p < q) 1 
			else 0
		}
	}
	
	@Pure
	private static def quadrant(Double2D it) {
		if (x > 0) {
			if (y >= 0) {
				return 1
			} else {
				return 4
			}
		} else if (x < 0) {
			if (y >= 0) {
				return 2
			} else {
				return 3
			}
		} else if (y > 0) {
			return 1
		} else if (y < 0) {
			return 3
		}
		return 0
	}
	
	@Pure
	@Inline(value="$3.compare($1, $2) <= 0", imported=MasonExtensions)
	static def operator_lessEqualsThan(Double2D what, Double2D from) {
		compare(what, from) <= 0
	}
	
	@Pure
	@Inline(value="$3.compare($1, $2) < 0", imported=MasonExtensions)
	static def operator_lessThan(Double2D what, Double2D from) {
		compare(what, from) < 0
	}
	
	@Pure
	@Inline(value="$3.compare($1, $2) > 0", imported=MasonExtensions)
	static def operator_greaterThan(Double2D what, Double2D to) {
		compare(what, to) > 0
	}
	
	@Pure
	@Inline(value="$3.compare($1, $2) >= 0", imported=MasonExtensions)
	static def operator_greaterEqualsThan(Double2D what, Double2D to) {
		compare(what, to) >= 0
	}
	
	@Pure
	@Inline(value="$3.between($1, $2.getKey(), $2.getValue())", imported=MasonExtensions)
	static def between(Double2D what, Pair<Double2D, Double2D> p) {
		between(what, p.key, p.value)
	}
	
	@Pure
	static def between(Double2D what, Double2D from, Double2D to) {
		// two cases:
		// 1) from is before to in counter clockwise and 2) to is before from
		if (
			//from <= to
			compare(from, to) <= 0
		) {
			// -Pi ------ F ********** T ------- Pi
			(
				//from < what
				compare(from, what) < 0
			) && (
				//what <= to
				compare(what, to) <= 0
			)
		} else {
			// -Pi ******* T ---------- F ******* Pi
			(
				//what <= to
				compare(what, to) <= 0
			) || (
				// from < what
				compare(from, what) < 0
			)
		}
	}
}