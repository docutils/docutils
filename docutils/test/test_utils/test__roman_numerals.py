#! /usr/bin/env python3

# $Id$
# Author: Adam Turner.
# Copyright: This module is placed in the public domain
#            or under the `Zero Clause BSD licence`_,
#            whichever is more permissive.
#
# .. _Zero Clause BSD licence: https://opensource.org/license/0BSD

"""
Tests for `docutils.utils._roman_numerals`.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from docutils.utils._roman_numerals import (
    MIN,
    MAX,
    RomanNumeral,
    OutOfRangeError,
    InvalidRomanNumeralError,
)


class NewRomanNumeralTestCase(unittest.TestCase):
    def test_zero(self) -> None:
        with self.assertRaises(OutOfRangeError) as ctx:
            RomanNumeral(0)
        msg = str(ctx.exception)
        self.assertEqual(
            msg,
            'Number out of range (must be between 1 and 4,999). Got 0.',
        )

    def test_one(self) -> None:
        self.assertEqual(int(RomanNumeral(1)), 1)

    def test_MIN(self) -> None:
        self.assertEqual(int(RomanNumeral(MIN)), MIN)

    def test_forty_two(self) -> None:
        self.assertEqual(int(RomanNumeral(42)), 42)

    def test_four_thousand_nine_hundred_and_ninety_nine(self) -> None:
        self.assertEqual(int(RomanNumeral(4_999)), 4_999)

    def test_MAX(self) -> None:
        self.assertEqual(int(RomanNumeral(MAX)), MAX)

    def test_five_thousand(self) -> None:
        with self.assertRaises(OutOfRangeError) as ctx:
            RomanNumeral(5_000)
        msg = str(ctx.exception)
        self.assertEqual(
            msg,
            'Number out of range (must be between 1 and 4,999). Got 5000.',
        )

    def test_minus_one(self) -> None:
        with self.assertRaises(OutOfRangeError) as ctx:
            RomanNumeral(-1)
        msg = str(ctx.exception)
        self.assertEqual(
            msg,
            'Number out of range (must be between 1 and 4,999). Got -1.',
        )

    def test_float(self) -> None:
        with self.assertRaises(TypeError) as ctx:
            RomanNumeral(4.2)
        msg = str(ctx.exception)
        self.assertEqual(
            msg,
            "RomanNumeral: an integer is required, not 'float'",
        )


class ToStringTestCase(unittest.TestCase):
    def test_str(self):
        test_numerals = [
            'I', 'II', 'III', 'IV', 'V',
            'VI', 'VII', 'VIII', 'IX', 'X',
            'XI', 'XII', 'XIII', 'XIV', 'XV',
            'XVI', 'XVII', 'XVIII', 'XIX', 'XX',
            'XXI', 'XXII', 'XXIII', 'XXIV',
        ]
        for n, roman_str in enumerate(test_numerals, start=1):
            with self.subTest(id=n, roman_str=roman_str):
                num = RomanNumeral(n)
                self.assertEqual(f'{num}', roman_str)

    def test_uppercase(self):
        test_numerals = [
            'I', 'II', 'III', 'IV', 'V',
            'VI', 'VII', 'VIII', 'IX', 'X',
            'XI', 'XII', 'XIII', 'XIV', 'XV',
            'XVI', 'XVII', 'XVIII', 'XIX', 'XX',
            'XXI', 'XXII', 'XXIII', 'XXIV',
        ]
        for n, roman_str in enumerate(test_numerals, start=1):
            with self.subTest(id=n, roman_str=roman_str):
                num = RomanNumeral(n)
                self.assertEqual(num.to_uppercase(), roman_str)

    def test_lowercase(self):
        test_numerals = [
            'i', 'ii', 'iii', 'iv', 'v',
            'vi', 'vii', 'viii', 'ix', 'x',
            'xi', 'xii', 'xiii', 'xiv', 'xv',
            'xvi', 'xvii', 'xviii', 'xix', 'xx',
            'xxi', 'xxii', 'xxiii', 'xxiv',
        ]
        for n, roman_str in enumerate(test_numerals, start=1):
            with self.subTest(id=n, roman_str=roman_str):
                num = RomanNumeral(n)
                self.assertEqual(num.to_lowercase(), roman_str)

    def test_minitrue(self):
        # IGNORANCE IS STRENGTH
        num = RomanNumeral(1984)
        self.assertEqual(f'{num}', 'MCMLXXXIV')
        self.assertEqual(num.to_uppercase(), 'MCMLXXXIV')
        self.assertEqual(num.to_lowercase(), 'mcmlxxxiv')


class FromStringTestCase(unittest.TestCase):
    def test_uppercase(self):
        test_numerals = [
            'I', 'II', 'III', 'IV', 'V',
            'VI', 'VII', 'VIII', 'IX', 'X',
            'XI', 'XII', 'XIII', 'XIV', 'XV',
            'XVI', 'XVII', 'XVIII', 'XIX', 'XX',
            'XXI', 'XXII', 'XXIII', 'XXIV',
        ]
        for n, roman_str in enumerate(test_numerals, start=1):
            with self.subTest(id=n, roman_str=roman_str):
                expected = RomanNumeral(n)
                parsed = RomanNumeral.from_string(roman_str)
                self.assertEqual(expected, parsed)

    def test_lowercase(self):
        test_numerals = [
            'i', 'ii', 'iii', 'iv', 'v',
            'vi', 'vii', 'viii', 'ix', 'x',
            'xi', 'xii', 'xiii', 'xiv', 'xv',
            'xvi', 'xvii', 'xviii', 'xix', 'xx',
            'xxi', 'xxii', 'xxiii', 'xxiv',
        ]
        for n, roman_str in enumerate(test_numerals, start=1):
            with self.subTest(id=n, roman_str=roman_str):
                expected = RomanNumeral(n)
                parsed = RomanNumeral.from_string(roman_str)
                self.assertEqual(expected, parsed)

    def test_special(self):
        parsed = RomanNumeral.from_string('MDCCCXXIII')
        self.assertEqual(RomanNumeral(1823), parsed)

        parsed = RomanNumeral.from_string('mdcccxxiii')
        self.assertEqual(RomanNumeral(1823), parsed)

        parsed = RomanNumeral.from_string('MCMLXXXIV')
        self.assertEqual(RomanNumeral(1984), parsed)

        parsed = RomanNumeral.from_string('mcmlxxxiv')
        self.assertEqual(RomanNumeral(1984), parsed)

        parsed = RomanNumeral.from_string('MM')
        self.assertEqual(RomanNumeral(2000), parsed)

        parsed = RomanNumeral.from_string('mm')
        self.assertEqual(RomanNumeral(2000), parsed)

        parsed = RomanNumeral.from_string('MMMMCMXCIX')
        self.assertEqual(RomanNumeral(4_999), parsed)

        parsed = RomanNumeral.from_string('mmmmcmxcix')
        self.assertEqual(RomanNumeral(4_999), parsed)

    def test_invalid(self):
        with self.assertRaises(InvalidRomanNumeralError) as ctx:
            RomanNumeral.from_string('Not a Roman numeral!')
        msg = str(ctx.exception)
        self.assertEqual(msg, 'Invalid Roman numeral: Not a Roman numeral!')

    def test_mixed_case(self):
        with self.assertRaises(InvalidRomanNumeralError) as ctx:
            RomanNumeral.from_string('McMlXxXiV')
        msg = str(ctx.exception)
        self.assertEqual(msg, 'Invalid Roman numeral: McMlXxXiV')


class RoundTripTestCase(unittest.TestCase):
    def test_round_trip(self):
        for n in range(MIN, MAX + 1, 19):
            num = RomanNumeral(n)
            parsed = RomanNumeral.from_string(str(num))
            self.assertEqual(num, parsed)


if __name__ == '__main__':
    unittest.main()
