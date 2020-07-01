/*
 * Better mobGriefing GameRule Copyright (c) 2017 Judge40
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.judge40.minecraft.bettermobgriefinggamerule.common;

import java.util.Locale;

/**
 * An enumeration representing the valid values for the mobGriefing game rules.
 */
public enum MobGriefingValue {
  FALSE, INHERIT, TRUE;

  /**
   * Convert an enumeration's external form to the enumeration.
   *
   * @param externalForm The external form of the enumeration, which is its name.
   * @return The enumeration matching the name.
   * @throws IllegalArgumentException If the external form does not match an enumeration.
   */
  public static MobGriefingValue toEnumeration(String externalForm) {
    return valueOf(externalForm.toUpperCase(Locale.ENGLISH));
  }

  /**
   * Converts the enumeration to its external form, which is its name as lower case.
   *
   * @return The enumeration's external form.
   */
  public String toExternalForm() {
    return name().toLowerCase(Locale.ENGLISH);
  }
}
