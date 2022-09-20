/*
 * Better mobGriefing GameRule Copyright (c) 2020 Judge40
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

package com.judge40.minecraft.bettermobgriefinggamerule.client.gui.widget;

import java.util.Collections;
import java.util.List;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.narration.NarratableEntry;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class BooleanConfigEntry extends AbstractConfigEntry<Boolean> {

  /**
   * A configuration entry with a label, boolean value selection, reset button and default button.
   *
   * @param font         The font to use to draw labels.
   * @param labelOffset  The offset from center to use for the entry's label, should match the
   *                     longest label in a list.
   * @param label        The label to display for the entry.
   * @param initialValue The entry's initial value.
   * @param defaultValue The entry's default value, used when setting a changed value to default.
   */
  BooleanConfigEntry(Font font, int labelOffset, String label, boolean initialValue,
      boolean defaultValue) {
    super(font, labelOffset, label, initialValue, defaultValue);
  }

  @Override
  protected Boolean getNextValue() {
    return !getCurrentValue();
  }

  @Override
  public List<? extends NarratableEntry> narratables() {
    return Collections.emptyList();
  }
}
