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

import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import java.util.Collections;
import java.util.List;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.narration.NarratableEntry;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class MobGriefingValueConfigEntry extends AbstractConfigEntry<MobGriefingValue> {

  private static final MobGriefingValue[] MOB_GRIEFING_VALUES = MobGriefingValue.values();

  private final ResourceLocation entityId;

  /**
   * A configuration entry with a label, value selection, reset button and default button.
   *
   * @param font         The font to use to draw labels.
   * @param labelOffset  The offset from center to use for the entry's label, should match the
   *                     longest label in a list.
   * @param entityId     The ID of entity controlled by this entry.
   * @param initialValue The entry's initial value.
   */
  MobGriefingValueConfigEntry(Font font, int labelOffset, ResourceLocation entityId,
      MobGriefingValue initialValue) {
    super(font, labelOffset, entityId.toString(), initialValue, MobGriefingValue.INHERIT);
    this.entityId = entityId;
  }

  @Override
  protected MobGriefingValue getNextValue() {
    int newOrdinal = (getCurrentValue().ordinal() + 1) % MOB_GRIEFING_VALUES.length;
    return MOB_GRIEFING_VALUES[newOrdinal];
  }

  /**
   * Get the ID of entity controlled by this entry.
   *
   * @return The entry's entity ID.
   */
  public ResourceLocation getEntityId() {
    return entityId;
  }

  @Override
  public List<? extends NarratableEntry> narratables() {
    return Collections.emptyList();
  }
}
