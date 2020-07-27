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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import javax.annotation.Nonnull;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.IGuiEventListener;
import net.minecraft.client.gui.widget.button.Button;
import net.minecraft.client.resources.I18n;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.fml.client.config.GuiButtonExt;

@OnlyIn(Dist.CLIENT)
public abstract class AbstractConfigEntry<T> extends AbstractEntry {

  private final FontRenderer fontRenderer;
  private final int labelOffset;
  private final String label;

  private final T initialValue;
  private final T defaultValue;
  private T currentValue;

  private final Button valueButton;
  private final Button resetButton;
  private final Button defaultButton;

  /**
   * A configuration entry with a label, value selection, reset button and default button.
   *
   * @param fontRenderer The font renderer to use to draw labels.
   * @param labelOffset  The offset from center to use for the entry's label, should match the
   *                     longest label in a list.
   * @param label        The label to display for the entry.
   * @param initialValue The entry's initial value.
   * @param defaultValue The entry's default value, used when setting a changed value to default.
   */
  AbstractConfigEntry(FontRenderer fontRenderer, int labelOffset, String label, T initialValue,
      T defaultValue) {
    this.fontRenderer = fontRenderer;
    this.labelOffset = labelOffset;
    this.label = label;

    this.initialValue = currentValue = initialValue;
    this.defaultValue = defaultValue;

    valueButton = new GuiButtonExt(0, 0, 50, 20, currentValue.toString(),
        (button) -> currentValue = getNextValue());

    resetButton = new GuiButtonExt(0, 0, 50, 20, I18n.format("controls.reset"),
        (button) -> restoreInitialValue());

    defaultButton = new GuiButtonExt(0, 0, 50, 20, "Default", (button) -> restoreDefaultValue());
  }

  /**
   * Whether the entry's current value matches its initial value.
   *
   * @return Whether the entry's value has changed.
   */
  public boolean isChanged() {
    return !Objects.equals(currentValue, initialValue);
  }

  /**
   * Whether the entry's current value matches its default value.
   *
   * @return Whether current value is the default.
   */
  public boolean isDefault() {
    return Objects.equals(currentValue, defaultValue);
  }

  /**
   * Get the entry's current value.
   *
   * @return The entry's value.
   */
  public T getCurrentValue() {
    return currentValue;
  }

  /**
   * Get the next value in the entry's value sequence.
   *
   * @return The next value in the entry's sequence.
   */
  abstract T getNextValue();

  /**
   * Set the entry's value to its initial value.
   */
  public void restoreInitialValue() {
    currentValue = initialValue;
  }

  /**
   * Set the entry's value to its default value.
   */
  public void restoreDefaultValue() {
    currentValue = defaultValue;
  }

  @Override
  public void render(int p_render_1_, int p_render_2_, int p_render_3_, int p_render_4_,
      int p_render_5_, int p_render_6_, int p_render_7_, boolean p_render_8_, float p_render_9_) {
    float x = p_render_3_ + 90 - labelOffset;
    float y = (float) (p_render_2_ + p_render_5_ / 2 - 4.5);
    fontRenderer.drawString(label, x, y, 16777215);

    valueButton.x = p_render_3_ + 105;
    valueButton.y = p_render_2_;
    valueButton.setMessage(currentValue.toString());
    valueButton.render(p_render_6_, p_render_7_, p_render_9_);

    resetButton.x = p_render_3_ + 165;
    resetButton.y = p_render_2_;
    resetButton.active = isChanged();
    resetButton.render(p_render_6_, p_render_7_, p_render_9_);

    defaultButton.x = p_render_3_ + 215;
    defaultButton.y = p_render_2_;
    defaultButton.active = !isDefault();
    defaultButton.render(p_render_6_, p_render_7_, p_render_9_);
  }

  @Nonnull
  @Override
  public List<? extends IGuiEventListener> children() {
    return Collections.unmodifiableList(Arrays.asList(valueButton, resetButton, defaultButton));
  }

  @Override
  public boolean mouseClicked(double x, double y, int button) {
    return valueButton.mouseClicked(x, y, button)
        || resetButton.mouseClicked(x, y, button)
        || defaultButton.mouseClicked(x, y, button);
  }

  @Override
  public boolean mouseReleased(double x, double y, int button) {
    return valueButton.mouseReleased(x, y, button)
        || resetButton.mouseReleased(x, y, button)
        || defaultButton.mouseReleased(x, y, button);
  }
}
