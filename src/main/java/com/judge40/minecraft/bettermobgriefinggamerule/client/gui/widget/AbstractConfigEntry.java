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

import com.mojang.blaze3d.matrix.MatrixStack;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import javax.annotation.Nonnull;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.IGuiEventListener;
import net.minecraft.client.gui.widget.button.Button;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.fml.client.gui.widget.ExtendedButton;

@OnlyIn(Dist.CLIENT)
public abstract class AbstractConfigEntry<T> extends AbstractEntry {

  private static final TranslationTextComponent DEFAULT = new TranslationTextComponent(
      "bettermobgriefinggamerule.config.gui.default");
  private static final TranslationTextComponent RESET = new TranslationTextComponent(
      "bettermobgriefinggamerule.config.gui.reset");

  private static final int BUTTON_HEIGHT = 20;
  private static final int BUTTON_WIDTH = 50;

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

    StringTextComponent currentValueText = new StringTextComponent(currentValue.toString());
    valueButton = new ExtendedButton(0, 0, BUTTON_WIDTH, BUTTON_HEIGHT, currentValueText,
        button -> currentValue = getNextValue());

    resetButton = new ExtendedButton(0, 0, BUTTON_WIDTH, BUTTON_HEIGHT, RESET,
        button -> restoreInitialValue());

    defaultButton = new ExtendedButton(0, 0, BUTTON_WIDTH, BUTTON_HEIGHT, DEFAULT,
        button -> restoreDefaultValue());
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
  public void render(@Nonnull MatrixStack matrixStack, int render1, int render2, int render3,
      int render4, int render5, int render6, int render7, boolean render8, float render9) {
    float x = (float) render3 + 90 - labelOffset;
    float y = render2 + render5 / 2F - 4.5F;
    fontRenderer.draw(matrixStack, label, x, y, 16777215);

    valueButton.x = render3 + 105;
    valueButton.y = render2;
    valueButton.setMessage(new StringTextComponent(currentValue.toString()));
    valueButton.render(matrixStack, render6, render7, render9);

    resetButton.x = render3 + 165;
    resetButton.y = render2;
    resetButton.active = isChanged();
    resetButton.render(matrixStack, render6, render7, render9);

    defaultButton.x = render3 + 215;
    defaultButton.y = render2;
    defaultButton.active = !isDefault();
    defaultButton.render(matrixStack, render6, render7, render9);
  }

  @Nonnull
  @Override
  public List<? extends IGuiEventListener> children() {
    return Collections.unmodifiableList(Arrays.asList(valueButton, resetButton, defaultButton));
  }
}
