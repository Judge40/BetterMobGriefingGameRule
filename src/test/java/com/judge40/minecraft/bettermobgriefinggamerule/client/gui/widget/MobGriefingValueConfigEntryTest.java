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

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import com.mojang.blaze3d.matrix.MatrixStack;
import java.util.List;
import java.util.stream.Collectors;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.widget.button.Button;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.TextFormatting;
import net.minecraftforge.fml.client.gui.widget.ExtendedButton;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class MobGriefingValueConfigEntryTest {

  @Test
  void shouldReturnEntityId() {
    // Given.
    MobGriefingValueConfigEntry entry = new MobGriefingValueConfigEntry(null, 0,
        new ResourceLocation("test:entity"), MobGriefingValue.INHERIT);

    // When.
    ResourceLocation entityId = entry.getEntityId();

    // Then.
    assertThat("Unexpected entityId.", entityId, is(new ResourceLocation("test:entity")));
  }

  @ParameterizedTest(name = "Should update current value when value button pressed {index} times.")
  @EnumSource(MobGriefingValue.class)
  void shouldUpdateCurrentValueWhenValueButtonPressed(MobGriefingValue targetValue) {
    // Given.
    MobGriefingValue[] mobGriefingValues = MobGriefingValue.values();
    MobGriefingValue initialValue = mobGriefingValues[mobGriefingValues.length - 1];
    MobGriefingValueConfigEntry entry = new MobGriefingValueConfigEntry(null, 0,
        new ResourceLocation("test:entity"), initialValue);

    // When.
    Button valueButton = (Button) entry.getEventListeners().get(0);

    for (int i = 0; i <= targetValue.ordinal(); i++) {
      valueButton.onPress();
    }

    // Then.
    MobGriefingValue currentValue = entry.getCurrentValue();
    assertThat("Unexpected current value.", currentValue, is(targetValue));

    boolean expectedIsChanged = !currentValue.equals(initialValue);
    assertThat("Unexpected value for isChanged.", entry.isChanged(), is(expectedIsChanged));

    boolean expectedIsDefault = currentValue.equals(MobGriefingValue.INHERIT);
    assertThat("Unexpected value for isDefault.", entry.isDefault(), is(expectedIsDefault));
  }

  @ParameterizedTest(name = "Should restore initial value when value button pressed {index} times"
      + " and Reset button pressed.")
  @EnumSource(MobGriefingValue.class)
  void shouldRestoreInitialValueWhenResetButtonPressed(MobGriefingValue initialValue) {
    // Given.
    MobGriefingValueConfigEntry entry = new MobGriefingValueConfigEntry(null, 0,
        new ResourceLocation("test:entity"), initialValue);

    // When.
    Button valueButton = (Button) entry.getEventListeners().get(0);

    for (int i = 0; i <= initialValue.ordinal(); i++) {
      valueButton.onPress();
    }

    Button resetButton = (Button) entry.getEventListeners().get(1);
    resetButton.onPress();

    // Then.
    assertThat("Unexpected current value.", entry.getCurrentValue(), is(initialValue));
    assertThat("Unexpected value for isChanged.", entry.isChanged(), is(false));
  }

  @ParameterizedTest(name = "Should restore default value when Default button pressed and current"
      + " value is {0}")
  @EnumSource(MobGriefingValue.class)
  void shouldRestoreDefaultValueWhenDefaultButtonPressed(MobGriefingValue initialValue) {
    // Given.
    MobGriefingValueConfigEntry entry = new MobGriefingValueConfigEntry(null, 0,
        new ResourceLocation("test:entity"), initialValue);

    // When.
    Button defaultButton = (Button) entry.getEventListeners().get(2);
    defaultButton.onPress();

    // Then.
    assertThat("Unexpected current value.", entry.getCurrentValue(), is(MobGriefingValue.INHERIT));
    assertThat("Unexpected value for isDefault.", entry.isDefault(), is(true));
  }

  @ParameterizedTest(name = "Should render buttons with correct state when the initial value is {0}"
      + " and is unchanged")
  @EnumSource(MobGriefingValue.class)
  void shouldRenderButtonsWithCorrectStateWhenValueUnchanged(MobGriefingValue initialValue) {
    // Given.
    FontRenderer fontRenderer = mock(FontRenderer.class);
    MobGriefingValueConfigEntry entry = new MobGriefingValueConfigEntry(fontRenderer, 0,
        new ResourceLocation("test:entity"), initialValue);

    // Override visibility of buttons so no attempt is made to actually render them.
    List<Button> children = entry.getEventListeners().stream()
        .filter(child -> child instanceof Button)
        .map(button -> (Button) button)
        .collect(Collectors.toList());
    children.forEach(button -> button.visible = false);

    // When.
    entry.render(new MatrixStack(), 0, 0, 0, 0, 0, 0, 0, true, 0);

    // Then.
    Button valueButton = children.get(0);
    assertThat("Unexpected button message.", valueButton.getMessage().getString(),
        is(initialValue.toString()));

    Button resetButton = children.get(1);
    assertThat("Unexpected reset button active state.", resetButton.active, is(false));

    Button defaultButton = children.get(2);
    boolean expectedDefaultActive = !initialValue.equals(MobGriefingValue.INHERIT);
    assertThat("Unexpected default button active state.", defaultButton.active,
        is(expectedDefaultActive));
  }

  @ParameterizedTest(name = "Should render buttons with correct state when the initial value is {0}"
      + " and is changed")
  @EnumSource(MobGriefingValue.class)
  void shouldRenderButtonsWithCorrectStateWhenValueChanged(MobGriefingValue initialValue) {
    // Given.
    FontRenderer fontRenderer = mock(FontRenderer.class);
    MobGriefingValueConfigEntry entry = new MobGriefingValueConfigEntry(fontRenderer, 0,
        new ResourceLocation("test:entity"), initialValue);

    // Override visibility of buttons so no attempt is made to actually render them.
    List<ExtendedButton> children = entry.getEventListeners().stream()
        .filter(child -> child instanceof ExtendedButton)
        .map(button -> (ExtendedButton) button)
        .collect(Collectors.toList());
    children.forEach(button -> button.visible = false);

    // When.
    Button valueButton = children.get(0);
    valueButton.onPress();
    entry.render(new MatrixStack(), 0, 0, 0, 0, 0, 0, 0, true, 0);

    // Then.
    MobGriefingValue currentValue = entry.getCurrentValue();
    assertThat("Unexpected button message.", valueButton.getMessage().getString(),
        is(currentValue.toString()));

    Button resetButton = children.get(1);
    assertThat("Unexpected reset button active state.", resetButton.active, is(true));

    Button defaultButton = children.get(2);
    boolean expectedDefaultActive = !currentValue.equals(MobGriefingValue.INHERIT);
    assertThat("Unexpected default button active state.", defaultButton.active,
        is(expectedDefaultActive));
  }

  @Test
  void shouldRenderEntryLabel() {
    // Given.
    FontRenderer fontRenderer = mock(FontRenderer.class);
    MobGriefingValueConfigEntry entry = new MobGriefingValueConfigEntry(fontRenderer, 100,
        new ResourceLocation("test:entity"), MobGriefingValue.INHERIT);

    // Override visibility of buttons so no attempt is made to actually render them.
    List<ExtendedButton> children = entry.getEventListeners().stream()
        .filter(child -> child instanceof ExtendedButton)
        .map(button -> (ExtendedButton) button)
        .collect(Collectors.toList());
    children.forEach(button -> button.visible = false);

    MatrixStack matrixStack = new MatrixStack();

    // When.
    entry.render(matrixStack, 10, 20, 30, 40, 50, 60, 70, true, 90);

    // Then.
    int colorCode = TextFormatting.WHITE.getColor();
    verify(fontRenderer).drawString(matrixStack, "test:entity", 20, 40.5F, colorCode);
  }
}
