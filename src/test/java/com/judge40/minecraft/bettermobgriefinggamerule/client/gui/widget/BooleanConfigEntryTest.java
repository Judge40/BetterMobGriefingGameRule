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

import com.judge40.minecraft.bettermobgriefinggamerule.TestUtils;
import java.util.List;
import java.util.stream.Collectors;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.widget.button.Button;
import net.minecraft.util.text.TextFormatting;
import net.minecraftforge.fml.client.config.GuiButtonExt;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class BooleanConfigEntryTest {

  @BeforeAll
  static void setUpBeforeAll() throws IllegalAccessException {
    TestUtils.initializeTestEnvironment();
  }

  @ParameterizedTest(name = "Should update current value when value button pressed {index} times.")
  @ValueSource(booleans = {true, false})
  void shouldUpdateCurrentValueWhenValueButtonPressed(boolean initialValue) {
    // Given.
    BooleanConfigEntry entry = new BooleanConfigEntry(null, 0, "label", initialValue, false);

    // When.
    Button valueButton = (Button) entry.children().get(0);
    valueButton.onPress();

    // Then.
    assertThat("Unexpected current value.", entry.getCurrentValue(), is(!initialValue));
    assertThat("Unexpected value for isChanged.", entry.isChanged(), is(true));
    assertThat("Unexpected value for isDefault.", entry.isDefault(), is(initialValue));
  }

  @ParameterizedTest(name = "Should restore initial value when value button pressed {index} times"
      + " and Reset button pressed.")
  @ValueSource(booleans = {true, false})
  void shouldRestoreInitialValueWhenResetButtonPressed(boolean initialValue) {
    // Given.
    BooleanConfigEntry entry = new BooleanConfigEntry(null, 0, "label", initialValue, true);

    // When.
    Button valueButton = (Button) entry.children().get(0);
    valueButton.onPress();

    Button resetButton = (Button) entry.children().get(1);
    resetButton.onPress();

    // Then.
    assertThat("Unexpected current value.", entry.getCurrentValue(), is(initialValue));
    assertThat("Unexpected value for isChanged.", entry.isChanged(), is(false));
  }

  @ParameterizedTest(name = "Should restore default value when Default button pressed and current"
      + " value is {0}")
  @ValueSource(booleans = {true, false})
  void shouldRestoreDefaultValueWhenDefaultButtonPressed(boolean initialValue) {
    // Given.
    BooleanConfigEntry entry = new BooleanConfigEntry(null, 0, "label", initialValue, true);

    // When.
    Button defaultButton = (Button) entry.children().get(2);
    defaultButton.onPress();

    // Then.
    assertThat("Unexpected current value.", entry.getCurrentValue(), is(true));
    assertThat("Unexpected value for isDefault.", entry.isDefault(), is(true));
  }

  @ParameterizedTest(name = "Should render buttons with correct state when the initial value is {0}"
      + " and is unchanged")
  @ValueSource(booleans = {true, false})
  void shouldRenderButtonsWithCorrectStateWhenValueUnchanged(boolean initialValue) {
    // Given.
    FontRenderer fontRenderer = mock(FontRenderer.class);
    BooleanConfigEntry entry = new BooleanConfigEntry(fontRenderer, 0, "label", initialValue, true);

    // Override visibility of buttons so no attempt is made to actually render them.
    List<Button> children = entry.children().stream()
        .filter(child -> child instanceof Button)
        .map(button -> (Button) button)
        .collect(Collectors.toList());
    children.forEach(button -> button.visible = false);

    // When.
    entry.render(0, 0, 0, 0, 0, 0, 0, true, 0);

    // Then.
    Button valueButton = children.get(0);
    assertThat("Unexpected button message.", valueButton.getMessage(),
        is(Boolean.toString(initialValue)));

    Button resetButton = children.get(1);
    assertThat("Unexpected reset button active state.", resetButton.active, is(false));

    Button defaultButton = children.get(2);
    assertThat("Unexpected default button active state.", defaultButton.active, is(!initialValue));
  }

  @ParameterizedTest(name = "Should render buttons with correct state when the initial value is {0}"
      + " and is changed")
  @ValueSource(booleans = {true, false})
  void shouldRenderButtonsWithCorrectStateWhenValueChanged(boolean initialValue) {
    // Given.
    FontRenderer fontRenderer = mock(FontRenderer.class);
    BooleanConfigEntry entry = new BooleanConfigEntry(fontRenderer, 0, "label", initialValue,
        false);

    // Override visibility of buttons so no attempt is made to actually render them.
    List<GuiButtonExt> children = entry.children().stream()
        .filter(child -> child instanceof GuiButtonExt)
        .map(button -> (GuiButtonExt) button)
        .collect(Collectors.toList());
    children.forEach(button -> button.visible = false);

    // When.
    Button valueButton = children.get(0);
    valueButton.onPress();
    entry.render(0, 0, 0, 0, 0, 0, 0, true, 0);

    // Then.
    boolean currentValue = entry.getCurrentValue();
    assertThat("Unexpected button message.", valueButton.getMessage(),
        is(Boolean.toString(currentValue)));

    Button resetButton = children.get(1);
    assertThat("Unexpected reset button active state.", resetButton.active, is(true));

    Button defaultButton = children.get(2);
    assertThat("Unexpected default button active state.", defaultButton.active, is(currentValue));
  }

  @Test
  void shouldRenderEntryLabel() {
    // Given.
    FontRenderer fontRenderer = mock(FontRenderer.class);
    BooleanConfigEntry entry = new BooleanConfigEntry(fontRenderer, 100, "label", true, false);

    // Override visibility of buttons so no attempt is made to actually render them.
    List<GuiButtonExt> children = entry.children().stream()
        .filter(child -> child instanceof GuiButtonExt)
        .map(button -> (GuiButtonExt) button)
        .collect(Collectors.toList());
    children.forEach(button -> button.visible = false);

    // When.
    entry.render(10, 20, 30, 40, 50, 60, 70, true, 90);

    // Then.
    int colorCode = TextFormatting.WHITE.getColor();
    verify(fontRenderer).drawString("label", 20, 40.5F, colorCode);
  }
}
