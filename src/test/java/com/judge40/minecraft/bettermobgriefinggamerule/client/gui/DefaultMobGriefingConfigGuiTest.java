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

package com.judge40.minecraft.bettermobgriefinggamerule.client.gui;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.ArgumentMatchers.anyFloat;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;

import com.judge40.minecraft.bettermobgriefinggamerule.TestUtils;
import com.judge40.minecraft.bettermobgriefinggamerule.client.gui.widget.BooleanConfigEntry;
import com.judge40.minecraft.bettermobgriefinggamerule.client.gui.widget.ConfigEntryList;
import com.judge40.minecraft.bettermobgriefinggamerule.client.gui.widget.MobGriefingValueConfigEntry;
import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.Config;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.screen.Screen;
import net.minecraft.client.gui.widget.button.Button;
import net.minecraft.util.ResourceLocation;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * The unit tests for {@link DefaultMobGriefingConfigGui}.
 */
class DefaultMobGriefingConfigGuiTest {

  private DefaultMobGriefingConfigGui gui;

  private ConfigEntryList entryList;

  @BeforeAll
  static void setUpBeforeAll() throws IllegalAccessException {
    TestUtils.initializeTestEnvironment();
  }

  @BeforeEach
  void setUp() throws IllegalAccessException {
    Minecraft minecraft = mock(Minecraft.class);
    FontRenderer fontRenderer = mock(FontRenderer.class);
    Field fontRendererField = FieldUtils.getField(Minecraft.class, "fontRenderer");
    FieldUtils.removeFinalModifier(fontRendererField);
    FieldUtils.writeField(fontRendererField, minecraft, fontRenderer, true);
    Screen parentScreen = mock(Screen.class);
    gui = spy(new DefaultMobGriefingConfigGui(minecraft, parentScreen));

    gui.init(minecraft, 0, 0);
    disableRendering();
  }

  @ParameterizedTest(name = "Should render buttons with correct state when initial value is {0}.")
  @ValueSource(booleans = {true, false})
  void shouldRenderButtonsWithCorrectStateWhenValueUnchanged(boolean initialValue)
      throws IllegalAccessException {
    // Given.
    TestUtils.initializeConfig(initialValue);
    reinitializeGui();

    // When.
    gui.render(0, 0, 0);

    // Then.
    Button resetButton = (Button) gui.children().get(1);
    assertThat("Unexpected reset button active state.", resetButton.active, is(false));

    Button defaultButton = (Button) gui.children().get(2);
    assertThat("Unexpected default button active state.", defaultButton.active, is(!initialValue));

    Button doneButton = (Button) gui.children().get(3);
    assertThat("Unexpected done button active state.", doneButton.active, is(true));
  }

  @ParameterizedTest(name = "Should render buttons with correct state when initial value is {0} and"
      + " the value is changed.")
  @ValueSource(booleans = {true, false})
  void shouldRenderButtonsWithCorrectStateWhenValueChanged(boolean initialValue)
      throws IllegalAccessException {
    // Given.
    TestUtils.initializeConfig(initialValue);
    reinitializeGui();

    BooleanConfigEntry globalEntry = (BooleanConfigEntry) entryList.children().get(1);
    Button globalValueButton = (Button) globalEntry.children().get(0);
    globalValueButton.onPress();

    // When.
    gui.render(0, 0, 0);

    // Then.
    Button resetButton = (Button) gui.children().get(1);
    assertThat("Unexpected reset button active state.", resetButton.active, is(true));

    Button defaultButton = (Button) gui.children().get(2);
    assertThat("Unexpected default button active state.", defaultButton.active, is(initialValue));

    Button doneButton = (Button) gui.children().get(3);
    assertThat("Unexpected done button active state.", doneButton.active, is(true));
  }

  @Test
  void shouldRestoreInitialValueWhenResetAllButtonPressed() throws IllegalAccessException {
    // Given.
    TestUtils.initializeConfig(true, MobGriefingValue.TRUE);
    reinitializeGui();

    BooleanConfigEntry globalEntry = (BooleanConfigEntry) entryList.children().get(1);
    Button globalValueButton = (Button) globalEntry.children().get(0);
    globalValueButton.onPress();

    MobGriefingValueConfigEntry entityEntry = (MobGriefingValueConfigEntry) entryList.children()
        .get(3);
    Button entityValueButton = (Button) entityEntry.children().get(0);
    entityValueButton.onPress();

    // When.
    Button resetButton = (Button) gui.children().get(1);
    resetButton.onPress();
    gui.render(0, 0, 0);

    // Then.
    assertThat("Unexpected value for isChanged.", globalEntry.isChanged(), is(false));
    assertThat("Unexpected value for isDefault.", globalEntry.isDefault(), is(true));
    assertThat("Unexpected value for isChanged.", entityEntry.isChanged(), is(false));
    assertThat("Unexpected value for isDefault.", entityEntry.isDefault(), is(false));

    assertThat("Unexpected reset button active state.", resetButton.active, is(false));

    Button defaultButton = (Button) gui.children().get(2);
    assertThat("Unexpected default button active state.", defaultButton.active, is(true));

    Button doneButton = (Button) gui.children().get(3);
    assertThat("Unexpected done button active state.", doneButton.active, is(true));
  }

  @Test
  void shouldRestoreDefaultValueWhenDefaultAllButtonPressed() throws IllegalAccessException {
    // Given.
    TestUtils.initializeConfig(false, MobGriefingValue.INHERIT);
    reinitializeGui();

    // When.
    Button defaultButton = (Button) gui.children().get(2);
    defaultButton.onPress();
    gui.render(0, 0, 0);

    // Then.
    BooleanConfigEntry globalEntry = (BooleanConfigEntry) entryList.children().get(1);
    assertThat("Unexpected value for isChanged.", globalEntry.isChanged(), is(true));
    assertThat("Unexpected value for isDefault.", globalEntry.isDefault(), is(true));

    MobGriefingValueConfigEntry entityEntry = (MobGriefingValueConfigEntry) entryList.children()
        .get(3);
    assertThat("Unexpected value for isChanged.", entityEntry.isChanged(), is(false));
    assertThat("Unexpected value for isDefault.", entityEntry.isDefault(), is(true));

    Button resetButton = (Button) gui.children().get(1);
    assertThat("Unexpected reset button active state.", resetButton.active, is(true));

    assertThat("Unexpected default button active state.", defaultButton.active, is(false));

    Button doneButton = (Button) gui.children().get(3);
    assertThat("Unexpected done button active state.", doneButton.active, is(true));
  }

  @Test
  void shouldNotUpdateConfigWhenConfigUnchangedAndDoneButtonPressed()
      throws IllegalAccessException {
    // Given.
    TestUtils.initializeConfig(true, MobGriefingValue.INHERIT, MobGriefingValue.INHERIT);
    reinitializeGui();

    // When.
    Button doneButton = (Button) gui.children().get(3);
    doneButton.onPress();

    // Then.
    assertThat("Unexpected value for global mobGriefing.", Config.defaultGlobalValue, is(true));

    MobGriefingValue mobGriefingValue = Config.entityIdsToDefaultEntityValue
        .get(new ResourceLocation("test:entity1"));
    assertThat("Unexpected value for entity mobGriefing.", mobGriefingValue,
        is(MobGriefingValue.INHERIT));

    mobGriefingValue = Config.entityIdsToDefaultEntityValue
        .get(new ResourceLocation("test:entity2"));
    assertThat("Unexpected value for entity mobGriefing.", mobGriefingValue,
        is(MobGriefingValue.INHERIT));
  }

  @Test
  void shouldUpdateConfigWhenConfigChangedAndDoneButtonPressed() throws IllegalAccessException {
    // Given.
    TestUtils.initializeConfig(true, MobGriefingValue.INHERIT, MobGriefingValue.INHERIT);
    reinitializeGui();

    BooleanConfigEntry globalEntry = (BooleanConfigEntry) entryList.children().get(1);
    Button globalValueButton = (Button) globalEntry.children().get(0);
    globalValueButton.onPress();

    MobGriefingValueConfigEntry entityEntry1 = (MobGriefingValueConfigEntry) entryList.children()
        .get(3);
    Button entityValueButton1 = (Button) entityEntry1.children().get(0);
    entityValueButton1.onPress();

    MobGriefingValueConfigEntry entityEntry2 = (MobGriefingValueConfigEntry) entryList.children()
        .get(4);
    Button entityValueButton2 = (Button) entityEntry2.children().get(0);
    entityValueButton2.onPress();
    entityValueButton2.onPress();

    // When.
    Button doneButton = (Button) gui.children().get(3);
    doneButton.onPress();

    // Then.
    assertThat("Unexpected value for global mobGriefing.", Config.defaultGlobalValue, is(false));

    MobGriefingValue mobGriefingValue = Config.entityIdsToDefaultEntityValue
        .get(new ResourceLocation("test:entity1"));
    assertThat("Unexpected value for entity mobGriefing.", mobGriefingValue,
        is(MobGriefingValue.FALSE));

    mobGriefingValue = Config.entityIdsToDefaultEntityValue
        .get(new ResourceLocation("test:entity2"));
    assertThat("Unexpected value for entity mobGriefing.", mobGriefingValue,
        is(MobGriefingValue.TRUE));
  }

  /**
   * Reinitialize the GUI so that the buttons can be recreated using the latest config values.
   *
   * @throws IllegalAccessException If rendering could not be disabled.
   */
  private void reinitializeGui() throws IllegalAccessException {
    gui.children().clear();
    gui.init();
    disableRendering();
  }

  /**
   * Disable the actual rendering of UI elements.
   *
   * @throws IllegalAccessException If rendering could not be disabled.
   */
  private void disableRendering() throws IllegalAccessException {
    // Swap entry list out for a spy.
    entryList = (ConfigEntryList) spy(gui.children().get(0));
    FieldUtils.writeField(gui, "configEntryList", entryList, true);

    // Swap buttons out for spies.
    Button resetButton = (Button) spy(gui.children().get(1));
    Button defaultButton = (Button) spy(gui.children().get(2));
    Button doneButton = (Button) spy(gui.children().get(3));
    List<Button> buttons = new ArrayList<>(Arrays.asList(resetButton, defaultButton, doneButton));
    FieldUtils.writeField(gui, "buttons", buttons, true);

    // Disable rendering of gui children.
    doNothing().when(gui).renderBackground();
    doNothing().when(entryList).render(anyInt(), anyInt(), anyFloat());
    doNothing().when(resetButton).render(anyInt(), anyInt(), anyFloat());
    doNothing().when(defaultButton).render(anyInt(), anyInt(), anyFloat());
    doNothing().when(doneButton).render(anyInt(), anyInt(), anyFloat());
  }
}
