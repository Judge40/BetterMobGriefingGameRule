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

import com.judge40.minecraft.bettermobgriefinggamerule.TestUtils;
import com.judge40.minecraft.bettermobgriefinggamerule.client.gui.DefaultMobGriefingConfigGui;
import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import java.lang.reflect.Field;
import java.util.List;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.screen.Screen;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class ConfigEntryListTest {

  private ConfigEntryList entryList;

  @BeforeAll
  static void setUpBeforeAll() throws IllegalAccessException {
    TestUtils.initializeTestEnvironment();
    TestUtils.initializeConfig(true, MobGriefingValue.FALSE, MobGriefingValue.TRUE,
        MobGriefingValue.INHERIT);
  }

  @BeforeEach
  void setUp() throws IllegalAccessException {
    Screen parentScreen = new DefaultMobGriefingConfigGui(null, null);
    parentScreen.width = 100;
    parentScreen.height = 200;

    Minecraft minecraft = mock(Minecraft.class);
    FontRenderer fontRenderer = mock(FontRenderer.class);
    Field fontRendererField = FieldUtils.getField(Minecraft.class, "font");
    FieldUtils.removeFinalModifier(fontRendererField);
    FieldUtils.writeField(fontRendererField, minecraft, fontRenderer, true);

    entryList = new ConfigEntryList(parentScreen, minecraft);
  }

  @Test
  void shouldSetGlobalEntryToConfigValue() {
    // When.
    BooleanConfigEntry globalEntry = entryList.getGlobalEntry();

    // Then.
    assertThat("Unexpected global value.", globalEntry.getCurrentValue(), is(true));
  }

  @Test
  void shouldSetEntityEntriesToConfigValues() {
    // When.
    List<MobGriefingValueConfigEntry> entityEntries = entryList.getEntityEntries();

    // Then.
    MobGriefingValueConfigEntry entityEntry = entityEntries.get(0);
    assertThat("Unexpected entity value.", entityEntry.getCurrentValue(),
        is(MobGriefingValue.FALSE));

    entityEntry = entityEntries.get(1);
    assertThat("Unexpected entity value.", entityEntry.getCurrentValue(),
        is(MobGriefingValue.TRUE));

    entityEntry = entityEntries.get(2);
    assertThat("Unexpected entity value.", entityEntry.getCurrentValue(),
        is(MobGriefingValue.INHERIT));
  }

  @Test
  void shouldReturnScrollbarPosition() {
    // When.
    int scrollbarPosition = entryList.getScrollbarPosition();

    // Then.
    assertThat("Unexpected scrollbar position.", scrollbarPosition, is(231));
  }

  @Test
  void shouldReturnRowWidth() {
    // When.
    int rowWidth = entryList.getRowWidth();

    // Then.
    assertThat("Unexpected row width.", rowWidth, is(270));
  }
}
