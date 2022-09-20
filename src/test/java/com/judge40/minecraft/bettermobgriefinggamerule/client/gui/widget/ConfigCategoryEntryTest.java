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
import static org.mockito.Mockito.when;

import com.mojang.blaze3d.vertex.PoseStack;
import java.util.List;
import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.components.events.GuiEventListener;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class ConfigCategoryEntryTest {

  private ConfigCategoryEntry entry;

  @BeforeEach
  void setUp() {
    Font font = mock(Font.class);
    entry = new ConfigCategoryEntry(font, 200, "labelKey");
  }

  @Test
  void shouldRenderCategoryLabel() {
    // Given.
    Font font = mock(Font.class);
    when(font.width("labelKey")).thenReturn(100);
    ConfigCategoryEntry entry = new ConfigCategoryEntry(font, 200, "labelKey");

    PoseStack poseStack = new PoseStack();

    // When.
    entry.render(poseStack, 10, 20, 30, 40, 50, 60, 70, true, 90);

    // Then.
    int colorCode = ChatFormatting.WHITE.getColor();
    verify(font).draw(poseStack, "labelKey", 50, 60, colorCode);
  }

  @ParameterizedTest(name = "Should not change focus when input is {0}.")
  @ValueSource(booleans = {true, false})
  void shouldNotChangeFocus(boolean input) {
    // When.
    boolean changeFocus = entry.changeFocus(input);

    // Then.
    assertThat("Unexpected value for changeFocus.", changeFocus, is(false));
  }

  @Test
  void shouldNotHaveChildren() {
    // When.
    List<? extends GuiEventListener> children = entry.children();

    // Then.
    assertThat("Unexpected number of children.", children.size(), is(0));
  }
}
