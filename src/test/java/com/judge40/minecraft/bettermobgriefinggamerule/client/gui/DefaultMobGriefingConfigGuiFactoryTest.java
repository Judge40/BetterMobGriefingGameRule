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

import java.util.Set;
import mockit.Mocked;
import net.minecraft.client.gui.GuiScreen;
import net.minecraftforge.fml.client.IModGuiFactory.RuntimeOptionCategoryElement;
import org.hamcrest.CoreMatchers;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * The unit tests for {@link DefaultMobGriefingConfigGuiFactory}.
 */
public class DefaultMobGriefingConfigGuiFactoryTest {

  private DefaultMobGriefingConfigGuiFactory factory;

  @Before
  public void setUp() {
    factory = new DefaultMobGriefingConfigGuiFactory();
  }

  /**
   * Test that a {@link DefaultMobGriefingConfigGui} is create for the configuration GUI.
   */
  @Test
  public void testCreateConfigGui_configGui(@Mocked DefaultMobGriefingConfigGui mockConfigGui) {
    // Call the method under test.
    GuiScreen configGui = factory.createConfigGui(null);

    // Perform assertions.
    Assert.assertThat("The configuration GUI returned did not match the expected class.", configGui,
        CoreMatchers.instanceOf(DefaultMobGriefingConfigGui.class));
  }

  /**
   * Test that true is returned for the hasConfigGui flag.
   */
  @Test
  public void testHasConfigGui_true() {
    // Call the method under test.
    boolean hasConfigGui = factory.hasConfigGui();

    // Perform assertions.
    Assert.assertThat("The hasConfigGui flag did not match the expected value.", hasConfigGui,
        CoreMatchers.is(true));
  }

  /**
   * This test added for code coverage only, the method under test is expected to do nothing.
   */
  @Test
  public void testInitialize_doNothing() {
    // Call the method under test.
    factory.initialize(null);
  }

  /**
   * Test that null is returned for the runtime GUI categories.
   */
  @Test
  public void testRuntimeGuiCategories_null() {
    // Call the method under test.
    Set<RuntimeOptionCategoryElement> categories = factory.runtimeGuiCategories();

    // Perform assertions.
    Assert.assertThat("The runtime GUI categories should be null.", categories,
        CoreMatchers.nullValue());
  }
}
