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

import cpw.mods.fml.client.IModGuiFactory.RuntimeOptionCategoryElement;
import cpw.mods.fml.client.IModGuiFactory.RuntimeOptionGuiHandler;
import net.minecraft.client.gui.GuiScreen;
import org.hamcrest.CoreMatchers;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Set;

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
   * Test that null is returned for the handler.
   */
  @Test
  public void testGetHandlerFor_null() {
    // Call the method under test.
    RuntimeOptionGuiHandler handler = factory.getHandlerFor(null);

    // Perform assertions.
    Assert.assertThat("The runtime option GUI handler should be null.", handler,
        CoreMatchers.nullValue());
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
   * Test that {@link DefaultMobGriefingConfigGui} is returned as the main configuration GUI class.
   */
  @Test
  public void testMainConfigGuiClass_configGuiClass() {
    // Call the method under test.
    Class<? extends GuiScreen> configGuiClass = factory.mainConfigGuiClass();

    // Perform assertions.
    Assert.assertThat("The main configuration GUI class returned did not match the expected class.",
        configGuiClass, CoreMatchers.equalTo(DefaultMobGriefingConfigGui.class));
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
