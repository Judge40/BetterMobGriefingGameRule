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

import org.junit.Test;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.DefaultMobGriefingConfiguration;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.DefaultMobGriefingConfigurationConstants;

import cpw.mods.fml.client.config.GuiConfig;
import mockit.Expectations;
import mockit.Mocked;
import net.minecraftforge.common.config.ConfigCategory;

/**
 * The unit tests for {@link DefaultMobGriefingConfigGui}.
 */
public class DefaultMobGriefingConfigGuiTest {

  /**
   * Test that the constructor retrieves the expected configuration category and builds the title
   * correctly.
   */
  @Test
  public void testConstructor_configurationElementsAndTitleRetrieved(
      @Mocked DefaultMobGriefingConfiguration configuration) {
    // Set up test data.
    BetterMobGriefingGameRule.configuration = configuration;

    // Record expectations.
    new Expectations(GuiConfig.class) {
      {
        configuration.getCategory(DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY);
        result = new ConfigCategory("globalCategoryName");

        configuration.getCategory(DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY);
        result = new ConfigCategory("entityCategoryName");

        configuration.toString();
        result = "parent-folder/.minecraft/config-folder/config-file.cfg";

        GuiConfig.getAbridgedConfigPath("parent-folder/.minecraft/config-folder/config-file.cfg");
        result = "/.minecraft/config-folder/config-file.cfg";
      }
    };

    // Call the constructor under test.
    new DefaultMobGriefingConfigGui(null);
  }
}
