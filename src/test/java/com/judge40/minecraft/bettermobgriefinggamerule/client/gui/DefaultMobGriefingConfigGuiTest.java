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

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;
import com.judge40.minecraft.bettermobgriefinggamerule.common.configuration.ConfigurationConstants;
import com.judge40.minecraft.bettermobgriefinggamerule.common.configuration.DefaultMobGriefingConfiguration;

import mockit.Expectations;
import mockit.Mocked;
import net.minecraftforge.common.config.ConfigCategory;
import net.minecraftforge.fml.client.config.GuiConfig;
import net.minecraftforge.fml.common.FMLModContainer;
import net.minecraftforge.fml.common.Loader;
import org.junit.Test;

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
      @Mocked DefaultMobGriefingConfiguration configuration, @Mocked FMLModContainer modContainer,
      @Mocked Loader loader) {
    // Set up test data.
    BetterMobGriefingGameRule entryPoint = new BetterMobGriefingGameRule();

    // Record expectations.
    new Expectations(entryPoint, GuiConfig.class) {
      {
        BetterMobGriefingGameRule.getInstance();
        result = entryPoint;

        entryPoint.getDefaultMobGriefingConfiguration();
        result = configuration;

        configuration.getCategory(ConfigurationConstants.GLOBAL_RULE_CATEGORY);
        result = new ConfigCategory("globalCategoryName");

        configuration.getCategory(ConfigurationConstants.ENTITY_RULES_CATEGORY);
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
