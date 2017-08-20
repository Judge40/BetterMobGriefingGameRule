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

import java.util.Arrays;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;
import com.judge40.minecraft.bettermobgriefinggamerule.ModInfoConstants;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.DefaultMobGriefingConfiguration;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.DefaultMobGriefingConfigurationConstants;

import cpw.mods.fml.client.config.GuiConfig;
import net.minecraft.client.gui.GuiScreen;
import net.minecraftforge.common.config.ConfigElement;

/**
 * The configuration GUI for setting the default value of mobGriefing rules.
 */
public class DefaultMobGriefingConfigGui extends GuiConfig {

  /**
   * Constructor which initializes the configuration GUI with the configuration elements and title.
   * 
   * @param parent The configuration GUI's parent screen.
   */
  public DefaultMobGriefingConfigGui(GuiScreen parent) {
    super(parent,
        Arrays.asList(
            new ConfigElement<>(getConfiguration()
                .getCategory(DefaultMobGriefingConfigurationConstants.GLOBAL_RULE_CATEGORY)),
            new ConfigElement<>(getConfiguration()
                .getCategory(DefaultMobGriefingConfigurationConstants.ENTITY_RULES_CATEGORY))),
        ModInfoConstants.ID, false, false, getAbridgedConfigPath(getConfiguration().toString()));
  }

  /**
   * Get the {@link DefaultMobGriefingConfiguration} from the {@link BetterMobGriefingGameRule}
   * instance.
   * 
   * @return The configuration.
   */
  private static DefaultMobGriefingConfiguration getConfiguration() {
    BetterMobGriefingGameRule entryPoint = BetterMobGriefingGameRule.getInstance();
    return entryPoint.getDefaultMobGriefingConfiguration();
  }
}
