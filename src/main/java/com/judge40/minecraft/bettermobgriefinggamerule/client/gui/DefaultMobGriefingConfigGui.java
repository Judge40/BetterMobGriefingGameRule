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
import com.judge40.minecraft.bettermobgriefinggamerule.common.ModInfoConstants;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.Config;
import java.util.Collections;
import java.util.List;
import net.minecraft.client.gui.screen.Screen;
import net.minecraft.util.text.StringTextComponent;
import net.minecraftforge.fml.client.config.IConfigElement;

/**
 * The configuration GUI for setting the default value of mob griefing rules.
 */
public class DefaultMobGriefingConfigGui extends Screen {

  /**
   * Constructor which initializes the configuration GUI with the configuration elements and title.
   *
   * @param parent The configuration GUI's parent screen.
   */
  public DefaultMobGriefingConfigGui(Screen parent) {
    super(new StringTextComponent(ModInfoConstants.DISPLAY_NAME));
//    super(parent, getConfigurationCategories(), ModInfoConstants.ID, false, false,
//        getAbridgedConfigPath(getConfiguration().toString()));
  }

  /**
   * Get the list of configuration categories for the mod's configuration.
   *
   * @return The configuration categories.
   */
  private static List<IConfigElement> getConfigurationCategories() {
//    DefaultMobGriefingConfiguration configuration = getConfiguration();
//    DummyConfigElement globalCategory =
//        new ConfigElement(configuration.getCategory(ConfigurationConstants.GLOBAL_RULE_CATEGORY));
//    ConfigElement entityCategory =
//        new ConfigElement(configuration.getCategory(ConfigurationConstants.ENTITY_RULES_CATEGORY));
//    List<IConfigElement> configurationCategories = Arrays.asList(globalCategory, entityCategory);
//
//    return configurationCategories;
    return Collections.emptyList();
  }
}
