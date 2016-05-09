/*
 * Better mobGriefing GameRule Copyright (c) 2016 Judge40
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
package com.judge40.minecraft.bettermobgriefinggamerule.client;

import java.util.Collections;
import java.util.List;
import java.util.Set;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;

import cpw.mods.fml.client.IModGuiFactory;
import cpw.mods.fml.client.config.GuiConfig;
import cpw.mods.fml.client.config.IConfigElement;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraftforge.common.config.ConfigElement;

/**
 * The GUI factory for BetterMobGriefingGameRule configuration
 */
public class BetterMobGriefingGameRuleConfigGuiFactory implements IModGuiFactory {

  /*
   * (non-Javadoc)
   * 
   * @see cpw.mods.fml.client.IModGuiFactory#initialize(net.minecraft.client.Minecraft)
   */
  @Override
  public void initialize(Minecraft minecraftInstance) {

  }

  /*
   * (non-Javadoc)
   * 
   * @see cpw.mods.fml.client.IModGuiFactory#mainConfigGuiClass()
   */
  @Override
  public Class<? extends GuiScreen> mainConfigGuiClass() {
    return BetterMobGriefingGameRuleGuiConfig.class;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cpw.mods.fml.client.IModGuiFactory#runtimeGuiCategories()
   */
  @Override
  public Set<RuntimeOptionCategoryElement> runtimeGuiCategories() {
    return null;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cpw.mods.fml.client.IModGuiFactory#getHandlerFor(cpw.mods.fml.client.IModGuiFactory.
   * RuntimeOptionCategoryElement)
   */
  @Override
  public RuntimeOptionGuiHandler getHandlerFor(RuntimeOptionCategoryElement element) {
    return null;
  }

  /**
   * The GUI config for BetterMobGriefingGameRule configuration
   */
  public static class BetterMobGriefingGameRuleGuiConfig extends GuiConfig {

    public BetterMobGriefingGameRuleGuiConfig(GuiScreen parent) {
      super(parent, getConfigElements(), BetterMobGriefingGameRule.MODID, false, false,
          getAbridgedConfigPath(
              BetterMobGriefingGameRule.configuration.getConfigFile().toString()));
    }

    /**
     * Get the configuration elements to display on the configuration screen.
     * 
     * @return List of the configuration elements
     */
    @SuppressWarnings("rawtypes")
    private static List<IConfigElement> getConfigElements() {
      return Collections
          .singletonList(new ConfigElement<>(BetterMobGriefingGameRule.configuration.getCategory(
              BetterMobGriefingGameRule.DEFAULT_MOBGRIEFING_VALUES_CONFIGURATION_CATEGORY)));
    }
  }
}
