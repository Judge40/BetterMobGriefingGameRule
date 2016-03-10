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
package com.judge40.minecraft.bettermobgriefinggamerulecore;

import java.util.Map;

import cpw.mods.fml.relauncher.IFMLLoadingPlugin;

/**
 * Loading plugin for BetterMobGriefingGameRule core mod
 */

@IFMLLoadingPlugin.Name(value = "Better mobGriefing GameRule Core")
@IFMLLoadingPlugin.MCVersion(value = "1.7.10")
@IFMLLoadingPlugin.TransformerExclusions(value = "com.judge40.minecraft.bettermobgriefingrulecore")
@IFMLLoadingPlugin.SortingIndex(Integer.MAX_VALUE)
public class BetterMobGriefingGameRuleIFMLLoadingPlugin implements IFMLLoadingPlugin {

  /*
   * (non-Javadoc)
   * 
   * @see cpw.mods.fml.relauncher.IFMLLoadingPlugin#getASMTransformerClass()
   */
  @Override
  public String[] getASMTransformerClass() {
    return new String[] {BetterMobGriefingGameRuleIClassTransformer.class.getName()};
  }

  /*
   * (non-Javadoc)
   * 
   * @see cpw.mods.fml.relauncher.IFMLLoadingPlugin#getModContainerClass()
   */
  @Override
  public String getModContainerClass() {
    return null;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cpw.mods.fml.relauncher.IFMLLoadingPlugin#getSetupClass()
   */
  @Override
  public String getSetupClass() {
    return null;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cpw.mods.fml.relauncher.IFMLLoadingPlugin#injectData(java.util.Map)
   */
  @Override
  public void injectData(Map<String, Object> data) {

  }

  /*
   * (non-Javadoc)
   * 
   * @see cpw.mods.fml.relauncher.IFMLLoadingPlugin#getAccessTransformerClass()
   */
  @Override
  public String getAccessTransformerClass() {
    return null;
  }
}
