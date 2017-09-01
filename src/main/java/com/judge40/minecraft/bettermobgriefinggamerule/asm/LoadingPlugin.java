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

package com.judge40.minecraft.bettermobgriefinggamerule.asm;

import com.judge40.minecraft.bettermobgriefinggamerule.common.ModInfoConstants;

import net.minecraftforge.fml.relauncher.IFMLLoadingPlugin;

import java.util.Map;

/**
 * The loading plugin for the BetterMobGriefingGameRule core mod.
 */
@IFMLLoadingPlugin.Name(value = ModInfoConstants.DISPLAY_NAME_CORE)
@IFMLLoadingPlugin.MCVersion(value = ModInfoConstants.MINECRAFT_VERSION)
@IFMLLoadingPlugin.TransformerExclusions(value = ModInfoConstants.BASE_PACKAGE)
@IFMLLoadingPlugin.SortingIndex(Integer.MAX_VALUE)
public class LoadingPlugin implements IFMLLoadingPlugin {

  @Override
  public String getAccessTransformerClass() {
    return null;
  }

  @Override
  public String[] getASMTransformerClass() {
    return new String[] {ClassTransformer.class.getName()};
  }

  @Override
  public String getModContainerClass() {
    return null;
  }

  @Override
  public String getSetupClass() {
    return null;
  }

  @Override
  public void injectData(Map<String, Object> data) {

  }
}
