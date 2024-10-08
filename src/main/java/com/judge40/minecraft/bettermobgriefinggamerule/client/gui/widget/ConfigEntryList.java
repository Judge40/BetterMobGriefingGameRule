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

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;
import com.judge40.minecraft.bettermobgriefinggamerule.common.ModInfoConstants;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.Config;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.components.ContainerObjectSelectionList;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class ConfigEntryList extends ContainerObjectSelectionList<AbstractEntry> {

  private BooleanConfigEntry globalEntry;
  private List<MobGriefingValueConfigEntry> entityEntries = new ArrayList<>();

  /**
   * Create a list of configuration entries, including global mobGriefing and entity specific rules
   * set in the configuration file.
   *
   * @param parent    The parent screen to add the list to.
   * @param minecraft The minecraft instance.
   */
  public ConfigEntryList(Screen parent, Minecraft minecraft) {
    super(minecraft, parent.width + 45, parent.height, 43, parent.height - 32, 20);
    Font font = minecraft.font;

    int globalLabelWidth = font.width(BetterMobGriefingGameRule.GLOBAL_RULE);

    final int maxLabelWidth = Config.entityIdsToDefaultEntityValue.keySet().stream()
        .mapToInt(entityId -> font.width(entityId.toString()))
        .filter(width -> width > globalLabelWidth)
        .max()
        .orElse(globalLabelWidth);

    addEntry(new ConfigCategoryEntry(font, width,
        ModInfoConstants.ID + ".config.defaultGlobalRule.title"));
    globalEntry = new BooleanConfigEntry(font, maxLabelWidth,
        BetterMobGriefingGameRule.GLOBAL_RULE, Config.defaultGlobalValue, true);
    addEntry(globalEntry);

    addEntry(new ConfigCategoryEntry(font, width,
        ModInfoConstants.ID + ".config.defaultEntityRules.title"));
    Config.entityIdsToDefaultEntityValue.entrySet().stream()
        .sorted(Map.Entry.comparingByKey(Comparator.comparing(ResourceLocation::toString)))
        .forEach(entry -> {
          MobGriefingValueConfigEntry entityEntry = new MobGriefingValueConfigEntry(font,
              maxLabelWidth, entry.getKey(), entry.getValue());
          entityEntries.add(entityEntry);
          addEntry(entityEntry);
        });
  }

  public BooleanConfigEntry getGlobalEntry() {
    return globalEntry;
  }

  public List<MobGriefingValueConfigEntry> getEntityEntries() {
    return entityEntries;
  }

  @Override
  protected int getScrollbarPosition() {
    return super.getScrollbarPosition() + 35;
  }

  @Override
  public int getRowWidth() {
    return super.getRowWidth() + 50;
  }
}
