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

import com.judge40.minecraft.bettermobgriefinggamerule.client.gui.widget.AbstractConfigEntry;
import com.judge40.minecraft.bettermobgriefinggamerule.client.gui.widget.BooleanConfigEntry;
import com.judge40.minecraft.bettermobgriefinggamerule.client.gui.widget.ConfigEntryList;
import com.judge40.minecraft.bettermobgriefinggamerule.client.gui.widget.MobGriefingValueConfigEntry;
import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import com.judge40.minecraft.bettermobgriefinggamerule.common.ModInfoConstants;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.ConfigHelper;
import java.util.Map;
import java.util.stream.Collectors;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.screen.Screen;
import net.minecraft.client.gui.widget.button.Button;
import net.minecraft.client.gui.widget.list.AbstractOptionList.Entry;
import net.minecraft.client.resources.I18n;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.StringTextComponent;
import net.minecraftforge.fml.client.gui.widget.ExtendedButton;

/**
 * The configuration GUI for setting the default value of mob griefing rules.
 */
public class DefaultMobGriefingConfigGui extends Screen {

  private static final String DEFAULT_ALL = I18n
      .format("bettermobgriefinggamerule.config.gui.defaultAll");
  private static final String DONE = I18n.format("gui.done");
  private static final String RESET_ALL = I18n
      .format("bettermobgriefinggamerule.config.gui.resetAll");

  private static final int BUTTON_HEIGHT = 20;
  private static final int BUTTON_WIDTH = 75;

  private final Minecraft minecraft;
  private final Screen parent;
  private ConfigEntryList configEntryList;
  private Button resetButton;
  private Button defaultButton;

  /**
   * Constructor which initializes the configuration GUI with the configuration elements and title.
   *
   * @param parent The configuration GUI's parent screen.
   */
  public DefaultMobGriefingConfigGui(Minecraft minecraft, Screen parent) {
    super(new StringTextComponent(ModInfoConstants.DISPLAY_NAME));
    this.minecraft = minecraft;
    this.parent = parent;

  }

  @Override
  public void init() {
    configEntryList = new ConfigEntryList(this, minecraft);
    children.add(configEntryList);

    final int x = width / 2 - 155;
    final int y = height - 29;

    resetButton = addButton(new ExtendedButton(x, y, BUTTON_WIDTH, BUTTON_HEIGHT, RESET_ALL,
        (button) -> {
          for (Entry child : configEntryList.children()) {

            if (child instanceof AbstractConfigEntry) {
              ((AbstractConfigEntry) child).restoreInitialValue();
            }
          }
        }));

    defaultButton = addButton(
        new ExtendedButton(x + BUTTON_WIDTH, y, BUTTON_WIDTH, BUTTON_HEIGHT, DEFAULT_ALL,
            (button) -> {
              for (Entry child : configEntryList.children()) {

                if (child instanceof AbstractConfigEntry) {
                  ((AbstractConfigEntry) child).restoreDefaultValue();
                }
              }
            }));

    addButton(
        new ExtendedButton(x + 10 + BUTTON_WIDTH * 2, y, BUTTON_WIDTH * 2, BUTTON_HEIGHT, DONE,
            (button) -> {
              updateConfig();
              minecraft.displayGuiScreen(parent);
            }));
  }

  private void updateConfig() {
    BooleanConfigEntry globalEntry = configEntryList.getGlobalEntry();

    if (globalEntry.isChanged()) {
      ConfigHelper.updateGlobalMobGriefing(globalEntry.getCurrentValue());
    }

    Map<ResourceLocation, MobGriefingValue> entityIdsToChangedValue = configEntryList
        .getEntityEntries().stream()
        .filter(MobGriefingValueConfigEntry::isChanged)
        .collect(Collectors.toMap(MobGriefingValueConfigEntry::getEntityId,
            MobGriefingValueConfigEntry::getCurrentValue));

    if (entityIdsToChangedValue.size() > 0) {
      ConfigHelper.updateEntityMobGriefing(entityIdsToChangedValue);
    }
  }

  @Override
  public void render(int render1, int render2, float render3) {
    renderBackground();
    configEntryList.render(render1, render2, render3);
    drawCenteredString(font, title.getFormattedText(), width / 2, 8, 16777215);

    boolean enableReset = false;
    boolean enableDefault = false;

    for (Entry child : configEntryList.children()) {

      if (child instanceof AbstractConfigEntry) {
        enableReset |= ((AbstractConfigEntry) child).isChanged();
        enableDefault |= !((AbstractConfigEntry) child).isDefault();

        if (enableReset && enableDefault) {
          break;
        }
      }
    }

    resetButton.active = enableReset;
    defaultButton.active = enableDefault;
    super.render(render1, render2, render3);
  }
}
