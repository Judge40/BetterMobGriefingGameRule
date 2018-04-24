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

package com.judge40.minecraft.bettermobgriefinggamerule.common;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;

import net.minecraft.entity.Entity;
import net.minecraftforge.event.entity.EntityMobGriefingEvent;
import net.minecraftforge.fml.client.event.ConfigChangedEvent.OnConfigChangedEvent;
import net.minecraftforge.fml.common.eventhandler.Event.Result;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;

/**
 * An event handler for all mob griefing events.
 */
public class MobGriefingEventHandler {

  /**
   * Synchronize the configuration changes when the configuration is changed.
   * 
   * @param configChangedEvent The OnConfigChangedEvent.
   */
  @SubscribeEvent
  public void onConfigChanged(OnConfigChangedEvent configChangedEvent) {
    if (configChangedEvent.getModID().equals(ModInfoConstants.ID)) {
      BetterMobGriefingGameRule.getInstance().getDefaultMobGriefingConfiguration().synchronize();
    }
  }

  /**
   * When mob griefing occurs, check whether mob griefing is enabled for the griefing entity and set
   * event's result accordingly. {@link Result#ALLOW} indicates that mob griefing is enabled and
   * {@link Result#DENY} indicates that mob griefing is disabled.
   * 
   * @param mobGriefingEvent The {@link EntityMobGriefingEvent} to update.
   */
  @SubscribeEvent
  public void onMobGriefing(EntityMobGriefingEvent mobGriefingEvent) {
    Entity griefingEntity = mobGriefingEvent.getEntity();

    if (griefingEntity != null) {
      boolean isEnabled = BetterMobGriefingGameRule.isMobGriefingEnabled(griefingEntity);

      if (isEnabled) {
        mobGriefingEvent.setResult(Result.ALLOW);
      } else {
        mobGriefingEvent.setResult(Result.DENY);
      }
    }
  }
}
