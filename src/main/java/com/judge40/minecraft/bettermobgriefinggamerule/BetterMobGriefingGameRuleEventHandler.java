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
package com.judge40.minecraft.bettermobgriefinggamerule;

import cpw.mods.fml.common.ObfuscationReflectionHelper;
import cpw.mods.fml.common.eventhandler.SubscribeEvent;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.world.Explosion;
import net.minecraft.world.GameRules;
import net.minecraftforge.event.world.ExplosionEvent.Detonate;

/**
 * Event handler for all EVENT_BUS events
 */
public class BetterMobGriefingGameRuleEventHandler {

  /**
   * On the explosion detonate event check whether mobGriefing is enabled for a specific entity and
   * update the relevant fields to reflect the desired value
   * 
   * @param detonateEvent The detonate event for the explosion to update
   */
  @SubscribeEvent
  public void onDetonateEvent(Detonate detonateEvent) {
    GameRules gameRules = detonateEvent.world.getGameRules();

    // Get whether mobGriefing is enabled for this entity
    EntityLivingBase entity = detonateEvent.explosion.getExplosivePlacedBy();
    String mobGriefingRule = BetterMobGriefingGameRule.getMobGriefingRule(gameRules, entity);
    boolean mobGriefingEnabled = gameRules.getGameRuleBooleanValue(mobGriefingRule);

    boolean mobGriefingOriginal =
        gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);

    // If better mobGriefing has overridden the default value then update relevant flag
    if (mobGriefingEnabled != mobGriefingOriginal) {
      ObfuscationReflectionHelper.setPrivateValue(Explosion.class, detonateEvent.explosion,
          mobGriefingEnabled, "isSmoking", "field_82755_b");
    }

    // If mobGriefing is not enabled then clear down the affected blocks
    if (!mobGriefingEnabled) {
      detonateEvent.getAffectedBlocks().clear();
    }
  }
}
