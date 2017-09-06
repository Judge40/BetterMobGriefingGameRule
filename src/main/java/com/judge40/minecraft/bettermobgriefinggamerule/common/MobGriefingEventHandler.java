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
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.projectile.EntityFireball;
import net.minecraft.world.Explosion;
import net.minecraft.world.GameRules;
import net.minecraftforge.event.world.ExplosionEvent.Detonate;
import net.minecraftforge.fml.client.event.ConfigChangedEvent.OnConfigChangedEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.relauncher.ReflectionHelper;

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
    if (configChangedEvent.modID.equals(ModInfoConstants.ID)) {
      BetterMobGriefingGameRule.getInstance().getDefaultMobGriefingConfiguration().synchronize();
    }
  }

  /**
   * On the explosion detonate event check whether mob griefing is enabled for a specific entity and
   * update the relevant fields to reflect the desired value.
   * 
   * @param detonateEvent The detonate event for the explosion to update.
   */
  @SubscribeEvent
  public void onDetonate(Detonate detonateEvent) {
    // Try and get the explosion source from the explosion.
    Entity explosionSource = null;
    Entity exploder = ReflectionHelper.getPrivateValue(Explosion.class, detonateEvent.explosion,
        ObfuscationHelper.convertName("field_77283_e"));

    if (exploder instanceof EntityLiving) {
      explosionSource = exploder;
    } else if (exploder instanceof EntityFireball) {
      explosionSource = ((EntityFireball) exploder).shootingEntity;
    }

    // If the explosion source could not be determined from the explosion then check the affected
    // entities for a match.
    if (explosionSource == null) {
      for (Entity affectedEntity : detonateEvent.getAffectedEntities()) {
        if (affectedEntity instanceof EntityFireball) {
          EntityFireball entityFireball = (EntityFireball) affectedEntity;

          // Compare the fireball and explosion positions to determine if the fireball is the source
          // of the explosion.
          double explosionX = ReflectionHelper.getPrivateValue(Explosion.class,
              detonateEvent.explosion, ObfuscationHelper.convertName("field_77284_b"));
          double explosionY = ReflectionHelper.getPrivateValue(Explosion.class,
              detonateEvent.explosion, ObfuscationHelper.convertName("field_77285_c"));
          double explosionZ = ReflectionHelper.getPrivateValue(Explosion.class,
              detonateEvent.explosion, ObfuscationHelper.convertName("field_77282_d"));

          if (Math.abs(entityFireball.posX - explosionX) == 0
              && Math.abs(entityFireball.posY - explosionY) == 0
              && Math.abs(entityFireball.posZ - explosionZ) == 0
              && entityFireball.shootingEntity instanceof EntityLiving) {
            explosionSource = entityFireball.shootingEntity;
            break;
          }
        }
      }
    }

    if (explosionSource != null) {
      // Get whether mobGriefing is enabled for this entity
      boolean entityMobGriefing = BetterMobGriefingGameRule.isMobGriefingEnabled(explosionSource);

      GameRules gameRules = detonateEvent.world.getGameRules();
      boolean globalMobGriefing = gameRules.getBoolean(BetterMobGriefingGameRule.GLOBAL_RULE);

      // If entity mobGriefing has overridden the global value then update the explosion's flag.
      if (entityMobGriefing != globalMobGriefing) {
        ReflectionHelper.setPrivateValue(Explosion.class, detonateEvent.explosion,
            entityMobGriefing, ObfuscationHelper.convertName("field_82755_b"));

        // If mobGriefing is not enabled then clear down the affected blocks
        if (!entityMobGriefing) {
          detonateEvent.getAffectedBlocks().clear();
        }
      }
    }
  }
}
