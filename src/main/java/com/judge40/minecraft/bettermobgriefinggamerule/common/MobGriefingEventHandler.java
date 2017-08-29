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
import com.judge40.minecraft.bettermobgriefinggamerule.common.entity.ai.BetterBreakDoorAiTask;

import cpw.mods.fml.client.event.ConfigChangedEvent.OnConfigChangedEvent;
import cpw.mods.fml.common.eventhandler.SubscribeEvent;
import cpw.mods.fml.relauncher.ReflectionHelper;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.ai.EntityAIBreakDoor;
import net.minecraft.entity.ai.EntityAITasks.EntityAITaskEntry;
import net.minecraft.entity.monster.EntityZombie;
import net.minecraft.entity.projectile.EntityFireball;
import net.minecraft.world.GameRules;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.world.ExplosionEvent.Detonate;

import java.util.Iterator;

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
    Entity explosionSource = null;

    // Try and get the explosion source from the explosion.
    if (detonateEvent.explosion.exploder instanceof EntityLiving) {
      explosionSource = detonateEvent.explosion.exploder;
    } else if (detonateEvent.explosion.exploder instanceof EntityFireball) {
      explosionSource = ((EntityFireball) detonateEvent.explosion.exploder).shootingEntity;
    }

    // If the explosion source could not be determined from the explosion then check the affected
    // entities for a match.
    if (explosionSource == null) {
      for (Entity affectedEntity : detonateEvent.getAffectedEntities()) {
        if (affectedEntity instanceof EntityFireball) {
          EntityFireball entityFireball = (EntityFireball) affectedEntity;

          // Compare the fireball and explosion positions to determine if the fireball is the source
          // of the explosion.
          if (Math.abs(entityFireball.posX - detonateEvent.explosion.explosionX) == 0
              && Math.abs(entityFireball.posY - detonateEvent.explosion.explosionY) == 0
              && Math.abs(entityFireball.posZ - detonateEvent.explosion.explosionZ) == 0
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
      boolean globalMobGriefing =
          gameRules.getGameRuleBooleanValue(BetterMobGriefingGameRule.GLOBAL_RULE);

      // If entity mobGriefing has overridden the global value then update the explosion's flag.
      if (entityMobGriefing != globalMobGriefing) {
        detonateEvent.explosion.isSmoking = entityMobGriefing;

        // If mobGriefing is not enabled then clear down the affected blocks
        if (!entityMobGriefing) {
          detonateEvent.getAffectedBlocks().clear();
        }
      }
    }
  }

  /**
   * When an {@link Entity} joins the world, update the relevant fields and tasks to allow the new
   * mobGriefing game rules to override the original rule.
   * 
   * @param event The {@link EntityJoinWorldEvent} for the {@code Entity} to be updated.
   */
  @SubscribeEvent
  public void onEntityJoinWorld(EntityJoinWorldEvent event) {
    if (event.entity instanceof EntityZombie) {
      EntityZombie zombie = (EntityZombie) event.entity;

      // Get the existing break door AI task.
      String breakDoorAiField = ObfuscationHelper.convertName("field_146075_bs");
      EntityAIBreakDoor breakDoorAiTask =
          ReflectionHelper.getPrivateValue(EntityZombie.class, zombie, breakDoorAiField);

      // Iterate through the entity's tasks and get the priority of the break door AI task if it
      // exists.
      int taskPriority = -1;

      for (Iterator<?> iterator = zombie.tasks.taskEntries.iterator(); iterator.hasNext();) {
        EntityAITaskEntry aiTaskEntry = (EntityAITaskEntry) iterator.next();

        if (aiTaskEntry.action.equals(breakDoorAiTask)) {
          taskPriority = aiTaskEntry.priority;
          break;
        }
      }

      // Set the new break door AI task so that it gets populated instead of the original task.
      BetterBreakDoorAiTask betterBreakDoorAiTask = new BetterBreakDoorAiTask(zombie);
      ReflectionHelper.setPrivateValue(EntityZombie.class, zombie, betterBreakDoorAiTask,
          breakDoorAiField);

      // If the break door AI task was already populated then remove it and add the new AI task.
      if (taskPriority > -1) {
        zombie.tasks.removeTask(breakDoorAiTask);
        zombie.tasks.addTask(taskPriority, betterBreakDoorAiTask);
      }
    }
  }
}
