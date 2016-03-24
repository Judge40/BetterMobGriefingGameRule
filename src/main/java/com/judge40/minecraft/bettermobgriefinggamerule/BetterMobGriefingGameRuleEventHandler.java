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

import java.util.Iterator;
import java.util.Random;

import org.apache.commons.lang3.tuple.ImmutablePair;

import com.judge40.minecraft.bettermobgriefinggamerule.entity.ai.BetterMobGriefingGameRuleEntityAIBreakDoor;
import com.judge40.minecraft.bettermobgriefinggamerule.entity.ai.BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour;

import cpw.mods.fml.common.ObfuscationReflectionHelper;
import cpw.mods.fml.common.eventhandler.SubscribeEvent;
import net.minecraft.block.Block;
import net.minecraft.block.BlockSilverfish;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.ai.EntityAIBreakDoor;
import net.minecraft.entity.ai.EntityAITasks.EntityAITaskEntry;
import net.minecraft.entity.boss.EntityWither;
import net.minecraft.entity.monster.EntitySilverfish;
import net.minecraft.entity.monster.EntityZombie;
import net.minecraft.entity.projectile.EntityFireball;
import net.minecraft.init.Blocks;
import net.minecraft.util.MathHelper;
import net.minecraft.world.Explosion;
import net.minecraft.world.GameRules;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.entity.living.LivingEvent.LivingUpdateEvent;
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

    // Get explosion source entity
    Entity exploder = ObfuscationReflectionHelper.getPrivateValue(Explosion.class,
        detonateEvent.explosion, "exploder", "field_77283_e");
    EntityLivingBase entity = null;

    if (exploder instanceof EntityLivingBase) {
      entity = (EntityLivingBase) exploder;
    } else if (exploder instanceof EntityFireball) {
      entity = ((EntityFireball) exploder).shootingEntity;
    }

    // If entity is null then the explosion source might be a fireball without shootingEntity
    // populated, check the affected entities for a matching fireball and retrieve the entity from
    // it
    if (entity == null) {
      for (Entity affectedEntity : detonateEvent.getAffectedEntities()) {
        if (affectedEntity instanceof EntityFireball) {
          EntityFireball entityFireball = (EntityFireball) affectedEntity;

          // Compare the fireball and explosion positions to determine if the fireball is the source
          // of the explosion
          if (entityFireball.posX == detonateEvent.explosion.explosionX
              && entityFireball.posY == detonateEvent.explosion.explosionY
              && entityFireball.posZ == detonateEvent.explosion.explosionZ) {
            entity = entityFireball.shootingEntity;
            break;
          }
        }
      }
    }

    // Get whether mobGriefing is enabled for this entity
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

  /**
   * When an Entity joins the world update the relevant fields and tasks to allow the new
   * mobGriefing game rule to override the original rule
   * 
   * @param entityJoinWorldEvent The EntityJoinWorldEvent for the Entity to be updated
   */
  @SubscribeEvent
  public void onEntityJoinWorldEvent(EntityJoinWorldEvent entityJoinWorldEvent) {

    if (entityJoinWorldEvent.entity instanceof EntityWither) {
      EntityWither entityWither = (EntityWither) entityJoinWorldEvent.entity;
      entityWither.tasks.addTask(0,
          new BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour(entityWither));
    } else if (entityJoinWorldEvent.entity instanceof EntityZombie) {
      EntityZombie entityZombie = (EntityZombie) entityJoinWorldEvent.entity;

      // Get existing "BreakDoor" task
      EntityAIBreakDoor entityAiBreakDoor = ObfuscationReflectionHelper
          .getPrivateValue(EntityZombie.class, entityZombie, "field_146075_bs");

      // Iterate through the entities tasks and get the priority of the "BreakDoor" task if it
      // exists
      Iterator<?> entityAiTaskEntryIterator = entityZombie.tasks.taskEntries.iterator();
      int priority = -1;

      while (entityAiTaskEntryIterator.hasNext()) {
        EntityAITaskEntry entityAiTaskEntry = (EntityAITaskEntry) entityAiTaskEntryIterator.next();

        if (entityAiTaskEntry.action == entityAiBreakDoor) {
          priority = entityAiTaskEntry.priority;
          break;
        }
      }

      // Set the new "BreakDoor" task so that it gets populated instead of the original task
      BetterMobGriefingGameRuleEntityAIBreakDoor betterMobGriefingGameRuleEntityAiBreakDoor =
          new BetterMobGriefingGameRuleEntityAIBreakDoor(entityZombie);
      ObfuscationReflectionHelper.setPrivateValue(EntityZombie.class, entityZombie,
          betterMobGriefingGameRuleEntityAiBreakDoor, "field_146075_bs");

      // If the task was already populated then remove it and add the new task
      if (priority > -1) {
        entityZombie.tasks.removeTask(entityAiBreakDoor);
        entityZombie.tasks.addTask(priority, betterMobGriefingGameRuleEntityAiBreakDoor);
      }
    }
  }

  /**
   * Handle any mob griefing behaviour which must be overridden when onLivingUpdate runs.
   */
  @SubscribeEvent
  public void onLivingUpdateEvent(LivingUpdateEvent livingUpdateEvent) {
    if (livingUpdateEvent.entityLiving instanceof EntitySilverfish) {
      handleSilverfishSummonAlly((EntitySilverfish) livingUpdateEvent.entityLiving);
    }
  }

  /**
   * A copy of the EntitySilverfish ally summon code from updateEntityActionState, the original code
   * will be disabled and this copy will run instead. The only differences are those required to
   * disable the original code and the usage of the better mobGriefing game rule.
   * 
   * @param entitySilverfish The EntitySilverfish to handle summoning of allies for.
   */
  private static void handleSilverfishSummonAlly(EntitySilverfish entitySilverfish) {
    if (entitySilverfish.worldObj.isRemote) {
      return;
    }

    int allySummonCooldown = ObfuscationReflectionHelper.getPrivateValue(EntitySilverfish.class,
        entitySilverfish, "allySummonCooldown", "field_70843_d");

    if (allySummonCooldown != -1) {
      ObfuscationReflectionHelper.setPrivateValue(EntitySilverfish.class, entitySilverfish, -1,
          "allySummonCooldown", "field_70843_d");
    } else if (entitySilverfish.getEntityData().hasKey("bettermobgriefinggamerulecooldown")) {
      allySummonCooldown =
          entitySilverfish.getEntityData().getInteger("bettermobgriefinggamerulecooldown");
    }

    if (allySummonCooldown > 0) {
      allySummonCooldown--;

      if (allySummonCooldown == 0) {
        int i = MathHelper.floor_double(entitySilverfish.posX);
        int j = MathHelper.floor_double(entitySilverfish.posY);
        int k = MathHelper.floor_double(entitySilverfish.posZ);
        boolean flag = false;

        for (int l = 0; !flag && l <= 5 && l >= -5; l = l <= 0 ? 1 - l : 0 - l) {
          for (int i1 = 0; !flag && i1 <= 10 && i1 >= -10; i1 = i1 <= 0 ? 1 - i1 : 0 - i1) {
            for (int j1 = 0; !flag && j1 <= 10 && j1 >= -10; j1 = j1 <= 0 ? 1 - j1 : 0 - j1) {
              if (entitySilverfish.worldObj.getBlock(i + i1, j + l, k + j1) == Blocks.monster_egg) {

                String mobGriefingRule = BetterMobGriefingGameRule
                    .getMobGriefingRule(entitySilverfish.worldObj.getGameRules(), entitySilverfish);
                boolean mobGriefingEnabled = entitySilverfish.worldObj.getGameRules()
                    .getGameRuleBooleanValue(mobGriefingRule);

                if (!mobGriefingEnabled) {
                  int k1 = entitySilverfish.worldObj.getBlockMetadata(i + i1, j + l, k + j1);
                  ImmutablePair<?, ?> immutablepair = BlockSilverfish.func_150197_b(k1);
                  entitySilverfish.worldObj.setBlock(i + i1, j + l, k + j1,
                      (Block) immutablepair.getLeft(),
                      ((Integer) immutablepair.getRight()).intValue(), 3);
                } else {
                  entitySilverfish.worldObj.func_147480_a(i + i1, j + l, k + j1, false);
                }

                Blocks.monster_egg.onBlockDestroyedByPlayer(entitySilverfish.worldObj, i + i1,
                    j + l, k + j1, 0);

                Random rand = ObfuscationReflectionHelper.getPrivateValue(Entity.class,
                    entitySilverfish, "rand", "field_70146_Z");

                if (rand.nextBoolean()) {
                  flag = true;
                  break;
                }
              }
            }
          }
        }
      }
    }
    entitySilverfish.getEntityData().setInteger("bettermobgriefinggamerulecooldown",
        allySummonCooldown);
  }
}
