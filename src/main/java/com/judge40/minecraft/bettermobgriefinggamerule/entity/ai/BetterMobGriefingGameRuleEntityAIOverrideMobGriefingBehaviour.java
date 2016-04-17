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
package com.judge40.minecraft.bettermobgriefinggamerule.entity.ai;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;

import cpw.mods.fml.common.ObfuscationReflectionHelper;
import net.minecraft.block.Block;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.ai.EntityAIBase;
import net.minecraft.entity.boss.EntityWither;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.MathHelper;

/**
 * Entity AI task which overrides mobGriefing behaviour where Forge events can not perform the
 * override
 */
public class BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour extends EntityAIBase {

  private EntityLiving entity;
  private int lastWitherBlockBreakCooldown = 20;

  /**
   * @param entity The EntityLiving for this AI task
   */
  public BetterMobGriefingGameRuleEntityAIOverrideMobGriefingBehaviour(EntityLiving entity) {
    this.entity = entity;
  }

  /*
   * (non-Javadoc)
   * 
   * @see net.minecraft.entity.ai.EntityAIBase#shouldExecute()
   */
  @Override
  public boolean shouldExecute() {
    return true;
  }

  /*
   * (non-Javadoc)
   * 
   * @see net.minecraft.entity.ai.EntityAIBase#startExecuting()
   */
  @Override
  public void updateTask() {
    if (entity instanceof EntityWither) {
      boolean betterMobGriefingEnabled = BetterMobGriefingGameRule.isMobGriefingEnabled(entity);

      boolean originalMobGriefingEnabled = entity.worldObj.getGameRules()
          .getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);

      if (originalMobGriefingEnabled ^ betterMobGriefingEnabled) {

        if (!betterMobGriefingEnabled) {
          // Increment field_82222_j (block break cooldown) to ensure it never gets to zero
          ObfuscationReflectionHelper.setPrivateValue(EntityWither.class, (EntityWither) entity, 20,
              "field_82222_j");
        } else {
          // Run the section of code from EntityWither which performs block damage

          if (((EntityWither) entity).func_82212_n() <= 0) {
            int blockBreakOnDamageCooldown = ObfuscationReflectionHelper
                .getPrivateValue(EntityWither.class, (EntityWither) entity, "field_82222_j");

            if (lastWitherBlockBreakCooldown != 0 && blockBreakOnDamageCooldown == 0) {
              int i = MathHelper.floor_double(entity.posY);
              int i1 = MathHelper.floor_double(entity.posX);
              int j1 = MathHelper.floor_double(entity.posZ);
              boolean flag = false;

              for (int l1 = -1; l1 <= 1; ++l1) {
                for (int i2 = -1; i2 <= 1; ++i2) {
                  for (int j = 0; j <= 3; ++j) {
                    int j2 = i1 + l1;
                    int k = i + j;
                    int l = j1 + i2;
                    Block block = entity.worldObj.getBlock(j2, k, l);

                    if (!block.isAir(entity.worldObj, j2, k, l)
                        && block.canEntityDestroy(entity.worldObj, j2, k, l, entity)) {
                      flag = entity.worldObj.func_147480_a(j2, k, l, true) || flag;
                    }
                  }
                }
              }

              if (flag) {
                entity.worldObj.playAuxSFXAtEntity((EntityPlayer) null, 1012, (int) entity.posX,
                    (int) entity.posY, (int) entity.posZ, 0);
              }
            }

            lastWitherBlockBreakCooldown = blockBreakOnDamageCooldown;
          }
        }
      }
    }
  }
}
