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
import net.minecraft.entity.ai.EntityAIEatGrass;
import net.minecraft.init.Blocks;
import net.minecraft.util.MathHelper;
import net.minecraft.world.World;

/**
 *
 */
public class BetterMobGriefingGameRuleEntityAIEatGrass extends EntityAIEatGrass {

  private EntityLiving entityLiving;
  private World world;

  /**
   * @param p_i45314_1_
   */
  public BetterMobGriefingGameRuleEntityAIEatGrass(EntityLiving p_i45314_1_) {
    super(p_i45314_1_);
    entityLiving = p_i45314_1_;
    world = p_i45314_1_.worldObj;
  }

  /*
   * (non-Javadoc)
   * 
   * @see net.minecraft.entity.ai.EntityAIEatGrass#updateTask()
   */
  @Override
  public void updateTask() {
    int field_151502_a =
        ObfuscationReflectionHelper.getPrivateValue(EntityAIEatGrass.class, this, "field_151502_a");

    field_151502_a = Math.max(0, field_151502_a - 1);

    if (field_151502_a == 4) {
      int i = MathHelper.floor_double(entityLiving.posX);
      int j = MathHelper.floor_double(entityLiving.posY);
      int k = MathHelper.floor_double(entityLiving.posZ);

      // Get whether mobGriefing is enabled for this entity
      boolean mobGriefingEnabled = BetterMobGriefingGameRule.isMobGriefingEnabled(entityLiving);


      if (world.getBlock(i, j, k) == Blocks.tallgrass) {
        if (mobGriefingEnabled) {
          world.func_147480_a(i, j, k, false);
        }

        entityLiving.eatGrassBonus();
      } else if (world.getBlock(i, j - 1, k) == Blocks.grass) {
        if (mobGriefingEnabled) {
          world.playAuxSFX(2001, i, j - 1, k, Block.getIdFromBlock(Blocks.grass));
          world.setBlock(i, j - 1, k, Blocks.dirt, 0, 2);
        }

        entityLiving.eatGrassBonus();
      }
    }

    ObfuscationReflectionHelper.setPrivateValue(EntityAIEatGrass.class, this, field_151502_a,
        "field_151502_a");
  }
}
