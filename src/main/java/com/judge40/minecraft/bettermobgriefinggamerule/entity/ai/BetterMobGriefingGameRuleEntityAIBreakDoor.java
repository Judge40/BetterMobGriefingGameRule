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

import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.ai.EntityAIBreakDoor;

/**
 * Extension of EntityAIBreakDoor to allow new mobGriefing game rules to be used instead of the
 * original rule
 */
public class BetterMobGriefingGameRuleEntityAIBreakDoor extends EntityAIBreakDoor {

  /**
   * @param p_i1618_1_ The EntityLiving for this AI task
   */
  public BetterMobGriefingGameRuleEntityAIBreakDoor(EntityLiving p_i1618_1_) {
    super(p_i1618_1_);
  }

  /*
   * (non-Javadoc)
   * 
   * @see net.minecraft.entity.ai.EntityAIBreakDoor#shouldExecute()
   */
  @Override
  public boolean shouldExecute() {
    boolean shouldExecute = super.shouldExecute();

    // Get whether mobGriefing is enabled for this entity
    String mobGriefingRule =
        BetterMobGriefingGameRule.getMobGriefingRule(theEntity.worldObj.getGameRules(), theEntity);
    boolean mobGriefingEnabled =
        theEntity.worldObj.getGameRules().getGameRuleBooleanValue(mobGriefingRule);

    // If shouldExecute from parent class is false and mobGriefing is true then also check
    // func_150015_f again as that might have been the cause of a false result and should not be
    // overridden
    if (!shouldExecute && mobGriefingEnabled) {
      shouldExecute = mobGriefingEnabled && field_151504_e != null
          && !field_151504_e.func_150015_f(theEntity.worldObj, entityPosX, entityPosY, entityPosZ);
    } else {
      shouldExecute = mobGriefingEnabled;
    }

    return shouldExecute;
  }
}
