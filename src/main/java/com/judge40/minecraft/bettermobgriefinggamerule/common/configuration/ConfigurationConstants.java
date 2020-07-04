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

package com.judge40.minecraft.bettermobgriefinggamerule.common.configuration;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.boss.WitherEntity;
import net.minecraft.entity.boss.dragon.EnderDragonEntity;
import net.minecraft.entity.merchant.villager.VillagerEntity;
import net.minecraft.entity.monster.BlazeEntity;
import net.minecraft.entity.monster.CreeperEntity;
import net.minecraft.entity.monster.EndermanEntity;
import net.minecraft.entity.monster.EvokerEntity;
import net.minecraft.entity.monster.GhastEntity;
import net.minecraft.entity.monster.HuskEntity;
import net.minecraft.entity.monster.SilverfishEntity;
import net.minecraft.entity.monster.ZombieEntity;
import net.minecraft.entity.monster.ZombiePigmanEntity;
import net.minecraft.entity.monster.ZombieVillagerEntity;
import net.minecraft.entity.passive.RabbitEntity;
import net.minecraft.entity.passive.SheepEntity;
import net.minecraft.entity.passive.SnowGolemEntity;

/**
 * The constants for mob griefing default value configuration information.
 */
public class ConfigurationConstants {

  // The constants for the localizable configuration UI message keys.
  public static final String GLOBAL_RULE_KEY = "bettermobgriefinggamerule.config.defaultGlobalRule";
  public static final String ENTITY_RULES_KEY =
      "bettermobgriefinggamerule.config.defaultEntityRules";

  // The constants for the configuration categories.
  public static final String GLOBAL_RULE_CATEGORY = "defaultglobalrule";
  public static final String ENTITY_RULES_CATEGORY = "defaultentityrules";

  // The entity classes with configuration support available by default.
  public static final List<Class<? extends LivingEntity>> ENTITY_CLASSES =
      Collections.unmodifiableList(Arrays.asList(BlazeEntity.class, CreeperEntity.class,
          EnderDragonEntity.class, EndermanEntity.class, EvokerEntity.class, GhastEntity.class,
          HuskEntity.class, ZombiePigmanEntity.class, RabbitEntity.class, SheepEntity.class,
          SilverfishEntity.class, SnowGolemEntity.class, VillagerEntity.class, WitherEntity.class,
          ZombieEntity.class, ZombieVillagerEntity.class));
}
