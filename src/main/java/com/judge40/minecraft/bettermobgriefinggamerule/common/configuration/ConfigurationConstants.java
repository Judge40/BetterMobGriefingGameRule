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

import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.boss.EntityDragon;
import net.minecraft.entity.boss.EntityWither;
import net.minecraft.entity.monster.EntityBlaze;
import net.minecraft.entity.monster.EntityCreeper;
import net.minecraft.entity.monster.EntityEnderman;
import net.minecraft.entity.monster.EntityGhast;
import net.minecraft.entity.monster.EntityPigZombie;
import net.minecraft.entity.monster.EntitySilverfish;
import net.minecraft.entity.monster.EntitySnowman;
import net.minecraft.entity.monster.EntityZombie;
import net.minecraft.entity.passive.EntityRabbit;
import net.minecraft.entity.passive.EntitySheep;
import net.minecraft.entity.passive.EntityVillager;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

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
  public static final List<Class<? extends EntityLiving>> ENTITY_CLASSES =
      Collections.unmodifiableList(Arrays.asList(EntityBlaze.class, EntityCreeper.class,
          EntityDragon.class, EntityEnderman.class, EntityGhast.class, EntityPigZombie.class,
          EntityRabbit.class, EntitySheep.class, EntitySilverfish.class, EntitySnowman.class,
          EntityVillager.class, EntityWither.class, EntityZombie.class));
}
