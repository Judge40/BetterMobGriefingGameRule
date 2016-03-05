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

import java.util.Arrays;
import java.util.List;

import cpw.mods.fml.common.Mod;
import cpw.mods.fml.common.Mod.EventHandler;
import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLServerStartedEvent;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.monster.EntityCreeper;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.GameRules;
import net.minecraftforge.common.MinecraftForge;

/**
 * Base class for 'Better mobGriefing GameRule' mod
 */
@Mod(modid = BetterMobGriefingGameRule.MODID, name = BetterMobGriefingGameRule.NAME,
    version = BetterMobGriefingGameRule.VERSION)
public class BetterMobGriefingGameRule {

  // Constants for mod attributes
  public static final String MODID = "bettermobgriefinggamerule";
  public static final String NAME = "Better mobGriefing GameRule";
  public static final String VERSION = "1.7.10-0.1.0";

  // Constants for the mobGriefing rules
  public static final String ORIGINAL = "mobGriefing";
  public static final String CREEPER = "mobGriefingCreeper";

  public static final List<String> MOBGRIEFING_GAME_RULES = Arrays.asList(CREEPER);

  /**
   * On initialisation registers the event handler
   * 
   * @param event The FMLInitializationEvent
   */
  @EventHandler
  public void onFMLInitializationEvent(FMLInitializationEvent initializationEvent) {
    MinecraftForge.EVENT_BUS.register(new BetterMobGriefingGameRuleEventHandler());
  }

  /**
   * On server start add new mobGriefing game rules
   * 
   * @param event The FMLServerStartedEvent
   */
  @EventHandler()
  public void onFMLServerStartedEvent(FMLServerStartedEvent serverStartedEvent) {
    addMobGriefingGameRules(MinecraftServer.getServer().getEntityWorld().getGameRules());
  }

  /**
   * Add mobGriefing game rules
   * 
   * @param gameRules The GameRules to add to
   */
  public static void addMobGriefingGameRules(GameRules gameRules) {
    for (String mobGriefingGameRule : MOBGRIEFING_GAME_RULES) {
      addMobGriefingGameRule(gameRules, mobGriefingGameRule);
    }
  }

  /**
   * Add the mobGriefing rule if it does not already exist. The default value will be set to match
   * the current value of the original mobGriefing rule.
   * 
   * @param gameRules The GameRules to add to
   * @param gameRule The rule to add
   */
  public static void addMobGriefingGameRule(GameRules gameRules, String gameRule) {
    // Only add the rule if it does not already exist, avoids overwriting user defined values
    if (!gameRules.hasRule(gameRule)) {
      // Set the current original mobGriefing value on new rules, ensures no behavioural change
      // without user interaction
      String originalMobGriefingValue = gameRules.getGameRuleStringValue(ORIGINAL);
      gameRules.addGameRule(gameRule, originalMobGriefingValue);
    }
  }

  /**
   * Get the mobGriefing rule to use for the given entity
   * 
   * @param gameRules The GameRules to get the rule from
   * @param entity The Entity to get the rule for
   * @return The mobGriefing rule for the entity
   */
  public static String getMobGriefingRule(GameRules gameRules, EntityLivingBase entity) {
    String mobGriefingRule = BetterMobGriefingGameRule.ORIGINAL;

    // If the entity is an instance of a supported entity and the rule is defined then override the
    // original rule
    if (entity instanceof EntityCreeper && gameRules.hasRule(BetterMobGriefingGameRule.CREEPER)) {
      mobGriefingRule = BetterMobGriefingGameRule.CREEPER;
    }

    return mobGriefingRule;
  }
}
