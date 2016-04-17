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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.judge40.minecraft.bettermobgriefinggamerule.world.BetterMobGriefingGameRuleWorldSavedData;

import cpw.mods.fml.common.ObfuscationReflectionHelper;
import net.minecraft.entity.EntityList;
import net.minecraft.entity.boss.EntityDragon;
import net.minecraft.entity.boss.EntityWither;
import net.minecraft.entity.monster.EntityCreeper;
import net.minecraft.entity.monster.EntityEnderman;
import net.minecraft.entity.monster.EntityGhast;
import net.minecraft.entity.monster.EntitySilverfish;
import net.minecraft.entity.monster.EntityZombie;
import net.minecraft.entity.passive.EntitySheep;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.GameRules;
import net.minecraft.world.World;

/**
 * Migrate data for versions before 0.3.0
 */
public class MigratePre030BetterMobGriefingGameRule {

  public static final Logger logger = LogManager.getLogger(BetterMobGriefingGameRule.MODID);

  public static final String CREEPER =
      (String) EntityList.classToStringMapping.get(EntityCreeper.class);
  public static final String DRAGON =
      (String) EntityList.classToStringMapping.get(EntityDragon.class);
  public static final String ENDERMAN =
      (String) EntityList.classToStringMapping.get(EntityEnderman.class);
  public static final String GHAST =
      (String) EntityList.classToStringMapping.get(EntityGhast.class);
  public static final String SHEEP =
      (String) EntityList.classToStringMapping.get(EntitySheep.class);
  public static final String SILVERFISH =
      (String) EntityList.classToStringMapping.get(EntitySilverfish.class);
  public static final String WITHER =
      (String) EntityList.classToStringMapping.get(EntityWither.class);
  public static final String ZOMBIE =
      (String) EntityList.classToStringMapping.get(EntityZombie.class);

  public static final List<String> MOBGRIEFING_GAME_RULES =
      Arrays.asList(CREEPER, DRAGON, ENDERMAN, GHAST, SHEEP, SILVERFISH, WITHER, ZOMBIE);

  /**
   * Migrate the original better mobGriefing game rules to the new world saved data form
   * 
   * @param gameRules The {@link GameRules} to migrate data from
   * @param worldSavedData The {@link BetterMobGriefingGameRuleWorldSavedData} to migrate data to
   */
  public static void migrateGameRulesToWorldData() {
    // Map entity names to original rules
    Map<String, String> entityNamesToOriginalEntityRule = new HashMap<>();

    for (String entityName : MOBGRIEFING_GAME_RULES) {
      String originalEntityRule = BetterMobGriefingGameRule.ORIGINAL.concat(entityName);

      // Handle special cases where the entity name used has changed from the original hard coded
      // value
      if (entityName.equals(DRAGON)) {
        originalEntityRule = BetterMobGriefingGameRule.ORIGINAL.concat("Dragon");
      } else if (entityName.equals(WITHER)) {
        originalEntityRule = BetterMobGriefingGameRule.ORIGINAL.concat("Wither");
      }

      entityNamesToOriginalEntityRule.put(entityName, originalEntityRule);
    }

    World world = MinecraftServer.getServer().getEntityWorld();
    GameRules gameRules = world.getGameRules();
    String mobGriefingValue = gameRules.getGameRuleStringValue(BetterMobGriefingGameRule.ORIGINAL);

    BetterMobGriefingGameRuleWorldSavedData worldSavedData =
        BetterMobGriefingGameRuleWorldSavedData.forWorld(world);

    for (Entry<String, String> entityNameToOriginalEntityRule : entityNamesToOriginalEntityRule
        .entrySet()) {
      String originalEntityRule = entityNameToOriginalEntityRule.getValue();

      if (gameRules.hasRule(originalEntityRule)) {
        // Get the game rule value
        String entityMobGriefingValue = gameRules.getGameRuleStringValue(originalEntityRule);

        // Remove the old game rule from the world's game rules
        TreeMap<?, ?> theGameRules = ObfuscationReflectionHelper.getPrivateValue(GameRules.class,
            gameRules, "theGameRules", "field_82577_x");
        theGameRules.remove(originalEntityRule);

        // If the rule does not already exist in the world saved data then migrate the game rule
        // value
        String entityName = entityNameToOriginalEntityRule.getKey();

        if (!worldSavedData.entityNamesToMobGriefingValue.containsKey(entityName)) {

          if (entityMobGriefingValue.equals(mobGriefingValue)) {
            entityMobGriefingValue = BetterMobGriefingGameRule.INHERIT;
          }

          worldSavedData.entityNamesToMobGriefingValue.put(entityName, entityMobGriefingValue);
          worldSavedData.setDirty(true);
          logger.info(String.format("%s game rule %s migrated with the value %s.",
              BetterMobGriefingGameRule.ORIGINAL, entityName, entityMobGriefingValue));
        }
      }
    }
  }
}
