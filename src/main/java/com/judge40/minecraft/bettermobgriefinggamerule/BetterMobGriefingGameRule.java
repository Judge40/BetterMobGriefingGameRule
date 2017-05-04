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

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;

import com.judge40.minecraft.bettermobgriefinggamerule.command.BetterMobGriefingGameRuleCommandGameRule;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.DefaultMobGriefingConfiguration;
import com.judge40.minecraft.bettermobgriefinggamerule.world.BetterMobGriefingGameRuleWorldSavedData;

import cpw.mods.fml.common.FMLCommonHandler;
import cpw.mods.fml.common.Mod;
import cpw.mods.fml.common.Mod.EventHandler;
import cpw.mods.fml.common.ObfuscationReflectionHelper;
import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import cpw.mods.fml.common.event.FMLServerStartingEvent;
import net.minecraft.command.CommandGameRule;
import net.minecraft.command.CommandHandler;
import net.minecraft.command.ICommand;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityList;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.World;
import net.minecraftforge.common.MinecraftForge;

/**
 * Base class for 'Better mobGriefing GameRule' mod
 */
@Mod(modid = BetterMobGriefingGameRule.MODID, name = BetterMobGriefingGameRule.NAME,
    version = BetterMobGriefingGameRule.VERSION, guiFactory = BetterMobGriefingGameRule.GUI_FACTORY)
public class BetterMobGriefingGameRule {

  // Constants for mod attributes
  public static final String MODID = "bettermobgriefinggamerule";
  public static final String NAME = "Better mobGriefing GameRule";
  public static final String VERSION = "1.7.10-0.4.0";
  public static final String GUI_FACTORY =
      "com.judge40.minecraft.bettermobgriefinggamerule.client.gui.DefaultMobGriefingConfigGuiFactory";

  // Constants for the mobGriefing rules
  public static final String ORIGINAL = "mobGriefing";
  public static final String TRUE = Boolean.toString(true);
  public static final String FALSE = Boolean.toString(false);
  public static final String INHERIT = "inherit";

  public static DefaultMobGriefingConfiguration configuration;

  private static String defaultGlobalRule;
  private static Map<String, String> defaultEntityRules = new HashMap<>();

  /**
   * Perform pre-initialization actions. The configuration file is loaded and the default
   * mobGriefing rule values are retrieved.
   * 
   * @param preInitializationEvent The FMLPreInitializationEvent
   */
  @EventHandler
  public void onFMLPreInitializationEvent(FMLPreInitializationEvent preInitializationEvent) {
    // Create and/or load the configuration
    configuration =
        new DefaultMobGriefingConfiguration(preInitializationEvent.getSuggestedConfigurationFile());

    populateDefaultMobGriefingRulesFromConfiguration();
  }

  /**
   * Populate the default mobGriefing rules map based on the configuration values
   */
  public static void populateDefaultMobGriefingRulesFromConfiguration() {
    defaultGlobalRule = configuration.getGlobalMobGriefingValue();
    defaultEntityRules = configuration.getEntityMobGriefingValues();
  }

  /**
   * On initialization registers the event handler
   * 
   * @param event The FMLInitializationEvent
   */
  @EventHandler
  public void onFMLInitializationEvent(FMLInitializationEvent initializationEvent) {
    BetterMobGriefingGameRuleEventHandler eventHandler =
        new BetterMobGriefingGameRuleEventHandler();
    FMLCommonHandler.instance().bus().register(eventHandler);
    MinecraftForge.EVENT_BUS.register(eventHandler);
  }

  /**
   * On server starting add new mobGriefing game rules
   * 
   * @param serverStartingEvent The FMLServerStartingEvent
   */
  @EventHandler()
  public void onFMLServerStartingEvent(FMLServerStartingEvent serverStartingEvent) {
    CommandHandler commandHandler =
        (CommandHandler) serverStartingEvent.getServer().getCommandManager();

    // Get original gamerule command and register the new gamerule command
    CommandGameRule newCommandGameRule = new BetterMobGriefingGameRuleCommandGameRule();
    ICommand originalCommandGameRule =
        (ICommand) commandHandler.getCommands().get(newCommandGameRule.getCommandName());
    commandHandler.registerCommand(newCommandGameRule);

    // Remove original gamerule command from the command set
    Set<?> commandSet = ObfuscationReflectionHelper.getPrivateValue(CommandHandler.class,
        commandHandler, "commandSet", "field_71561_b");
    commandSet.remove(originalCommandGameRule);

    // TODO: remove migration before release of 1.0.0
    MigratePre030BetterMobGriefingGameRule.migrateGameRulesToWorldData();
    // Add new game rules to world data
    addMobGriefingGameRules();
  }

  /**
   * Add mobGriefing game rules for the mob griefing entities
   */
  public static void addMobGriefingGameRules() {
    World world = MinecraftServer.getServer().getEntityWorld();

    // Set the global mobGriefing game rule value if this is a new world
    if (world.getTotalWorldTime() == 0) {
      world.getGameRules().setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL,
          defaultGlobalRule);
    }

    // Add the entity rules
    BetterMobGriefingGameRuleWorldSavedData worldSavedData =
        BetterMobGriefingGameRuleWorldSavedData.forWorld(world);

    for (Entry<String, String> defaultEntityRule : defaultEntityRules.entrySet()) {
      String entityName = defaultEntityRule.getKey();
      String defaultValue = defaultEntityRule.getValue();

      // Add the rule only if it does not already exist
      String currentValue =
          worldSavedData.entityNamesToMobGriefingValue.putIfAbsent(entityName, defaultValue);
      if (currentValue == null) {
        worldSavedData.setDirty(true);
      }
    }
  }

  /**
   * Whether mob griefing is enabled to the given {@link Entity}
   * 
   * @param entity The Entity to get the mob griefing value for
   * @return Whether mob griefing is enabled
   */
  public static boolean isMobGriefingEnabled(Entity entity) {
    Boolean mobGriefingEnabled = null;
    String entityName = EntityList.getEntityString(entity);

    if (entityName != null) {
      BetterMobGriefingGameRuleWorldSavedData worldSavedData =
          BetterMobGriefingGameRuleWorldSavedData.forWorld(entity.worldObj);
      String mobGriefingValue = worldSavedData.entityNamesToMobGriefingValue.get(entityName);

      if (Objects.equals(mobGriefingValue, Boolean.toString(true))
          || Objects.equals(mobGriefingValue, Boolean.toString(false))) {
        mobGriefingEnabled = Boolean.valueOf(mobGriefingValue);
      }
    }

    if (mobGriefingEnabled == null) {
      mobGriefingEnabled = entity.worldObj.getGameRules()
          .getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
    }

    return mobGriefingEnabled;
  }
}
