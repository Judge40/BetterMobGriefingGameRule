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

import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingEventHandler;
import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import com.judge40.minecraft.bettermobgriefinggamerule.common.ModInfoConstants;
import com.judge40.minecraft.bettermobgriefinggamerule.common.ObfuscationHelper;
import com.judge40.minecraft.bettermobgriefinggamerule.common.command.BetterMobGriefingCommand;
import com.judge40.minecraft.bettermobgriefinggamerule.common.configuration.DefaultMobGriefingConfiguration;
import com.judge40.minecraft.bettermobgriefinggamerule.common.world.EntityMobGriefingData;

import net.minecraft.command.CommandHandler;
import net.minecraft.command.ICommand;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityList;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.World;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.common.FMLModContainer;
import net.minecraftforge.fml.common.Loader;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.Mod.EventHandler;
import net.minecraftforge.fml.common.ModContainer;
import net.minecraftforge.fml.common.event.FMLInitializationEvent;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;
import net.minecraftforge.fml.common.event.FMLServerStartingEvent;
import net.minecraftforge.fml.relauncher.ReflectionHelper;

import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Base class for Better mobGriefing GameRule mod.
 */
@Mod(modid = ModInfoConstants.ID, name = ModInfoConstants.DISPLAY_NAME,
    version = ModInfoConstants.VERSION, guiFactory = ModInfoConstants.GUI_FACTORY,
    acceptableRemoteVersions = "*")
public class BetterMobGriefingGameRule {

  public static final String GLOBAL_RULE = "mobGriefing";

  private DefaultMobGriefingConfiguration configuration = null;

  /**
   * Get the instance of {@link BetterMobGriefingGameRule} from the {@link Loader}'s mod list.
   * 
   * @return The instance of the {@code BetterMobGriefingGameRule} mod.
   */
  public static BetterMobGriefingGameRule getInstance() {
    Map<String, ModContainer> modList = Loader.instance().getIndexedModList();
    FMLModContainer modContainer = (FMLModContainer) modList.get(ModInfoConstants.ID);

    return (BetterMobGriefingGameRule) modContainer.getMod();
  }

  /**
   * Perform pre-initialization actions. The configuration file is loaded and the default
   * mobGriefing rule values are retrieved.
   * 
   * @param event The FMLPreInitializationEvent.
   */
  @EventHandler
  public void onFmlPreInitializationEvent(FMLPreInitializationEvent event) {
    // Create and/or load the configuration
    configuration = new DefaultMobGriefingConfiguration(event.getSuggestedConfigurationFile());
  }

  /**
   * On initialization registers the event handler.
   * 
   * @param event The FMLInitializationEvent.
   */
  @EventHandler
  public void onFmlInitializationEvent(FMLInitializationEvent event) {
    MobGriefingEventHandler eventHandler = new MobGriefingEventHandler();
    MinecraftForge.EVENT_BUS.register(eventHandler);
  }

  /**
   * On server starting add new mobGriefing game rules.
   * 
   * @param event The FMLServerStartingEvent.
   */
  @EventHandler()
  public void onFmlServerStartingEvent(FMLServerStartingEvent event) {
    CommandHandler commandHandler = (CommandHandler) event.getServer().getCommandManager();

    // Create a new game rule command handler and retrieve the original handler.
    BetterMobGriefingCommand newGameRuleHandler = new BetterMobGriefingCommand();
    ICommand originalGameRuleHandler =
        commandHandler.getCommands().get(newGameRuleHandler.getName());

    // Remove the original game rule command handler from the command set and register the new
    // handler.
    Set<?> commandSet = ReflectionHelper.getPrivateValue(CommandHandler.class, commandHandler,
        ObfuscationHelper.convertName("field_71561_b"));
    commandSet.remove(originalGameRuleHandler);
    commandHandler.registerCommand(newGameRuleHandler);

    // Add new game rules to world data
    World world = event.getServer().getEntityWorld();

    // Set the global mob griefing game rule value if this is a new world.
    if (world.getTotalWorldTime() == 0) {
      world.getGameRules().setOrCreateGameRule(GLOBAL_RULE,
          configuration.getGlobalMobGriefingValue().toExternalForm());
    }

    // Add the entity mob griefing game rules.
    EntityMobGriefingData entityMobGriefingData = EntityMobGriefingData.forWorld(world);
    entityMobGriefingData.populateFromConfiguration(configuration);
  }

  /**
   * Whether mob griefing is enabled to the given {@link Entity}.
   * 
   * @param entity The Entity to get the mob griefing value for.
   * @return Whether mob griefing is enabled.
   */
  public static boolean isMobGriefingEnabled(Entity entity) {
    Boolean mobGriefingEnabled = null;
    ResourceLocation entityType = EntityList.getKey(entity);

    // If the entity type was found then try and get the entity's value from the world data.
    if (entityType != null) {
      String entityName = entityType.getResourcePath();

      EntityMobGriefingData entityMobGriefingData = EntityMobGriefingData.forWorld(entity.world);
      MobGriefingValue mobGriefingValue = entityMobGriefingData.getMobGriefingValue(entityName);

      if (Objects.equals(mobGriefingValue, MobGriefingValue.TRUE)
          || Objects.equals(mobGriefingValue, MobGriefingValue.FALSE)) {
        mobGriefingEnabled = Boolean.valueOf(mobGriefingValue.toExternalForm());
      }
    }

    // If no entity rule was found then default to the global value.
    if (mobGriefingEnabled == null) {
      mobGriefingEnabled = entity.world.getGameRules().getBoolean(GLOBAL_RULE);
    }

    return mobGriefingEnabled;
  }

  /**
   * @return The {@link DefaultMobGriefingConfiguration}, will be null until the
   *         {@link FMLPreInitializationEvent} fires.
   */
  public DefaultMobGriefingConfiguration getDefaultMobGriefingConfiguration() {
    return configuration;
  }
}
