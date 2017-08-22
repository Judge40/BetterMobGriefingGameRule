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

import com.judge40.minecraft.bettermobgriefinggamerule.command.BetterMobGriefingCommand;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.DefaultMobGriefingConfiguration;
import com.judge40.minecraft.bettermobgriefinggamerule.world.EntityMobGriefingData;

import cpw.mods.fml.common.FMLCommonHandler;
import cpw.mods.fml.common.FMLModContainer;
import cpw.mods.fml.common.Loader;
import cpw.mods.fml.common.Mod;
import cpw.mods.fml.common.Mod.EventHandler;
import cpw.mods.fml.common.ModContainer;
import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import cpw.mods.fml.common.event.FMLServerStartingEvent;
import cpw.mods.fml.relauncher.ReflectionHelper;
import net.minecraft.command.CommandGameRule;
import net.minecraft.command.CommandHandler;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityList;
import net.minecraft.world.World;
import net.minecraftforge.common.MinecraftForge;

import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Base class for 'Better mobGriefing GameRule' mod.
 */
@Mod(modid = ModInfoConstants.ID, name = ModInfoConstants.DISPLAY_NAME,
    version = ModInfoConstants.MINECRAFT_VERSION, guiFactory = ModInfoConstants.GUI_FACTORY)
public class BetterMobGriefingGameRule {

  // Constants for the mobGriefing rules
  public static final String ORIGINAL = "mobGriefing";

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
    FMLCommonHandler.instance().bus().register(eventHandler);
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
    CommandGameRule originalGameRuleHandler =
        (CommandGameRule) commandHandler.getCommands().get(newGameRuleHandler.getCommandName());

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
      world.getGameRules().setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL,
          configuration.getGlobalMobGriefingValue().toExternalForm());
    }

    // Add the entity mob griefing game rules.
    EntityMobGriefingData entityMobGriefingData = EntityMobGriefingData.forWorld(world);
    entityMobGriefingData.populateFromConfiguration(configuration, false);
  }

  /**
   * Whether mob griefing is enabled to the given {@link Entity}.
   * 
   * @param entity The Entity to get the mob griefing value for.
   * @return Whether mob griefing is enabled.
   */
  public static boolean isMobGriefingEnabled(Entity entity) {
    Boolean mobGriefingEnabled = null;
    String entityName = EntityList.getEntityString(entity);

    if (entityName != null) {
      EntityMobGriefingData entityMobGriefingData = EntityMobGriefingData.forWorld(entity.worldObj);
      MobGriefingValue mobGriefingValue = entityMobGriefingData.getMobGriefingValue(entityName);

      if (Objects.equals(mobGriefingValue, MobGriefingValue.TRUE)
          || Objects.equals(mobGriefingValue, MobGriefingValue.FALSE)) {
        mobGriefingEnabled = Boolean.valueOf(mobGriefingValue.toExternalForm());
      }
    }

    if (mobGriefingEnabled == null) {
      mobGriefingEnabled = entity.worldObj.getGameRules()
          .getGameRuleBooleanValue(BetterMobGriefingGameRule.ORIGINAL);
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
