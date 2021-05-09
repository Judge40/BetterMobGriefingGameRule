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

import com.judge40.minecraft.bettermobgriefinggamerule.client.gui.DefaultMobGriefingConfigGui;
import com.judge40.minecraft.bettermobgriefinggamerule.common.ModInfoConstants;
import com.judge40.minecraft.bettermobgriefinggamerule.common.command.BetterMobGriefingCommand;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.Config;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.ConfigHolder;
import com.judge40.minecraft.bettermobgriefinggamerule.common.world.EntityMobGriefingData;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.GameRules;
import net.minecraft.world.GameRules.BooleanValue;
import net.minecraft.world.World;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.ExtensionPoint;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.config.ModConfig.Type;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Base class for Better mobGriefing GameRule mod.
 */
@Mod(ModInfoConstants.ID)
@EventBusSubscriber
public class BetterMobGriefingGameRule {

  private static final Logger LOGGER = LogManager.getLogger();

  public static final String GLOBAL_RULE = "mobGriefing";

  /**
   * Register the mod config.
   */
  public BetterMobGriefingGameRule() {
    // Register mod config.
    ModLoadingContext modLoadingContext = ModLoadingContext.get();
    modLoadingContext.registerConfig(Type.COMMON, ConfigHolder.COMMON_SPEC);
    modLoadingContext.registerExtensionPoint(ExtensionPoint.CONFIGGUIFACTORY,
        () -> DefaultMobGriefingConfigGui::new);
  }

  /**
   * On server starting add new mobGriefing game rules.
   *
   * @param event The FMLServerStartingEvent.
   */
  @SubscribeEvent
  public static void onFmlServerStartingEvent(FMLServerStartingEvent event) {
    LOGGER.debug("Server starting.");
    MinecraftServer server = event.getServer();
    BetterMobGriefingCommand.register(server.getCommands().getDispatcher());

    // Add new game rules to world data
    World world = server.overworld();

    // Set the global mob griefing game rule value if this is a new world.
    if (world.getGameTime() == 0) {
      LOGGER.debug("New world detected, overwriting global mobGriefing rule.");

      boolean globalMobGriefingValue = Config.defaultGlobalValue;
      BooleanValue mobGriefing = world.getGameRules().getRule(GameRules.RULE_MOBGRIEFING);
      mobGriefing.set(globalMobGriefingValue, server);
    } else {
      LOGGER.debug("Existing world detected, global mobGriefing rule not overwritten.");
    }

    // Add the entity mob griefing game rules.
    EntityMobGriefingData entityMobGriefingData = EntityMobGriefingData
        .forServer(server);
    entityMobGriefingData.populateFromConfiguration();
  }
}
