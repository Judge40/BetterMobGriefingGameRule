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
import com.judge40.minecraft.bettermobgriefinggamerule.common.command.BetterMobGriefingCommand;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.Config;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.ConfigHolder;
import com.judge40.minecraft.bettermobgriefinggamerule.common.world.EntityMobGriefingData;
import java.util.Objects;
import net.minecraft.entity.Entity;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.GameRules.BooleanValue;
import net.minecraft.world.GameRules.RuleKey;
import net.minecraft.world.World;
import net.minecraft.world.dimension.DimensionType;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.config.ModConfig;
import net.minecraftforge.fml.config.ModConfig.ModConfigEvent;
import net.minecraftforge.fml.config.ModConfig.Type;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.registries.ForgeRegistries;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Base class for Better mobGriefing GameRule mod.
 */
@Mod(ModInfoConstants.ID)
public class BetterMobGriefingGameRule {

  private static final Logger LOGGER = LogManager.getLogger();

  public static final String GLOBAL_RULE = "mobGriefing";

  public BetterMobGriefingGameRule() {
    MobGriefingEventHandler eventHandler = new MobGriefingEventHandler();
    MinecraftForge.EVENT_BUS.register(eventHandler);
    MinecraftForge.EVENT_BUS.register(this);

    FMLJavaModLoadingContext.get().getModEventBus().register(eventHandler);

    ModLoadingContext modLoadingContext = ModLoadingContext.get();
    modLoadingContext.registerConfig(Type.COMMON, ConfigHolder.COMMON_SPEC);
  }

  /**
   * Get the instance of {@link BetterMobGriefingGameRule} from the {@code Loader}'s mod list.
   *
   * @return The instance of the {@code BetterMobGriefingGameRule} mod.
   */
  public static BetterMobGriefingGameRule getInstance() {
//    Map<String, ModContainer> modList = Loader.instance().getIndexedModList();
//    FMLModContainer modContainer = (FMLModContainer) modList.get(ModInfoConstants.ID);
//
//    return (BetterMobGriefingGameRule) modContainer.getMod();
    return new BetterMobGriefingGameRule();
  }

  @SubscribeEvent
  public void onModConfigEvent(ModConfigEvent event) {
    ModConfig config = event.getConfig();

    if (config.getSpec() == ConfigHolder.COMMON_SPEC) {
      //DefaultMobGriefingConfiguration.bake();
    }
  }

  /**
   * On server starting add new mobGriefing game rules.
   *
   * @param event The FMLServerStartingEvent.
   */
  @SubscribeEvent
  public void onFmlServerStartingEvent(FMLServerStartingEvent event) {
    LOGGER.debug("Server starting.");
    BetterMobGriefingCommand.register(event.getCommandDispatcher());

    // Add new game rules to world data
    World world = event.getServer().getWorld(DimensionType.OVERWORLD);

    // Set the global mob griefing game rule value if this is a new world.
    if (world.getGameTime() == 0) {
      LOGGER.debug("New world detected, creating game rules.");

      boolean globalMobGriefingValue = Config.defaultGlobalValue;
      BooleanValue mobGriefing = world.getGameRules().get(new RuleKey<BooleanValue>(GLOBAL_RULE));
      mobGriefing.set(globalMobGriefingValue, event.getServer());
    } else {
      LOGGER.debug("Existing world detected, no game rules created.");
    }

    // Add the entity mob griefing game rules.
    EntityMobGriefingData entityMobGriefingData = EntityMobGriefingData
        .forServer(event.getServer());
    entityMobGriefingData.populateFromConfiguration();
  }

  /**
   * Whether mob griefing is enabled to the given {@link Entity}.
   *
   * @param entity The Entity to get the mob griefing value for.
   * @return Whether mob griefing is enabled.
   */
  public static boolean isMobGriefingEnabled(Entity entity) {
    Boolean mobGriefingEnabled = null;
    ResourceLocation entityType = ForgeRegistries.ENTITIES.getKey(entity.getType());

    // If the entity type was found then try and get the entity's value from the world data.
    if (entityType != null) {
      String entityName = entityType.getPath();

      EntityMobGriefingData entityMobGriefingData = EntityMobGriefingData.forWorld(entity.world);
      MobGriefingValue mobGriefingValue = entityMobGriefingData.getMobGriefingValue(entityName);

      if (Objects.equals(mobGriefingValue, MobGriefingValue.TRUE)
          || Objects.equals(mobGriefingValue, MobGriefingValue.FALSE)) {
        mobGriefingEnabled = Boolean.valueOf(mobGriefingValue.toExternalForm());
      }
    }

    // If no entity rule was found then default to the global value.
    if (mobGriefingEnabled == null) {
      mobGriefingEnabled = entity.world.getGameRules().getBoolean(new RuleKey<>(GLOBAL_RULE));
    }

    return mobGriefingEnabled;
  }
}
