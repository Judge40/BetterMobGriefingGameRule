/*
 * Better mobGriefing GameRule Copyright (c) 2020 Judge40
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

package com.judge40.minecraft.bettermobgriefinggamerule.common.config;

import static com.judge40.minecraft.bettermobgriefinggamerule.common.config.ConfigHolder.COMMON_CONFIG;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.MOD;

import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.common.ForgeConfigSpec.EnumValue;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.config.ModConfig;
import net.minecraftforge.fml.event.config.ModConfigEvent.Loading;

@EventBusSubscriber(bus = MOD)
public class ConfigHelper {

  /**
   * Synchronize the configuration changes when the configuration is changed.
   *
   * @param event The ModConfigEvent.
   */
  // TODO: The ModConfigEvent does not fire consistently despite the toml file being updated,
  //  updates are synchronized manually as a workaround so only handle the Loading event here.
  @SubscribeEvent
  public static void onModConfigEvent(Loading event) {
    ModConfig config = event.getConfig();

    if (config.getSpec() == ConfigHolder.COMMON_SPEC) {
      synchronizeCommon();
    }
  }

  /**
   * Synchronize the config file values to mod config reference values.
   */
  public static void synchronizeCommon() {
    Config.defaultGlobalValue = COMMON_CONFIG.defaultGlobalBooleanValue
        .get();
    Config.entityIdsToDefaultEntityValue = COMMON_CONFIG.entityIdsToDefaultEntityEnumValue
        .entrySet().stream()
        .collect(Collectors.toMap(Entry::getKey, entry -> entry.getValue().get()));
  }

  /**
   * Update the global config value.
   *
   * @param value The global value to set.
   */
  public static void updateGlobalMobGriefing(boolean value) {
    COMMON_CONFIG.defaultGlobalBooleanValue.set(value);

    // TODO: The ModConfigEvent does not fire consistently despite the toml file being updated,
    //  manually synchronize as a workaround.
    Config.defaultGlobalValue = value;
  }

  /**
   * Update the entity config values.
   *
   * @param entityIdToValue A map of entity ID to value to update the config with.
   */
  public static void updateEntityMobGriefing(
      Map<ResourceLocation, MobGriefingValue> entityIdToValue) {
    entityIdToValue.forEach((entityId, value) -> {
      EnumValue<MobGriefingValue> enumValue = COMMON_CONFIG.entityIdsToDefaultEntityEnumValue
          .get(entityId);
      enumValue.set(value);

      // TODO: The ModConfigEvent does not fire consistently despite the toml file being updated,
      //  manually synchronize as a workaround.
      Config.entityIdsToDefaultEntityValue.put(entityId, value);
    });
  }
}
