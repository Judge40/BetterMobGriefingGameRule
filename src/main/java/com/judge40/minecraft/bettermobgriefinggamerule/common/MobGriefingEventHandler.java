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

package com.judge40.minecraft.bettermobgriefinggamerule.common;

import com.judge40.minecraft.bettermobgriefinggamerule.common.world.EntityMobGriefingData;
import net.minecraft.entity.Entity;
import net.minecraft.entity.projectile.SmallFireballEntity;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.GameRules;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.EntityMobGriefingEvent;
import net.minecraftforge.eventbus.api.Event.Result;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;

/**
 * An event handler for all mob griefing events.
 */
@EventBusSubscriber()
public class MobGriefingEventHandler {

  /**
   * When mob griefing occurs, check whether mob griefing is enabled for the griefing entity and set
   * event's result accordingly. {@link Result#ALLOW} indicates that mob griefing is enabled and
   * {@link Result#DENY} indicates that mob griefing is disabled.
   *
   * @param mobGriefingEvent The {@link EntityMobGriefingEvent} to update.
   */
  @SubscribeEvent
  public static void onMobGriefing(EntityMobGriefingEvent mobGriefingEvent) {
    Entity griefingEntity = mobGriefingEvent.getEntity();

    if (griefingEntity != null) {
      // TODO: Workaround for MinecraftForge bug, remove when PR #9038 merged.
      if (griefingEntity instanceof SmallFireballEntity) {
        Entity owner = ((SmallFireballEntity) griefingEntity).getOwner();

        if (owner != null) {
          griefingEntity = owner;
        }
      }

      if (isMobGriefingEnabled(griefingEntity)) {
        mobGriefingEvent.setResult(Result.ALLOW);
      } else {
        mobGriefingEvent.setResult(Result.DENY);
      }
    }
  }

  /**
   * Whether mob griefing is enabled to the given {@link Entity}.
   *
   * @param entity The Entity to get the mob griefing value for.
   * @return Whether mob griefing is enabled.
   */
  private static boolean isMobGriefingEnabled(Entity entity) {
    Boolean mobGriefingEnabled = null;
    ResourceLocation entityId = entity.getType().getRegistryName();
    MinecraftServer entityServer = entity.getServer();

    // If the entity type was found then try and get the entity's value from the world data.
    if (entityId != null && entityServer != null) {
      EntityMobGriefingData entityMobGriefingData = EntityMobGriefingData.forServer(entityServer);
      MobGriefingValue mobGriefingValue = entityMobGriefingData.getMobGriefingValue(entityId);

      if (!mobGriefingValue.equals(MobGriefingValue.INHERIT)) {
        mobGriefingEnabled = Boolean.valueOf(mobGriefingValue.toString());
      }
    }

    // If no entity rule was found then default to the global value.
    if (mobGriefingEnabled == null) {
      World entityWorld = entity.level;
      mobGriefingEnabled = entityWorld.getGameRules().getBoolean(GameRules.RULE_MOBGRIEFING);
    }

    return mobGriefingEnabled;
  }
}
