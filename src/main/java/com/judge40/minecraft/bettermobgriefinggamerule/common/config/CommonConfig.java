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

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;
import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import com.judge40.minecraft.bettermobgriefinggamerule.common.ModInfoConstants;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.ForgeConfigSpec.BooleanValue;
import net.minecraftforge.common.ForgeConfigSpec.Builder;
import net.minecraftforge.common.ForgeConfigSpec.EnumValue;

class CommonConfig {

  // The constants for the localizable configuration UI message keys.
  private static final String GLOBAL_RULE_DESCRIPTION =
      ModInfoConstants.ID + ".config.defaultGlobalRule.description";
  private static final String ENTITY_RULES_DESCRIPTION =
      ModInfoConstants.ID + ".config.defaultEntityRules.description";

  private static final String GLOBAL_RULE_CATEGORY = "global";
  private static final String ENTITY_RULES_CATEGORY = "entity";

  private static final List<EntityType<? extends LivingEntity>> ENTITY_TYPES =
      Collections.unmodifiableList(Arrays.asList(
          EntityType.BLAZE, EntityType.CREEPER, EntityType.ENDER_DRAGON, EntityType.ENDERMAN,
          EntityType.EVOKER, EntityType.GHAST, EntityType.HUSK, EntityType.RABBIT, EntityType.SHEEP,
          EntityType.SILVERFISH, EntityType.SNOW_GOLEM, EntityType.VILLAGER, EntityType.WITHER,
          EntityType.ZOMBIE, EntityType.ZOMBIE_PIGMAN, EntityType.ZOMBIE_VILLAGER
      ));

  final BooleanValue defaultGlobalBooleanValue;

  final Map<ResourceLocation, EnumValue<MobGriefingValue>> entityIdsToDefaultEntityEnumValue;

  CommonConfig(Builder configBuilder) {
    configBuilder
        .comment("Default Global Rule")
        // TODO: Use translation for title when/if top-level comments can be translated.
        .push(GLOBAL_RULE_CATEGORY);

    defaultGlobalBooleanValue = configBuilder
        .comment(
            "The default global mobGriefing value which will be used when creating a new world.")
        .translation(GLOBAL_RULE_DESCRIPTION)
        .define(BetterMobGriefingGameRule.GLOBAL_RULE, true);

    configBuilder.pop();

    configBuilder
        .comment("Default Entity Rules")
        // TODO: Use translation for title when/if top-level comments can be translated.
        .push(ENTITY_RULES_CATEGORY)
        .comment(
            "The default entity specific mobGriefing rules which will be used when creating a new world or opening an existing world with no existing rule for the entity.")
        .translation(ENTITY_RULES_DESCRIPTION);

    Map<ResourceLocation, EnumValue<MobGriefingValue>> entityKeysToEntityEnumValue = new HashMap<>();

    for (EntityType<? extends LivingEntity> entityType : ENTITY_TYPES) {
      ResourceLocation entityKey = entityType.getRegistryName();

      if (entityKey != null) {
        EnumValue<MobGriefingValue> entityValue = configBuilder
            .defineEnum(entityKey.toString(), MobGriefingValue.INHERIT);
        entityKeysToEntityEnumValue.put(entityKey, entityValue);
      }
    }

    configBuilder.pop();

    this.entityIdsToDefaultEntityEnumValue = Collections
        .unmodifiableMap(entityKeysToEntityEnumValue);
  }
}
