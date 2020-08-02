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

package com.judge40.minecraft.bettermobgriefinggamerule;

import com.electronwill.nightconfig.core.AbstractCommentedConfig;
import com.electronwill.nightconfig.core.CommentedConfig;
import com.electronwill.nightconfig.core.ConfigFormat;
import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.Config;
import com.judge40.minecraft.bettermobgriefinggamerule.common.config.ConfigHolder;
import java.util.HashMap;
import java.util.Map;
import net.minecraft.client.resources.I18n;
import net.minecraft.client.resources.Locale;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.registry.Bootstrap;
import net.minecraftforge.common.ForgeConfigSpec.Builder;
import net.minecraftforge.common.ForgeConfigSpec.EnumValue;
import org.apache.commons.lang3.reflect.FieldUtils;

public class TestUtils {

  public static void initializeTestEnvironment() throws IllegalAccessException {
    // Bootstrap registries so loading the config first does not cause failures.
    Bootstrap.func_218816_b();

    // Initialize locale so calling I18n does not cause failures.
    FieldUtils.writeStaticField(I18n.class, "i18nLocale", new Locale(), true);

    FieldUtils.writeField(ConfigHolder.COMMON_SPEC, "childConfig", new CommentedConfigStub(), true);
    initializeConfig(true, MobGriefingValue.INHERIT);
  }

  public static void initializeConfig(boolean mobGriefing, MobGriefingValue... mobGriefingValues)
      throws IllegalAccessException {
    Config.defaultGlobalValue = mobGriefing;

    Map<ResourceLocation, MobGriefingValue> entityIdsToValue = new HashMap<>();
    Config.entityIdsToDefaultEntityValue = entityIdsToValue;

    Builder configBuilder = new Builder();

    Map<ResourceLocation, EnumValue<MobGriefingValue>> entityIdsToDefaultEntityEnumValue = new HashMap<>();
    FieldUtils.writeField(ConfigHolder.COMMON_CONFIG, "entityIdsToDefaultEntityEnumValue",
        entityIdsToDefaultEntityEnumValue, true);

    for (int i = 0; i < mobGriefingValues.length; ) {
      MobGriefingValue mobGriefingValue = mobGriefingValues[i++];
      ResourceLocation entityId = new ResourceLocation("test:entity" + i);
      entityIdsToValue.put(entityId, mobGriefingValue);

      EnumValue<MobGriefingValue> mobGriefingValueEnum = configBuilder
          .defineEnum(entityId.toString(), mobGriefingValue);
      FieldUtils.writeField(mobGriefingValueEnum, "spec", ConfigHolder.COMMON_SPEC, true);
      entityIdsToDefaultEntityEnumValue.put(entityId, mobGriefingValueEnum);
    }
  }

  private static class CommentedConfigStub extends AbstractCommentedConfig {

    private CommentedConfigStub() {
      super(false);
    }

    @Override
    public AbstractCommentedConfig clone() {
      return null;
    }

    @Override
    public CommentedConfig createSubConfig() {
      return new CommentedConfigStub();
    }

    @Override
    public ConfigFormat<?> configFormat() {
      return null;
    }
  }
}
