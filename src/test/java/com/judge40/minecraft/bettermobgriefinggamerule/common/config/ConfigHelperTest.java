package com.judge40.minecraft.bettermobgriefinggamerule.common.config;

import static com.judge40.minecraft.bettermobgriefinggamerule.common.config.ConfigHolder.COMMON_CONFIG;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import com.judge40.minecraft.bettermobgriefinggamerule.TestUtils;
import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import java.util.Collections;
import java.util.Map;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.ForgeConfigSpec.EnumValue;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;

class ConfigHelperTest {

  @BeforeAll
  static void setUpBeforeAll() throws IllegalAccessException {
    TestUtils.initializeTestEnvironment();
  }

  @ParameterizedTest(name = "Should update global mobGriefing when input is {0}")
  @ValueSource(booleans = {true, false})
  void shouldUpdateGlobalMobGriefing(boolean input) {
    // When.
    ConfigHelper.updateGlobalMobGriefing(input);

    // Then.
    assertThat("Unexpected mobGriefing value.", Config.defaultGlobalValue, is(input));
    assertThat("Unexpected mobGriefing value.", COMMON_CONFIG.defaultGlobalBooleanValue.get(),
        is(input));
  }

  @ParameterizedTest(name = "Should update entity mobGriefing when input is {0}")
  @EnumSource(MobGriefingValue.class)
  void shouldUpdateEntityMobGriefing(MobGriefingValue input) {
    // Given.
    ResourceLocation entityId = new ResourceLocation("test:entity1");
    Map<ResourceLocation, MobGriefingValue> entityIdToValue = Collections
        .singletonMap(entityId, input);

    // When.
    ConfigHelper.updateEntityMobGriefing(entityIdToValue);

    // Then.
    MobGriefingValue mobGriefingValue = Config.entityIdsToDefaultEntityValue.get(entityId);
    assertThat("Unexpected mobGriefing value.", mobGriefingValue, is(input));

    EnumValue<MobGriefingValue> mobGriefingEnumValue = COMMON_CONFIG.entityIdsToDefaultEntityEnumValue
        .get(entityId);
    assertThat("Unexpected mobGriefing value.", mobGriefingEnumValue.get(), is(input));
  }

  @ParameterizedTest(name = "Should synchronize global mobGriefing when updated value is {0}")
  @ValueSource(booleans = {true, false})
  void shouldSynchronizeGlobalConfig(boolean input) {
    // Given.
    COMMON_CONFIG.defaultGlobalBooleanValue.set(input);

    // When.
    ConfigHelper.synchronizeCommon();

    // Then.
    assertThat("Unexpected mobGriefing value.", Config.defaultGlobalValue, is(input));
  }

  @ParameterizedTest(name = "Should synchronize entity mobGriefing when updated value is {0}")
  @EnumSource(MobGriefingValue.class)
  void shouldSynchronizeEntityConfig(MobGriefingValue input) {
    // Given.
    ResourceLocation entityId = new ResourceLocation("test:entity1");
    EnumValue<MobGriefingValue> mobGriefingEnumValue = COMMON_CONFIG.entityIdsToDefaultEntityEnumValue
        .get(entityId);
    mobGriefingEnumValue.set(input);

    // When.
    ConfigHelper.synchronizeCommon();

    // Then.
    MobGriefingValue mobGriefingValue = Config.entityIdsToDefaultEntityValue.get(entityId);
    assertThat("Unexpected mobGriefing value.", mobGriefingValue, is(input));
  }
}
