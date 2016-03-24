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

import org.hamcrest.CoreMatchers;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLServerStartedEvent;
import cpw.mods.fml.common.eventhandler.EventBus;
import mockit.Deencapsulation;
import mockit.Mock;
import mockit.MockUp;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.boss.EntityWither;
import net.minecraft.entity.monster.EntityCreeper;
import net.minecraft.entity.monster.EntityEnderman;
import net.minecraft.entity.monster.EntityGhast;
import net.minecraft.entity.monster.EntitySilverfish;
import net.minecraft.entity.monster.EntityZombie;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.GameRules;
import net.minecraft.world.World;

/**
 * Tests for BetterMobGriefingGameRule
 */
public class BetterMobGriefingGameRuleTest {

  private BetterMobGriefingGameRule betterMobGriefingGameRule;

  private GameRules gameRules;

  /**
   * @throws java.lang.Exception
   */
  @Before
  public void setUp() throws Exception {
    betterMobGriefingGameRule = new BetterMobGriefingGameRule();
    gameRules = new GameRules();
  }

  /**
   * @throws java.lang.Exception
   */
  @After
  public void tearDown() throws Exception {
    betterMobGriefingGameRule = null;
    gameRules = null;
  }

  /**
   * Test that the event handler is initialized on FMLInitializationEvent
   */
  @Test
  public void testOnFMLInitializationEvent_eventHandlerInitialized() {
    new MockUp<EventBus>() {
      @Mock(invocations = 1)
      void register(Object target) {
        Assert.assertThat("Initialized object is not the expected event handler", target,
            CoreMatchers.instanceOf(BetterMobGriefingGameRuleEventHandler.class));
      }
    };

    betterMobGriefingGameRule.onFMLInitializationEvent(new FMLInitializationEvent());
  }

  /**
   * Test that the new game rules are added on FMLServerStartedEvent
   */
  @Test
  public void testOnFMLServerStartedEvent_addMobGriefingGameRulesCalled() {
    new MockUp<MinecraftServer>() {
      @Mock
      void $clinit() {

      }

      @Mock
      MinecraftServer getServer() {
        return Deencapsulation.newUninitializedInstance(MinecraftServer.class);
      }
    };

    new MockUp<MinecraftServer>() {
      @Mock
      World getEntityWorld() {
        return Deencapsulation.newUninitializedInstance(World.class);
      }
    };

    new MockUp<World>() {
      @Mock
      GameRules getGameRules() {
        return gameRules;
      }
    };

    new MockUp<BetterMobGriefingGameRule>() {
      @Mock(invocations = 1)
      void addMobGriefingGameRules(GameRules gameRules) {

      }
    };

    betterMobGriefingGameRule.onFMLServerStartedEvent(new FMLServerStartedEvent());
  }

  /**
   * Test that the rule for EntityCreeper is added to the GameRules
   */
  @Test
  public void testAddMobGriefingGameRules_entityCreeper_entityCreeperRuleAdded() {
    BetterMobGriefingGameRule.addMobGriefingGameRules(gameRules);
    Assert.assertThat("The Creeper mobGriefing rule was not added to the GameRules",
        gameRules.hasRule(BetterMobGriefingGameRule.CREEPER), CoreMatchers.is(true));
  }

  /**
   * Test that the rule for EntityEnderman is added to the GameRules
   */
  @Test
  public void testAddMobGriefingGameRules_entityEnderman_entityEndermanRuleAdded() {
    BetterMobGriefingGameRule.addMobGriefingGameRules(gameRules);
    Assert.assertThat("The Enderman mobGriefing rule was not added to the GameRules",
        gameRules.hasRule(BetterMobGriefingGameRule.ENDERMAN), CoreMatchers.is(true));
  }

  /**
   * Test that the rule for EntityGhast is added to the GameRules
   */
  @Test
  public void testAddMobGriefingGameRules_entityGhast_entityGhastRuleAdded() {
    BetterMobGriefingGameRule.addMobGriefingGameRules(gameRules);
    Assert.assertThat("The Ghast mobGriefing rule was not added to the GameRules",
        gameRules.hasRule(BetterMobGriefingGameRule.GHAST), CoreMatchers.is(true));
  }

  /**
   * Test that the rule for EntitySilverfish is added to the GameRules
   */
  @Test
  public void testAddMobGriefingGameRules_entitySilverfish_entitySilverfishRuleAdded() {
    BetterMobGriefingGameRule.addMobGriefingGameRules(gameRules);
    Assert.assertThat("The Silverfish mobGriefing rule was not added to the GameRules",
        gameRules.hasRule(BetterMobGriefingGameRule.SILVERFISH), CoreMatchers.is(true));
  }

  /**
   * Test that the rule for EntityWither is added to the GameRules
   */
  @Test
  public void testAddMobGriefingGameRules_entityWither_entityWitherRuleAdded() {
    BetterMobGriefingGameRule.addMobGriefingGameRules(gameRules);
    Assert.assertThat("The Wither mobGriefing rule was not added to the GameRules",
        gameRules.hasRule(BetterMobGriefingGameRule.WITHER), CoreMatchers.is(true));
  }

  /**
   * Test that the rule for EntityZombie is added to the GameRules
   */
  @Test
  public void testAddMobGriefingGameRules_entityZombie_entityZombieRuleAdded() {
    BetterMobGriefingGameRule.addMobGriefingGameRules(gameRules);
    Assert.assertThat("The Zombie mobGriefing rule was not added to the GameRules",
        gameRules.hasRule(BetterMobGriefingGameRule.ZOMBIE), CoreMatchers.is(true));
  }

  /**
   * Test that a new rule is added with the correct default value when original is true
   */
  @Test
  public void testAddMobGriefingGameRule_noExistingRuleOriginalRuleTrue_newRuleDefaultTrue() {
    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL, "true");
    BetterMobGriefingGameRule.addMobGriefingGameRule(gameRules, "newRule");

    Assert.assertThat("The new rule was not added to the GameRules", gameRules.hasRule("newRule"),
        CoreMatchers.is(true));
    Assert.assertThat("The new rule does not have the correct default value",
        gameRules.getGameRuleBooleanValue("newRule"), CoreMatchers.is(true));
  }

  /**
   * Test that a new rule is added with the correct default value when original is false
   */
  @Test
  public void testAddMobGriefingGameRule_noExistingRuleOriginalRuleFalse_newRuleDefaultFalse() {
    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL, "false");
    BetterMobGriefingGameRule.addMobGriefingGameRule(gameRules, "newRule");

    Assert.assertThat("The new rule was not added to the GameRules", gameRules.hasRule("newRule"),
        CoreMatchers.is(true));
    Assert.assertThat("The new rule does not have the correct default value",
        gameRules.getGameRuleBooleanValue("newRule"), CoreMatchers.is(false));
  }

  /**
   * Test that when a rule already exists it is not added again
   */
  @Test
  public void testAddMobGriefingGameRule_existingRule_noNewRuleAdded() {
    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL, "true");
    gameRules.setOrCreateGameRule("existingRule", "false");

    new MockUp<GameRules>() {
      @Mock(invocations = 0)
      void addGameRule(String gameRule, String value) {

      }
    };

    BetterMobGriefingGameRule.addMobGriefingGameRule(gameRules, "existingRule");

    Assert.assertThat("The existing rule's value should not have been changed",
        gameRules.getGameRuleBooleanValue("existingRule"), CoreMatchers.is(false));
  }

  /**
   * Test that when entity is a Creeper and the matching rule does exist that the Creeper
   * mobGriefing rule is used
   */
  @Test
  public void testGetMobGriefingRule_entityCreeperRuleExists_creeperRule() {
    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.CREEPER, "true");
    String mobGriefingRule =
        BetterMobGriefingGameRule.getMobGriefingRule(gameRules, new EntityCreeper(null));
    Assert.assertThat("The returned mobGriefing rule does not match the expected rule",
        mobGriefingRule, CoreMatchers.is(BetterMobGriefingGameRule.CREEPER));
  }

  /**
   * Test that when entity is a creeper and the matching rule does not exist that the default
   * mobGriefing rule is used
   */
  @Test
  public void testGetMobGriefingRule_entityCreeperRuleNotExists_originalRule() {
    String mobGriefingRule =
        BetterMobGriefingGameRule.getMobGriefingRule(gameRules, new EntityCreeper(null));
    Assert.assertThat("The returned mobGriefing rule does not match the expected rule",
        mobGriefingRule, CoreMatchers.is(BetterMobGriefingGameRule.ORIGINAL));
  }

  /**
   * Test that when entity is an Enderman and the matching rule does exist that the Enderman
   * mobGriefing rule is used
   */
  @Test
  public void testGetMobGriefingRule_entityEndermanRuleExists_endermanRule() {
    new MockUp<EntityEnderman>() {
      @Mock
      void $clinit() {

      }
    };

    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ENDERMAN, "true");
    String mobGriefingRule =
        BetterMobGriefingGameRule.getMobGriefingRule(gameRules, new EntityEnderman(null));
    Assert.assertThat("The returned mobGriefing rule does not match the expected rule",
        mobGriefingRule, CoreMatchers.is(BetterMobGriefingGameRule.ENDERMAN));
  }

  /**
   * Test that when entity is an Enderman and the matching rule does not exist that the default
   * mobGriefing rule is used
   */
  @Test
  public void testGetMobGriefingRule_entityEndermanRuleNotExists_originalRule() {
    new MockUp<EntityEnderman>() {
      @Mock
      void $clinit() {

      }
    };

    String mobGriefingRule =
        BetterMobGriefingGameRule.getMobGriefingRule(gameRules, new EntityEnderman(null));
    Assert.assertThat("The returned mobGriefing rule does not match the expected rule",
        mobGriefingRule, CoreMatchers.is(BetterMobGriefingGameRule.ORIGINAL));
  }

  /**
   * Test that when entity is an Ghast and the matching rule does exist that the Enderman
   * mobGriefing rule is used
   */
  @Test
  public void testGetMobGriefingRule_entityGhastRuleExists_ghastRule() {
    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.GHAST, "true");
    String mobGriefingRule =
        BetterMobGriefingGameRule.getMobGriefingRule(gameRules, new EntityGhast(null));
    Assert.assertThat("The returned mobGriefing rule does not match the expected rule",
        mobGriefingRule, CoreMatchers.is(BetterMobGriefingGameRule.GHAST));
  }

  /**
   * Test that when entity is an Ghast and the matching rule does not exist that the default
   * mobGriefing rule is used
   */
  @Test
  public void testGetMobGriefingRule_entityGhastRuleNotExists_originalRule() {
    String mobGriefingRule =
        BetterMobGriefingGameRule.getMobGriefingRule(gameRules, new EntityGhast(null));
    Assert.assertThat("The returned mobGriefing rule does not match the expected rule",
        mobGriefingRule, CoreMatchers.is(BetterMobGriefingGameRule.ORIGINAL));
  }

  /**
   * Test that when entity is an Silverfish and the matching rule does exist that the Enderman
   * mobGriefing rule is used
   */
  @Test
  public void testGetMobGriefingRule_entitySilverfishRuleExists_silverfishRule() {
    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.SILVERFISH, "true");
    String mobGriefingRule =
        BetterMobGriefingGameRule.getMobGriefingRule(gameRules, new EntitySilverfish(null));
    Assert.assertThat("The returned mobGriefing rule does not match the expected rule",
        mobGriefingRule, CoreMatchers.is(BetterMobGriefingGameRule.SILVERFISH));
  }

  /**
   * Test that when entity is an Silverfish and the matching rule does not exist that the default
   * mobGriefing rule is used
   */
  @Test
  public void testGetMobGriefingRule_entitySilverfishRuleNotExists_originalRule() {
    String mobGriefingRule =
        BetterMobGriefingGameRule.getMobGriefingRule(gameRules, new EntitySilverfish(null));
    Assert.assertThat("The returned mobGriefing rule does not match the expected rule",
        mobGriefingRule, CoreMatchers.is(BetterMobGriefingGameRule.ORIGINAL));
  }

  /**
   * Test that when entity is an Wither and the matching rule does exist that the Enderman
   * mobGriefing rule is used
   */
  @Test
  public void testGetMobGriefingRule_entityWitherRuleExists_witherRule() {
    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.WITHER, "true");
    String mobGriefingRule =
        BetterMobGriefingGameRule.getMobGriefingRule(gameRules, new EntityWither(null));
    Assert.assertThat("The returned mobGriefing rule does not match the expected rule",
        mobGriefingRule, CoreMatchers.is(BetterMobGriefingGameRule.WITHER));
  }

  /**
   * Test that when entity is an Wither and the matching rule does not exist that the default
   * mobGriefing rule is used
   */
  @Test
  public void testGetMobGriefingRule_entityWitherRuleNotExists_originalRule() {
    String mobGriefingRule =
        BetterMobGriefingGameRule.getMobGriefingRule(gameRules, new EntityWither(null));
    Assert.assertThat("The returned mobGriefing rule does not match the expected rule",
        mobGriefingRule, CoreMatchers.is(BetterMobGriefingGameRule.ORIGINAL));
  }

  /**
   * Test that when entity is an Zombie and the matching rule does exist that the Zombie mobGriefing
   * rule is used
   */
  @Test
  public void testGetMobGriefingRule_entityZombieRuleExists_zombieRule() {
    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ZOMBIE, "true");
    String mobGriefingRule =
        BetterMobGriefingGameRule.getMobGriefingRule(gameRules, new EntityZombie(null));
    Assert.assertThat("The returned mobGriefing rule does not match the expected rule",
        mobGriefingRule, CoreMatchers.is(BetterMobGriefingGameRule.ZOMBIE));
  }

  /**
   * Test that when entity is an Zombie and the matching rule does not exist that the default
   * mobGriefing rule is used
   */
  @Test
  public void testGetMobGriefingRule_entityZombieRuleNotExists_originalRule() {
    String mobGriefingRule =
        BetterMobGriefingGameRule.getMobGriefingRule(gameRules, new EntityZombie(null));
    Assert.assertThat("The returned mobGriefing rule does not match the expected rule",
        mobGriefingRule, CoreMatchers.is(BetterMobGriefingGameRule.ORIGINAL));
  }

  /**
   * Test that when the entity is not specifically handled the default mobGriefing rule is used
   */
  @Test
  public void testGetMobGriefingRule_entityNotHandled_originalRule() {
    gameRules.setOrCreateGameRule(BetterMobGriefingGameRule.ENDERMAN, "true");
    String mobGriefingRule = BetterMobGriefingGameRule.getMobGriefingRule(gameRules,
        Deencapsulation.newUninitializedInstance(EntityLiving.class));
    Assert.assertThat("The returned mobGriefing rule does not match the expected rule",
        mobGriefingRule, CoreMatchers.is(BetterMobGriefingGameRule.ORIGINAL));
  }

  /**
   * Test that when the entity is null the default mobGriefing rule is used
   */
  @Test
  public void testGetMobGriefingRule_entityNull_originalRule() {
    String mobGriefingRule = BetterMobGriefingGameRule.getMobGriefingRule(gameRules, null);
    Assert.assertThat("The returned mobGriefing rule does not match the expected rule",
        mobGriefingRule, CoreMatchers.is(BetterMobGriefingGameRule.ORIGINAL));
  }
}
