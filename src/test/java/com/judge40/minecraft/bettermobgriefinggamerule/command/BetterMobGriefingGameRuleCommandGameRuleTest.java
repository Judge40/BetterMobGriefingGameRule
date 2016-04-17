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
package com.judge40.minecraft.bettermobgriefinggamerule.command;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.hamcrest.CoreMatchers;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;
import com.judge40.minecraft.bettermobgriefinggamerule.world.BetterMobGriefingGameRuleWorldSavedData;

import mockit.Mock;
import mockit.MockUp;
import net.minecraft.command.CommandGameRule;
import net.minecraft.command.ICommand;
import net.minecraft.command.ICommandSender;
import net.minecraft.command.WrongUsageException;
import net.minecraft.entity.EntityList;
import net.minecraft.entity.monster.EntityCreeper;
import net.minecraft.entity.monster.EntityZombie;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.IChatComponent;
import net.minecraft.world.GameRules;
import net.minecraft.world.World;
import net.minecraft.world.storage.MapStorage;

/**
 * Tests for {@link BetterMobGriefingGameRuleCommandGameRule}
 */
public class BetterMobGriefingGameRuleCommandGameRuleTest {

  BetterMobGriefingGameRuleCommandGameRule commandGameRule;

  private World world;
  private BetterMobGriefingGameRuleWorldSavedData worldSavedData;

  /**
   * @throws java.lang.Exception
   */
  @Before
  public void setUp() throws Exception {
    commandGameRule = new BetterMobGriefingGameRuleCommandGameRule();
    world = new MockUp<World>() {
      @Mock
      GameRules getGameRules() {
        return new GameRules();
      }
    }.getMockInstance();

    MapStorage mapStorage = new MapStorage(null);
    worldSavedData = new BetterMobGriefingGameRuleWorldSavedData(BetterMobGriefingGameRule.MODID);
    mapStorage.setData(BetterMobGriefingGameRule.MODID, worldSavedData);
    world.mapStorage = mapStorage;
  }

  /**
   * @throws java.lang.Exception
   */
  @After
  public void tearDown() throws Exception {
    commandGameRule = null;
    world = null;
    worldSavedData = null;
  }

  /**
   * Test that the parent class's method is called when there are no command words
   */
  @Test
  public void testProcessCommand_zeroWords_callParent() {
    new MockUp<CommandGameRule>() {
      @Mock(invocations = 1)
      void processCommand(ICommandSender commandSender, String[] commandWords) {

      }
    };
    commandGameRule.processCommand(null, new String[0]);
  }

  /**
   * Test that the parent class's method is called when the first command word is not "mobGriefing"
   */
  @Test
  public void testProcessCommand_firstWordNotMobGriefing_callParent() {
    new MockUp<CommandGameRule>() {
      @Mock(invocations = 1)
      void processCommand(ICommandSender commandSender, String[] commandWords) {

      }
    };
    commandGameRule.processCommand(null, new String[] {"notMobGriefing", "true"});
  }

  /**
   * Test that the original mob griefing rule value is sent to the command sender when the command
   * is the single word "mobGriefing" and there are no custom rules in the world data
   */
  @Test
  public void testProcessComannd_oneWordFirstWordMobGriefingNoCustomRulesExist_outputOnlyOriginalRuleValueToSender() {
    ICommandSender commandSender = new MockUp<ICommandSender>() {
      @Mock(invocations = 1)
      void addChatMessage(IChatComponent chatComponent) {
        String sentChatMessage = chatComponent.getUnformattedText();
        String expectedChatMessage =
            String.format("%s = %s", BetterMobGriefingGameRule.ORIGINAL, Boolean.toString(true));

        Assert.assertThat(
            "The chat message sent to the command sender does not match the expected message.",
            sentChatMessage, CoreMatchers.is(expectedChatMessage));
      }

      @Mock
      World getEntityWorld() {
        return world;
      }
    }.getMockInstance();

    world.getGameRules().setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL,
        Boolean.toString(true));

    commandGameRule.processCommand(commandSender, new String[] {"mobGriefing"});
  }

  /**
   * Test that all mob griefing rules values are sent to the command sender when the command is the
   * single word "mobGriefing" and there are custom rules in the world data
   */
  @Test
  public void testProcessComannd_oneWordFirstWordMobGriefingCustomRulesExist_outputAllRuleValuesToSender() {
    ICommandSender commandSender = new MockUp<ICommandSender>() {
      @Mock(invocations = 1)
      void addChatMessage(IChatComponent chatComponent) {
        String sentChatMessage = chatComponent.getUnformattedText();

        String originalMobGriefingValue =
            String.format("%s = %s", BetterMobGriefingGameRule.ORIGINAL, Boolean.toString(true));
        String betterMobGriefingValues = worldSavedData.toString();
        String expectedChatMessage =
            String.format("%s, %s", originalMobGriefingValue, betterMobGriefingValues);

        Assert.assertThat(
            "The chat message sent to the command sender does not match the expected message.",
            sentChatMessage, CoreMatchers.is(expectedChatMessage));
      }

      @Mock
      World getEntityWorld() {
        return world;
      }
    }.getMockInstance();

    world.getGameRules().setOrCreateGameRule(BetterMobGriefingGameRule.ORIGINAL,
        Boolean.toString(true));
    worldSavedData.entityNamesToMobGriefingValue.put(
        (String) EntityList.classToStringMapping.get(EntityCreeper.class), Boolean.toString(false));
    worldSavedData.entityNamesToMobGriefingValue.put(
        (String) EntityList.classToStringMapping.get(EntityZombie.class), Boolean.toString(true));

    commandGameRule.processCommand(commandSender, new String[] {"mobGriefing"});
  }

  /**
   * Test that the parent class's method is called to update the original mob griefing rule when
   * given a value of "true"
   */
  @Test
  public void testProcessComannd_twoWordsFirstWordMobGriefingSecondWordTrue_callParent() {
    ICommandSender commandSender = new MockUp<ICommandSender>() {
      @Mock
      World getEntityWorld() {
        return world;
      }
    }.getMockInstance();

    new MockUp<CommandGameRule>() {
      @Mock(invocations = 1)
      void processCommand(ICommandSender commandSender, String[] commandWords) {

      }
    };

    commandGameRule.processCommand(commandSender, new String[] {"mobGriefing", "true"});
  }

  /**
   * Test that the parent class's method is called to update the original mob griefing rule when
   * given a value of "false"
   */
  @Test
  public void testProcessComannd_twoWordsFirstWordMobGriefingSecondWordFalse_callParent() {
    ICommandSender commandSender = new MockUp<ICommandSender>() {
      @Mock
      World getEntityWorld() {
        return world;
      }
    }.getMockInstance();

    new MockUp<CommandGameRule>() {
      @Mock(invocations = 1)
      void processCommand(ICommandSender commandSender, String[] commandWords) {

      }
    };

    commandGameRule.processCommand(commandSender, new String[] {"mobGriefing", "false"});
  }

  /**
   * Test that the mob griefing rule value for the entity is sent to the command sender when the
   * command first word is "mobGriefing" and the second word is an entity name with a matching
   * custom rule
   */
  @Test
  public void testProcessComannd_twoWordsFirstWordMobGriefingSecondWordMatchingEntityName_outputRuleValueToSender() {
    ICommandSender commandSender = new MockUp<ICommandSender>() {
      @Mock(invocations = 1)
      void addChatMessage(IChatComponent chatComponent) {
        String sentChatMessage = chatComponent.getUnformattedText();
        String expectedChatMessage = String.format("%s %s = %s", BetterMobGriefingGameRule.ORIGINAL,
            "dummyEntityName", Boolean.toString(false));

        Assert.assertThat(
            "The chat message sent to the command sender does not match the expected message.",
            sentChatMessage, CoreMatchers.is(expectedChatMessage));
      }

      @Mock
      World getEntityWorld() {
        return world;
      }
    }.getMockInstance();

    worldSavedData.entityNamesToMobGriefingValue.put("dummyEntityName", "false");

    commandGameRule.processCommand(commandSender, new String[] {"mobGriefing", "dummyEntityName"});
  }

  /**
   * Test that an error is sent to the command sender when the command first word is "mobGriefing"
   * and the second word is an entity name with no matching custom rule
   */
  @Test
  public void testProcessComannd_twoWordsFirstWordMobGriefingSecondWordNoMatchingEntityName_outputNoRuleMessageToSender() {
    ICommandSender commandSender = new MockUp<ICommandSender>() {
      @Mock
      World getEntityWorld() {
        return world;
      }
    }.getMockInstance();

    new MockUp<CommandGameRule>() {
      @Mock(invocations = 1)
      void func_152373_a(ICommandSender commandSender, ICommand command, String messageKey,
          Object... messageParameters) {
        Assert.assertThat("The command was not sent to the expected sender.", commandSender,
            CoreMatchers.sameInstance(commandSender));
        Assert.assertThat("The command is not the expected command.", command,
            CoreMatchers.sameInstance(commandGameRule));
        Assert.assertThat("The message key is not the expected message key.", messageKey,
            CoreMatchers.is("commands.gamerule.norule"));
        Assert.assertThat("The message parameters do not match the expected message parameters.",
            messageParameters, CoreMatchers.is(new Object[] {"mobGriefing dummyEntityName"}));
      }
    };

    commandGameRule.processCommand(commandSender, new String[] {"mobGriefing", "dummyEntityName"});
  }

  /**
   * Test that the entities mob griefing rule is updated when the command first word is
   * "mobGriefing", the second word is a registered entity name and the third word is "true"
   */
  @Test
  public void testProcessCommand_threeWordsFirstWordMobGriefingSecondWordRegisteredEntityLivingThirdWordTrue_entityMobGriefingValueUpdated() {
    ICommandSender commandSender = new MockUp<ICommandSender>() {
      @Mock
      World getEntityWorld() {
        return world;
      }
    }.getMockInstance();

    new MockUp<CommandGameRule>() {
      @Mock(invocations = 1)
      void func_152373_a(ICommandSender commandSender, ICommand command, String messageKey,
          Object... messageParameters) {
        Assert.assertThat("The command was not sent to the expected sender.", commandSender,
            CoreMatchers.sameInstance(commandSender));
        Assert.assertThat("The command is not the expected command.", command,
            CoreMatchers.sameInstance(commandGameRule));
        Assert.assertThat("The message key is not the expected message key.", messageKey,
            CoreMatchers.is("commands.gamerule.success"));
        Assert.assertThat("The message parameters do not match the expected message parameters.",
            messageParameters, CoreMatchers.is(new Object[0]));
      }
    };

    String entityName = (String) EntityList.classToStringMapping.get(EntityCreeper.class);
    commandGameRule.processCommand(commandSender, new String[] {"mobGriefing", entityName, "true"});

    String entityMobGriefingValue = worldSavedData.entityNamesToMobGriefingValue.get(entityName);
    Assert.assertThat("The entities mob griefing value does not match the expected value.",
        entityMobGriefingValue, CoreMatchers.is(Boolean.toString(true)));
  }

  /**
   * Test that the entities mob griefing rule is updated when the command first word is
   * "mobGriefing", the second word is a registered entity name and the third word is "false"
   */
  @Test
  public void testProcessCommand_threeWordsFirstWordMobGriefingSecondWordRegisteredEntityLivingThirdWordFalse_entityMobGriefingValueUpdated() {
    ICommandSender commandSender = new MockUp<ICommandSender>() {
      @Mock
      World getEntityWorld() {
        return world;
      }
    }.getMockInstance();

    new MockUp<CommandGameRule>() {
      @Mock(invocations = 1)
      void func_152373_a(ICommandSender commandSender, ICommand command, String messageKey,
          Object... messageParameters) {
        Assert.assertThat("The command was not sent to the expected sender.", commandSender,
            CoreMatchers.sameInstance(commandSender));
        Assert.assertThat("The command is not the expected command.", command,
            CoreMatchers.sameInstance(commandGameRule));
        Assert.assertThat("The message key is not the expected message key.", messageKey,
            CoreMatchers.is("commands.gamerule.success"));
        Assert.assertThat("The message parameters do not match the expected message parameters.",
            messageParameters, CoreMatchers.is(new Object[0]));
      }
    };

    String entityName = (String) EntityList.classToStringMapping.get(EntityCreeper.class);
    commandGameRule.processCommand(commandSender,
        new String[] {"mobGriefing", entityName, "false"});

    String entityMobGriefingValue = worldSavedData.entityNamesToMobGriefingValue.get(entityName);
    Assert.assertThat("The entities mob griefing value does not match the expected value.",
        entityMobGriefingValue, CoreMatchers.is(Boolean.toString(false)));
  }

  /**
   * Test that a WrongUsageException is thrown when the command first word is "mobGriefing", the
   * second word is a registered entity name and the third word is invalid
   */
  @Test(expected = WrongUsageException.class)
  public void testProcessCommand_threeWordsFirstWordMobGriefingSecondWordRegisteredEntityLivingThirdWordInvalidValue_wrongUsageException() {
    ICommandSender commandSender = new MockUp<ICommandSender>() {
      @Mock
      World getEntityWorld() {
        return world;
      }
    }.getMockInstance();

    String entityName = (String) EntityList.classToStringMapping.get(EntityCreeper.class);
    commandGameRule.processCommand(commandSender,
        new String[] {"mobGriefing", entityName, "invalidValue"});

    String entityMobGriefingValue = worldSavedData.entityNamesToMobGriefingValue.get(entityName);
    Assert.assertThat("The entities mob griefing value does not match the expected value.",
        entityMobGriefingValue, CoreMatchers.nullValue());
  }

  /**
   * Test that a WrongUsageException is thrown when the command first word is "mobGriefing", the
   * second word is a registered non-EntityLiving entity name
   */
  @Test(expected = WrongUsageException.class)
  public void testProcessCommand_threeWordsFirstWordMobGriefingSecondWordRegisteredNonEntityLiving_wrongUsageException() {
    ICommandSender commandSender = new MockUp<ICommandSender>() {
      @Mock
      World getEntityWorld() {
        return world;
      }
    }.getMockInstance();

    String entityName = (String) EntityList.classToStringMapping.get(EntityPlayer.class);
    commandGameRule.processCommand(commandSender,
        new String[] {"mobGriefing", entityName, "invalidValue"});

    String entityMobGriefingValue = worldSavedData.entityNamesToMobGriefingValue.get(entityName);
    Assert.assertThat("The entities mob griefing value does not match the expected value.",
        entityMobGriefingValue, CoreMatchers.nullValue());
  }

  /**
   * Test that a WrongUsageException is thrown when the command first word is "mobGriefing", the
   * second word is not a registered entity name
   */
  @Test(expected = WrongUsageException.class)
  public void testProcessCommand_threeWordsFirstWordMobGriefingSecondWordNotRegisteredEntity_wrongUsageException() {
    ICommandSender commandSender = new MockUp<ICommandSender>() {
      @Mock
      World getEntityWorld() {
        return world;
      }
    }.getMockInstance();

    commandGameRule.processCommand(commandSender,
        new String[] {"mobGriefing", "dummyEntityName", "invalidValue"});

    String entityMobGriefingValue =
        worldSavedData.entityNamesToMobGriefingValue.get("dummyEntityName");
    Assert.assertThat("The entities mob griefing value does not match the expected value.",
        entityMobGriefingValue, CoreMatchers.nullValue());
  }

  /**
   * Test that a WrongUsageException is thrown when the command is more than three words long
   */
  @Test(expected = WrongUsageException.class)
  public void testProcessCommand_fourWords_wrongUsageException() {
    ICommandSender commandSender = new MockUp<ICommandSender>() {
      @Mock
      World getEntityWorld() {
        return world;
      }
    }.getMockInstance();

    commandGameRule.processCommand(commandSender,
        new String[] {"mobGriefing", "entityName", "value", "extraWord"});
  }

  /**
   * Test that the parent class's method is called when only a single word is passed
   */
  @Test
  public void testAddTabCompletionOptions_firstWordTabComplete_returnParentResult() {
    List<String> dummyTabCompletionOptions = Collections.singletonList("dummyTabCompletionOption");

    new MockUp<CommandGameRule>() {
      @Mock(invocations = 1)
      List<?> addTabCompletionOptions(ICommandSender commandSender, String[] commandWords) {
        return dummyTabCompletionOptions;
      }
    };

    List<?> tabCompletionOptions = commandGameRule.addTabCompletionOptions(null, new String[1]);
    Assert.assertThat("The returned tab completion options do no match the expected options.",
        tabCompletionOptions, CoreMatchers.is(dummyTabCompletionOptions));
  }

  /**
   * Test that the parent class's method is called when the first command word is not "mobGriefing"
   */
  @Test
  public void testAddTabCompletionOptions_firstWordTabCompleteNotMobGriefing_returnParentResult() {
    List<String> dummyTabCompletionOptions = Collections.singletonList("dummyTabCompletionOption");

    new MockUp<CommandGameRule>() {
      @Mock(invocations = 1)
      List<?> addTabCompletionOptions(ICommandSender commandSender, String[] commandWords) {
        return dummyTabCompletionOptions;
      }
    };

    List<?> tabCompletionOptions =
        commandGameRule.addTabCompletionOptions(null, new String[] {"notMobGriefing", "true"});
    Assert.assertThat("The returned tab completion options do no match the expected options.",
        tabCompletionOptions, CoreMatchers.is(dummyTabCompletionOptions));
  }

  /**
   * Test that all of "true", "false" and entity names are given as second word tab completion
   * options when the first word is "mobGriefing"
   */
  @Test
  public void testAddTabCompletionOptions_firstWordMobGriefingSecondWordTabComplete_trueFalseEntityName() {
    ICommandSender commandSender = new MockUp<ICommandSender>() {
      @Mock
      World getEntityWorld() {
        return world;
      }
    }.getMockInstance();

    String entityName = (String) EntityList.classToStringMapping.get(EntityZombie.class);
    worldSavedData.entityNamesToMobGriefingValue.put(entityName, Boolean.toString(true));

    List<?> returnedTabCompletionOptions =
        commandGameRule.addTabCompletionOptions(commandSender, new String[] {"mobGriefing", ""});
    List<String> expectedTabCompletionOptions =
        Arrays.asList(Boolean.toString(true), Boolean.toString(false), entityName);
    Assert.assertThat("The returned tab completion options do no match the expected options.",
        returnedTabCompletionOptions, CoreMatchers.is(expectedTabCompletionOptions));
  }

  /**
   * Test that both "true" and "false" are given as third word tab completion options when the first
   * word is "mobGriefing" and second word is a valid entity name
   */
  @Test
  public void testAddTabCompletionOptions_firstWordMobGriefingSecondWordEntityNameThirdWordTabComplete_trueFalse() {
    String entityName = (String) EntityList.classToStringMapping.get(EntityZombie.class);

    List<?> returnedTabCompletionOptions =
        commandGameRule.addTabCompletionOptions(null, new String[] {"mobGriefing", entityName, ""});
    List<String> expectedTabCompletionOptions = Arrays.asList(Boolean.toString(true),
        Boolean.toString(false), BetterMobGriefingGameRule.INHERIT);
    Assert.assertThat("The returned tab completion options do no match the expected options.",
        returnedTabCompletionOptions, CoreMatchers.is(expectedTabCompletionOptions));
  }

  /**
   * Test that no third word tab completion options are given when the first word is "mobGriefing"
   * and second word is not a valid entity name
   */
  @Test
  public void testAddTabCompletionOptions_firstWordMobGriefingSecondWordNotEntityNameThirdWordTabComplete_null() {
    List<?> returnedTabCompletionOptions =
        commandGameRule.addTabCompletionOptions(null, new String[] {"mobGriefing", "true", ""});
    Assert.assertThat("The returned tab completion options do no match the expected options.",
        returnedTabCompletionOptions, CoreMatchers.nullValue());
  }

  /**
   * Test that no third word tab completion options are given when more than three words are passed
   */
  @Test
  public void testAddTabCompletionOptions_forthWordTabComplete_null() {
    List<?> returnedTabCompletionOptions = commandGameRule.addTabCompletionOptions(null,
        new String[] {"mobGriefing", "entityName", "true", ""});
    Assert.assertThat("The returned tab completion options do no match the expected options.",
        returnedTabCompletionOptions, CoreMatchers.nullValue());
  }
}
