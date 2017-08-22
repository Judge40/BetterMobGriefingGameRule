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

import com.judge40.minecraft.bettermobgriefinggamerule.MobGriefingValue;
import com.judge40.minecraft.bettermobgriefinggamerule.world.EntityMobGriefingData;

import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import net.minecraft.command.CommandGameRule;
import net.minecraft.command.ICommandSender;
import net.minecraft.command.WrongUsageException;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityList;
import net.minecraft.entity.EntityLiving;
import net.minecraft.util.ChatComponentText;
import net.minecraft.world.GameRules;
import net.minecraft.world.World;
import org.hamcrest.CoreMatchers;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * The unit tests for {@link BetterMobGriefingCommand}.
 */
public class BetterMobGriefingCommandTest {

  private BetterMobGriefingCommand command;

  @Mocked
  private ICommandSender commandSender;

  @Mocked
  private World world;

  @Before
  public void setUp() {
    command = new BetterMobGriefingCommand();
  }

  /**
   * Test that the parent handles the tab completion when tab completing the first word and it is
   * "mobGriefing".
   */
  @Test
  public void testAddTabCompletionOptions_mobGriefingCompleteWordOne_handledByParent(
      @Mocked CommandGameRule parentCommand) {
    // Set up test data.
    String[] commandWords = new String[] {"mobGriefing"};

    // Call the method under test.
    command.addTabCompletionOptions(commandSender, commandWords);

    // Verify expectations.
    new Verifications() {
      {
        parentCommand.addTabCompletionOptions(commandSender, commandWords);
      }
    };
  }

  /**
   * Test that true, false and registered entity names options are returned when tab completing the
   * second word and the first word is "mobGriefing".
   */
  @Test
  public void testAddTabCompletionOptions_mobGriefingCompleteWordTwo_trueFalseEntityNames(
      @Mocked World world) {
    // Set up test data.
    EntityMobGriefingData entityMobGriefingData = new EntityMobGriefingData("");
    Set<String> registeredEntityNames =
        new HashSet<>(Arrays.asList("entityName1", "entityName2", "entityName3"));

    String[] expectedPossibleWords =
        new String[] {"true", "false", "entityName1", "entityName2", "entityName3"};
    List<String> matchingWords = Arrays.asList(expectedPossibleWords);

    String[] commandWords = new String[] {"mobGriefing", ""};

    // Record expectations.
    new Expectations(BetterMobGriefingCommand.class, EntityMobGriefingData.class) {
      {
        commandSender.getEntityWorld();
        result = world;

        EntityMobGriefingData.forWorld(world);
        result = entityMobGriefingData;

        entityMobGriefingData.getRegisteredEntityNames();
        result = registeredEntityNames;

        BetterMobGriefingCommand.getListOfStringsMatchingLastWord(commandWords,
            expectedPossibleWords);
        result = matchingWords;
      }
    };

    // Call the method under test.
    List<?> tabCompletionOptions = command.addTabCompletionOptions(commandSender, commandWords);

    // Perform assertions.
    Assert.assertThat("The tab completion options did not match the expected options.",
        tabCompletionOptions, CoreMatchers.sameInstance(matchingWords));
  }

  /**
   * Test that true, false and inherit options are returned when tab completing the third word and
   * the first word is "mobGriefing".
   */
  @Test
  public void testAddTabCompletionOptions_mobGriefingCompleteWordThree_trueFalseInherit() {
    // Set up test data.
    String[] expectedPossibleWords = new String[] {"true", "false", "inherit"};
    List<String> matchingWords = Arrays.asList(expectedPossibleWords);

    String[] commandWords = new String[] {"mobGriefing", "", ""};

    // Record expectations.
    new Expectations(BetterMobGriefingCommand.class) {
      {
        BetterMobGriefingCommand.getListOfStringsMatchingLastWord(commandWords,
            expectedPossibleWords);
        result = matchingWords;
      }
    };

    // Call the method under test.
    List<?> tabCompletionOptions = command.addTabCompletionOptions(commandSender, commandWords);

    // Perform assertions.
    Assert.assertThat("The tab completion options did not match the expected options.",
        tabCompletionOptions, CoreMatchers.sameInstance(matchingWords));
  }

  /**
   * Test that null is returned when tab completing the fourth word and the first word is
   * "mobGriefing".
   */
  @Test
  public void testAddTabCompletionOptions_mobGriefingCompleteWordFour_null(
      @Mocked CommandGameRule parentCommand) {
    // Set up test data.
    String[] commandWords = new String[] {"mobGriefing", "", "", ""};

    // Call the method under test.
    List<?> tabCompletionOptions = command.addTabCompletionOptions(commandSender, commandWords);

    // Perform assertions.
    Assert.assertThat("The tab completion options did not match the expected options.",
        tabCompletionOptions, CoreMatchers.nullValue());

    // Verify expectations.
    new Verifications() {
      {
        BetterMobGriefingCommand.getListOfStringsMatchingLastWord(commandWords, (String[]) any);
        times = 0;
      }
    };
  }

  /**
   * Test that the parent handles the tab completion when the first word is not "mobGriefing".
   */
  @Test
  public void testAddTabCompletionOptions_notMobGriefing_handledByParent(
      @Mocked CommandGameRule parentCommand) {
    // Set up test data.
    String[] commandWords = new String[] {"notMobGriefing", ""};

    // Call the method under test.
    command.addTabCompletionOptions(commandSender, commandWords);

    // Verify expectations.
    new Verifications() {
      {
        parentCommand.addTabCompletionOptions(commandSender, commandWords);
      }
    };
  }

  /**
   * Test that the parent handles the command when there are no command words.
   */
  @Test
  public void testProcessCommand_noWords_handledByParent(@Mocked CommandGameRule parentCommand) {
    // Set up test data.
    String[] commandWords = new String[0];

    // Call the method under test.
    command.processCommand(commandSender, commandWords);

    // Verify expectations.
    new Verifications() {
      {
        parentCommand.processCommand(commandSender, commandWords);
      }
    };
  }

  /**
   * Test that the global value and entity values are output when the only command word is
   * "mobGriefing" and entity values exist.
   */
  @Test
  public void testProcessCommand_mobGriefingEntityValuesExist_globalAndEntityValuesOutput(
      @Mocked World world) {
    // Set up test data.
    EntityMobGriefingData entityMobGriefingData = new EntityMobGriefingData("");
    entityMobGriefingData.setMobGriefingValue("entityName1", MobGriefingValue.TRUE);
    entityMobGriefingData.setMobGriefingValue("entityName2", MobGriefingValue.FALSE);
    entityMobGriefingData.setMobGriefingValue("entityName3", MobGriefingValue.INHERIT);
    GameRules gameRules = new GameRules();

    List<ChatComponentText> capturedChatText = new ArrayList<>();
    String[] commandWords = new String[] {"mobGriefing"};

    // Record Expectations.
    new Expectations(gameRules, EntityMobGriefingData.class) {
      {
        commandSender.getEntityWorld();
        result = world;

        EntityMobGriefingData.forWorld(world);
        result = entityMobGriefingData;

        world.getGameRules();
        result = gameRules;

        gameRules.getGameRuleStringValue("mobGriefing");
        result = "globalValue";

        commandSender.addChatMessage(withCapture(capturedChatText));
      }
    };

    // Call the method under test.
    command.processCommand(commandSender, commandWords);

    // Perform assertions.
    Assert.assertThat("The number of chat outputs did not match the expected number.",
        capturedChatText.size(), CoreMatchers.is(4));
    Assert.assertThat("The chat output did not contain the expected value.",
        capturedChatText.get(0).getUnformattedText(), CoreMatchers.is("mobGriefing = globalValue"));
    Assert.assertThat("The chat output did not contain the expected value.",
        capturedChatText.get(1).getUnformattedText(),
        CoreMatchers.is("mobGriefing entityName1 = true"));
    Assert.assertThat("The chat output did not contain the expected value.",
        capturedChatText.get(2).getUnformattedText(),
        CoreMatchers.is("mobGriefing entityName2 = false"));
    Assert.assertThat("The chat output did not contain the expected value.",
        capturedChatText.get(3).getUnformattedText(),
        CoreMatchers.is("mobGriefing entityName3 = inherit"));
  }

  /**
   * Test that the global value is output when the only command word is "mobGriefing" and entity
   * values do not exist.
   */
  @Test
  public void testProcessCommand_mobGriefingEntityValuesNotExists_globalValueOutput(
      @Mocked World world) {
    // Set up test data.
    EntityMobGriefingData entityMobGriefingData = new EntityMobGriefingData("");
    GameRules gameRules = new GameRules();

    List<ChatComponentText> capturedChatText = new ArrayList<>();
    String[] commandWords = new String[] {"mobGriefing"};

    // Record Expectations.
    new Expectations(entityMobGriefingData, gameRules) {
      {
        commandSender.getEntityWorld();
        result = world;

        EntityMobGriefingData.forWorld(world);
        result = entityMobGriefingData;

        world.getGameRules();
        result = gameRules;

        gameRules.getGameRuleStringValue("mobGriefing");
        result = "globalValue";

        entityMobGriefingData.toString();
        result = "";

        commandSender.addChatMessage(withCapture(capturedChatText));
      }
    };

    // Call the method under test.
    command.processCommand(commandSender, commandWords);

    // Perform assertions.
    Assert.assertThat("The number of chat outputs did not match the expected number.",
        capturedChatText.size(), CoreMatchers.is(1));
    Assert.assertThat("The chat output did not contain the expected value.",
        capturedChatText.get(0).getUnformattedText(), CoreMatchers.is("mobGriefing = globalValue"));
  }

  /**
   * Test that the parent handles the command when the command words are "mobGriefing" and "true".
   */
  @Test
  public void testProcessCommand_mobGriefingTrue_handledByParent(
      @Mocked CommandGameRule parentCommand) {
    // Set up test data.
    EntityMobGriefingData entityMobGriefingData = new EntityMobGriefingData("");

    String[] commandWords = new String[] {"mobGriefing", "true"};

    // Record Expectations.
    new Expectations(EntityMobGriefingData.class) {
      {
        commandSender.getEntityWorld();
        result = world;

        EntityMobGriefingData.forWorld(world);
        result = entityMobGriefingData;
      }
    };

    // Call the method under test.
    command.processCommand(commandSender, commandWords);

    // Verify executions.
    new Verifications() {
      {
        parentCommand.processCommand(commandSender, commandWords);
      }
    };
  }

  /**
   * Test that the parent handles the command when the command words are "mobGriefing" and "false".
   */
  @Test
  public void testProcessCommand_mobGriefingFalse_handledByParent(
      @Mocked CommandGameRule parentCommand) {
    // Set up test data.
    EntityMobGriefingData entityMobGriefingData = new EntityMobGriefingData("");

    String[] commandWords = new String[] {"mobGriefing", "false"};

    // Record Expectations.
    new Expectations(EntityMobGriefingData.class) {
      {
        commandSender.getEntityWorld();
        result = world;

        EntityMobGriefingData.forWorld(world);
        result = entityMobGriefingData;
      }
    };

    // Call the method under test.
    command.processCommand(commandSender, commandWords);

    // Verify executions.
    new Verifications() {
      {
        parentCommand.processCommand(commandSender, commandWords);
      }
    };
  }

  /**
   * Test that the entity value is output when the command words are "mobGriefing" and a valid
   * entity name.
   */
  @Test
  public void testProcessCommand_mobGriefingValidEntityName_entityValueOutput() {
    // Set up test data.
    EntityMobGriefingData entityMobGriefingData = new EntityMobGriefingData("");

    List<ChatComponentText> capturedChatText = new ArrayList<>();
    String[] commandWords = new String[] {"mobGriefing", "entityName1"};

    // Record Expectations.
    new Expectations(entityMobGriefingData) {
      {
        commandSender.getEntityWorld();
        result = world;

        EntityMobGriefingData.forWorld(world);
        result = entityMobGriefingData;

        entityMobGriefingData.getMobGriefingValue("entityName1");
        result = MobGriefingValue.TRUE;

        commandSender.addChatMessage(withCapture(capturedChatText));
      }
    };

    // Call the method under test.
    command.processCommand(commandSender, commandWords);

    // Perform assertions.
    Assert.assertThat("The number of chat outputs did not match the expected number.",
        capturedChatText.size(), CoreMatchers.is(1));
    Assert.assertThat("The chat output did not contain the expected value.",
        capturedChatText.get(0).getUnformattedText(),
        CoreMatchers.is("mobGriefing entityName1 = true"));
  }

  /**
   * Test that the no game rule message is output when the command words are "mobGriefing" and an
   * invalid entity name.
   */
  @Test
  public void testProcessCommand_mobGriefingInvalidEntityName_noRuleMessage() {
    // Set up test data.
    EntityMobGriefingData entityMobGriefingData = new EntityMobGriefingData("");

    String[] commandWords = new String[] {"mobGriefing", "entityName1"};

    // Record Expectations.
    new Expectations(entityMobGriefingData) {
      {
        commandSender.getEntityWorld();
        result = world;

        EntityMobGriefingData.forWorld(world);
        result = entityMobGriefingData;

        entityMobGriefingData.getMobGriefingValue("entityName1");
        result = null;
      }
    };

    // Call the method under test.
    command.processCommand(commandSender, commandWords);

    // Verify expectations.
    new Verifications() {
      {
        BetterMobGriefingCommand.func_152373_a(commandSender, command, "commands.gamerule.norule",
            new Object[] {"mobGriefing entityName1"});
      }
    };
  }

  /**
   * Test that the entity value is set when the command words are "mobGriefing", a valid entity name
   * and a valid entity value.
   */
  @Test
  public void testProcessCommand_mobGriefingValidEntityNameValidValue_entityValueSet() {
    // Set up test data.
    EntityMobGriefingData entityMobGriefingData = new EntityMobGriefingData("");

    String entityName = (String) EntityList.classToStringMapping.get(EntityLiving.class);
    String[] commandWords = new String[] {"mobGriefing", entityName, "inherit"};

    // Record Expectations.
    new Expectations(entityMobGriefingData) {
      {
        commandSender.getEntityWorld();
        result = world;

        EntityMobGriefingData.forWorld(world);
        result = entityMobGriefingData;
      }
    };

    // Call the method under test.
    command.processCommand(commandSender, commandWords);

    // Verify expectations.
    new Verifications() {
      {
        entityMobGriefingData.setMobGriefingValue(entityName, MobGriefingValue.INHERIT);

        BetterMobGriefingCommand.func_152373_a(commandSender, command, "commands.gamerule.success",
            new Object[0]);
      }
    };
  }

  /**
   * Test that a {@link WrongUsageException} is thrown when the command words are "mobGriefing", a
   * valid entity name and an invalid entity value.
   */
  @Test(expected = WrongUsageException.class)
  public void testProcessCommand_mobGriefingValidEntityNameInvalidValue_exception() {
    // Set up test data.
    EntityMobGriefingData entityMobGriefingData = new EntityMobGriefingData("");

    String entityName = (String) EntityList.classToStringMapping.get(EntityLiving.class);
    String[] commandWords = new String[] {"mobGriefing", entityName, ""};

    // Record Expectations.
    new Expectations(entityMobGriefingData) {
      {
        commandSender.getEntityWorld();
        result = world;

        EntityMobGriefingData.forWorld(world);
        result = entityMobGriefingData;
      }
    };

    // Call the method under test.
    command.processCommand(commandSender, commandWords);

    // Verify expectations.
    new Verifications() {
      {
        entityMobGriefingData.setMobGriefingValue(anyString, (MobGriefingValue) any);
        times = 0;
      }
    };
  }

  /**
   * Test that a {@link WrongUsageException} is thrown when the command words are "mobGriefing", the
   * entity name of an invalid type and a third word.
   */
  @Test(expected = WrongUsageException.class)
  public void testProcessCommand_mobGriefingInvalidEntityNameThirdWord_exception() {
    // Set up test data.
    EntityMobGriefingData entityMobGriefingData = new EntityMobGriefingData("");

    String entityName = (String) EntityList.classToStringMapping.get(Entity.class);
    String[] commandWords = new String[] {"mobGriefing", entityName, "inherit"};

    // Record Expectations.
    new Expectations(entityMobGriefingData) {
      {
        commandSender.getEntityWorld();
        result = world;

        EntityMobGriefingData.forWorld(world);
        result = entityMobGriefingData;
      }
    };

    // Call the method under test.
    command.processCommand(commandSender, commandWords);

    // Verify expectations.
    new Verifications() {
      {
        entityMobGriefingData.setMobGriefingValue(anyString, (MobGriefingValue) any);
        times = 0;
      }
    };
  }

  /**
   * Test that a {@link WrongUsageException} is thrown when the command words are "mobGriefing", a
   * name which is not an entity and a third word.
   */
  @Test(expected = WrongUsageException.class)
  public void testProcessCommand_mobGriefingNotEntityNameThirdWord_exception() {
    // Set up test data.
    EntityMobGriefingData entityMobGriefingData = new EntityMobGriefingData("");

    String[] commandWords = new String[] {"mobGriefing", "entityName1", "inherit"};

    // Record Expectations.
    new Expectations(entityMobGriefingData) {
      {
        commandSender.getEntityWorld();
        result = world;

        EntityMobGriefingData.forWorld(world);
        result = entityMobGriefingData;
      }
    };

    // Call the method under test.
    command.processCommand(commandSender, commandWords);

    // Verify expectations.
    new Verifications() {
      {
        entityMobGriefingData.setMobGriefingValue(anyString, (MobGriefingValue) any);
        times = 0;
      }
    };
  }

  /**
   * Test that a {@link WrongUsageException} is thrown when there are four command words and the
   * first is "mobGriefing".
   */
  @Test(expected = WrongUsageException.class)
  public void testProcessCommand_mobGriefingFourWords_exception() {
    // Set up test data.
    EntityMobGriefingData entityMobGriefingData = new EntityMobGriefingData("");

    String[] commandWords = new String[] {"mobGriefing", "entityName1", "inherit", ""};

    // Record Expectations.
    new Expectations(entityMobGriefingData) {
      {
        commandSender.getEntityWorld();
        result = world;

        EntityMobGriefingData.forWorld(world);
        result = entityMobGriefingData;
      }
    };

    // Call the method under test.
    command.processCommand(commandSender, commandWords);

    // Verify expectations.
    new Verifications() {
      {
        entityMobGriefingData.setMobGriefingValue(anyString, (MobGriefingValue) any);
        times = 0;
      }
    };
  }

  /**
   * Test that the parent handles the command when the first word is not "mobGriefing".
   */
  @Test
  public void testProcessCommand_notMobGriefing_handledByParent(
      @Mocked CommandGameRule parentCommand) {
    // Set up test data.
    String[] commandWords = new String[] {"notMobGriefing"};

    // Call the method under test.
    command.processCommand(commandSender, commandWords);

    // Verify expectations.
    new Verifications() {
      {
        parentCommand.processCommand(commandSender, commandWords);
      }
    };
  }
}
