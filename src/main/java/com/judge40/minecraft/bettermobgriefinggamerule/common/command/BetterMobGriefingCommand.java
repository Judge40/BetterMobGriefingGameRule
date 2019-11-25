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

package com.judge40.minecraft.bettermobgriefinggamerule.common.command;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;
import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import com.judge40.minecraft.bettermobgriefinggamerule.common.world.EntityMobGriefingData;

import net.minecraft.command.CommandException;
import net.minecraft.command.CommandGameRule;
import net.minecraft.command.CommandResultStats;
import net.minecraft.command.ICommandSender;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityList;
import net.minecraft.entity.EntityLiving;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.TextComponentString;
import net.minecraft.world.GameRules;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A custom command handler for the mob griefing game rule, it allows auto-completion and assignment
 * of {@link MobGriefingValue EntityMobGriefingValues}.
 */
public class BetterMobGriefingCommand extends CommandGameRule {

  /**
   * Process the command and perform the relevant actions. The mob griefing game rule will either be
   * updated or the current value will be output, depending on the command entered. Other commands
   * will be routed to the default handling.
   * 
   * @param commandSender The entity the command was sent by.
   * @param commandWords A string array of words making up the command.
   * @throws CommandException when the command entered is invalid.
   */
  @Override
  public void execute(MinecraftServer server, ICommandSender commandSender, String[] commandWords)
      throws CommandException {
    // Only handle processing of mob griefing game rules.
    if (commandWords.length >= 1 && commandWords[0].equals(BetterMobGriefingGameRule.GLOBAL_RULE)) {
      EntityMobGriefingData entityMobGriefingData =
          EntityMobGriefingData.forWorld(commandSender.getEntityWorld());
      GameRules gameRules = commandSender.getEntityWorld().getGameRules();

      if (commandWords.length == 1) {
        // If the length is one then output the mob griefing values for both the global and entity
        // rules.
        String globalMobGriefingValue = gameRules.getString(BetterMobGriefingGameRule.GLOBAL_RULE);

        String globalOutput =
            String.format("%s = %s", BetterMobGriefingGameRule.GLOBAL_RULE, globalMobGriefingValue);
        commandSender.sendMessage(new TextComponentString(globalOutput));

        if (!entityMobGriefingData.toString().isEmpty()) {
          String[] entityValues = entityMobGriefingData.toString().split(", ");

          for (String entityValue : entityValues) {
            String entityOutput =
                String.format("%s %s", BetterMobGriefingGameRule.GLOBAL_RULE, entityValue);
            commandSender.sendMessage(new TextComponentString(entityOutput));
          }
        }
      } else if (commandWords.length == 2) {
        if (commandWords[1].equals(MobGriefingValue.TRUE.toExternalForm())
            || commandWords[1].equals(MobGriefingValue.FALSE.toExternalForm())) {
          // If the command length is two and the second word is true or false then pass handling to
          // the default handler to set the global mob griefing rule.
          super.execute(server, commandSender, commandWords);
        } else {
          ResourceLocation entityType = new ResourceLocation(commandWords[1]);
          String entityName = entityType.getPath();
          MobGriefingValue entityMobGriefingValue =
              entityMobGriefingData.getMobGriefingValue(entityName);

          // If the second word is an entity name with its own value, output the entity name and mob
          // griefing value. Otherwise inform the sender that there is currently no entity rule for
          // that entity.
          if (entityMobGriefingValue != null) {
            String message = String.format("%s %s = %s", BetterMobGriefingGameRule.GLOBAL_RULE,
                entityName, entityMobGriefingValue.toExternalForm());
            commandSender.sendMessage(new TextComponentString(message));
            commandSender.setCommandStat(CommandResultStats.Type.QUERY_RESULT,
                gameRules.getInt(commandWords[0]));
          } else {
            String message =
                String.format("%s %s", BetterMobGriefingGameRule.GLOBAL_RULE, commandWords[1]);
            throw new CommandException("commands.gamerule.norule", new Object[] {message});
          }
        }
      } else if (commandWords.length == 3) {
        // If the second word is a valid entity name then try and set the entity rule to the value
        // given in the third word, otherwise throw a wrong usage exception.
        ResourceLocation entityType = new ResourceLocation(commandWords[1]);
        String entityName = entityType.getPath();
        Class<? extends Entity> entityClass = EntityList.getClass(entityType);

        if (entityClass != null && EntityLiving.class.isAssignableFrom(entityClass)) {
          try {
            MobGriefingValue mobGriefingValue = MobGriefingValue.toEnumeration(commandWords[2]);
            entityMobGriefingData.setMobGriefingValue(entityType.getPath(),
                mobGriefingValue);
            notifyCommandListener(commandSender, this, "commands.gamerule.success", new Object[] {
                String.format("%s %s", commandWords[0], entityName), commandWords[2]});
          } catch (IllegalArgumentException iae) {
            String exceptionMessage = String.format("/gamerule %s <entity name> %s|%s|%s",
                BetterMobGriefingGameRule.GLOBAL_RULE, MobGriefingValue.TRUE.toExternalForm(),
                MobGriefingValue.FALSE.toExternalForm(), MobGriefingValue.INHERIT.toExternalForm());
            throw new CommandException(exceptionMessage);
          }
        } else {
          throw new CommandException(
              String.format("%s is not a valid entity name", commandWords[1]));
        }
      } else {
        // Throw a wrong usage exception where there are too many words.
        String exceptionMessage = String.format("/gamerule %s <entity name> %s|%s|%s",
            BetterMobGriefingGameRule.GLOBAL_RULE, MobGriefingValue.TRUE.toExternalForm(),
            MobGriefingValue.FALSE.toExternalForm(), MobGriefingValue.INHERIT.toExternalForm());
        throw new CommandException(exceptionMessage);
      }
    } else {
      super.execute(server, commandSender, commandWords);
    }
  }

  @Override
  public List<String> getTabCompletions(MinecraftServer server, ICommandSender commandSender,
      String[] commandWords, BlockPos pos) {
    List<String> tabCompletionOptions;

    if (commandWords.length == 1
        || !commandWords[0].equals(BetterMobGriefingGameRule.GLOBAL_RULE)) {
      // When the first word is being completed or the first word is not the mobGriefing game rule
      // the parent's behavior will handle tab completion.
      tabCompletionOptions = super.getTabCompletions(server, commandSender, commandWords, pos);
    } else if (commandWords.length <= 3) {
      // When the first word is the mobGriefing game rule true and false are always possible words.
      List<String> possibleWords = new ArrayList<>();
      possibleWords.add(MobGriefingValue.TRUE.toExternalForm());
      possibleWords.add(MobGriefingValue.FALSE.toExternalForm());

      if (commandWords.length == 2) {
        // When the command length is two then registered entity names are also possible words.
        EntityMobGriefingData entityMobGriefingData =
            EntityMobGriefingData.forWorld(commandSender.getEntityWorld());
        List<String> entityNames =
            new ArrayList<>(entityMobGriefingData.getRegisteredEntityNames());
        Collections.sort(entityNames);
        possibleWords.addAll(entityNames);
      } else {
        // When the command length is three then an entity rule is being set and inherit is a
        // possible word.
        possibleWords.add(MobGriefingValue.INHERIT.toExternalForm());
      }

      tabCompletionOptions = getListOfStringsMatchingLastWord(commandWords,
          possibleWords.toArray(new String[possibleWords.size()]));
    } else {
      // Default for no results is an empty list.
      tabCompletionOptions = Collections.emptyList();
    }

    return tabCompletionOptions;
  }
}
