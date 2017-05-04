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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.judge40.minecraft.bettermobgriefinggamerule.BetterMobGriefingGameRule;
import com.judge40.minecraft.bettermobgriefinggamerule.MobGriefingValue;
import com.judge40.minecraft.bettermobgriefinggamerule.world.EntityMobGriefingData;

import net.minecraft.command.CommandGameRule;
import net.minecraft.command.ICommandSender;
import net.minecraft.command.WrongUsageException;
import net.minecraft.entity.EntityList;
import net.minecraft.entity.EntityLiving;
import net.minecraft.util.ChatComponentText;
import net.minecraft.world.GameRules;

/**
 * A custom command handler for the mobGriefing game rule, it allows auto-completion and assignment
 * of {@link EntityMobGriefingValue EntityMobGriefingValues}.
 */
public class BetterMobGriefingCommand extends CommandGameRule {

  /*
   * (non-Javadoc)
   * 
   * @see net.minecraft.command.CommandGameRule#addTabCompletionOptions(net.minecraft.command.
   * ICommandSender, java.lang.String[])
   */
  @Override
  public List<?> addTabCompletionOptions(ICommandSender commandSender, String[] commandWords) {
    List<?> tabCompletionOptions = null;

    if (commandWords.length == 1 || !commandWords[0].equals(BetterMobGriefingGameRule.ORIGINAL)) {
      // When the first word is being completed or the first word is not the mobGriefing game rule
      // the parent's behavior will handle tab completion.
      tabCompletionOptions = super.addTabCompletionOptions(commandSender, commandWords);
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
    }

    return tabCompletionOptions;
  }

  /*
   * (non-Javadoc)
   * 
   * @see net.minecraft.command.CommandGameRule#processCommand(net.minecraft.command.ICommandSender,
   * java.lang.String[])
   */
  @Override
  public void processCommand(ICommandSender commandSender, String[] commandWords) {
    if (commandWords.length >= 1 && commandWords[0].equals(BetterMobGriefingGameRule.ORIGINAL)) {
      EntityMobGriefingData entityMobGriefingData =
          EntityMobGriefingData.forWorld(commandSender.getEntityWorld());

      if (commandWords.length == 1) {
        GameRules gameRules = commandSender.getEntityWorld().getGameRules();
        String globalMobGriefingValue =
            gameRules.getGameRuleStringValue(BetterMobGriefingGameRule.ORIGINAL);

        String globalOutput =
            String.format("%s = %s", BetterMobGriefingGameRule.ORIGINAL, globalMobGriefingValue);
        commandSender.addChatMessage(new ChatComponentText(globalOutput));

        if (!entityMobGriefingData.toString().isEmpty()) {
          String[] entityValues = entityMobGriefingData.toString().split(", ");

          for (String entityValue : entityValues) {
            String entityOutput =
                String.format("%s %s", BetterMobGriefingGameRule.ORIGINAL, entityValue);
            commandSender.addChatMessage(new ChatComponentText(entityOutput));
          }
        }
      } else if (commandWords.length == 2) {
        if (commandWords[1].equals(MobGriefingValue.TRUE.toExternalForm())
            || commandWords[1].equals(MobGriefingValue.FALSE.toExternalForm())) {
          super.processCommand(commandSender, commandWords);
        } else {
          String entityName = commandWords[1];
          MobGriefingValue entityMobGriefingValue =
              entityMobGriefingData.getMobGriefingValue(entityName);

          if (entityMobGriefingValue != null) {
            String message = String.format("%s %s = %s", BetterMobGriefingGameRule.ORIGINAL,
                entityName, entityMobGriefingValue.toExternalForm());
            commandSender.addChatMessage(new ChatComponentText(message));
          } else {
            String message =
                String.format("%s %s", BetterMobGriefingGameRule.ORIGINAL, commandWords[1]);
            func_152373_a(commandSender, this, "commands.gamerule.norule", new Object[] {message});
          }
        }

      } else if (commandWords.length == 3) {
        String entityName = commandWords[1];
        Class<?> entityClass = (Class<?>) EntityList.stringToClassMapping.get(entityName);

        if (entityClass != null && EntityLiving.class.isAssignableFrom(entityClass)) {
          try {
            MobGriefingValue mobGriefingValue = MobGriefingValue.toEnumeration(commandWords[2]);
            entityMobGriefingData.setMobGriefingValue(entityName, mobGriefingValue);
            func_152373_a(commandSender, this, "commands.gamerule.success", new Object[0]);
          } catch (IllegalArgumentException e) {
            String exceptionMessage = String.format("/gamerule %s <entity name> %s|%s|%s",
                BetterMobGriefingGameRule.ORIGINAL, MobGriefingValue.TRUE.toExternalForm(),
                MobGriefingValue.FALSE.toExternalForm(), MobGriefingValue.INHERIT.toExternalForm());
            throw new WrongUsageException(exceptionMessage);
          }
        } else {
          throw new WrongUsageException(String.format("%s is not a valid entity name", entityName));
        }
      } else {
        String exceptionMessage = String.format("/gamerule %s <entity name> %s|%s|%s",
            BetterMobGriefingGameRule.ORIGINAL, MobGriefingValue.TRUE.toExternalForm(),
            MobGriefingValue.FALSE.toExternalForm(), MobGriefingValue.INHERIT.toExternalForm());
        throw new WrongUsageException(exceptionMessage);
      }
    } else {
      super.processCommand(commandSender, commandWords);
    }
  }
}
