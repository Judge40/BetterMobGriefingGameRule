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
import com.judge40.minecraft.bettermobgriefinggamerule.world.BetterMobGriefingGameRuleWorldSavedData;

import net.minecraft.command.CommandGameRule;
import net.minecraft.command.ICommandSender;
import net.minecraft.command.WrongUsageException;
import net.minecraft.entity.EntityList;
import net.minecraft.entity.EntityLiving;
import net.minecraft.util.ChatComponentText;
import net.minecraft.world.GameRules;

/**
 *
 */
public class BetterMobGriefingGameRuleCommandGameRule extends CommandGameRule {

  /*
   * (non-Javadoc)
   * 
   * @see net.minecraft.command.CommandGameRule#processCommand(net.minecraft.command.ICommandSender,
   * java.lang.String[])
   */
  @Override
  public void processCommand(ICommandSender commandSender, String[] commandWords) {
    if (commandWords.length >= 1 && commandWords.length <= 3
        && commandWords[0].equals(BetterMobGriefingGameRule.ORIGINAL)) {
      BetterMobGriefingGameRuleWorldSavedData worldSavedData =
          BetterMobGriefingGameRuleWorldSavedData.forWorld(commandSender.getEntityWorld());

      if (commandWords.length == 1) {

        GameRules gameRules = commandSender.getEntityWorld().getGameRules();
        String mobGriefingValue =
            gameRules.getGameRuleStringValue(BetterMobGriefingGameRule.ORIGINAL);

        StringBuilder messageBuilder = new StringBuilder();
        messageBuilder
            .append(String.format("%s = %s", BetterMobGriefingGameRule.ORIGINAL, mobGriefingValue));

        if (!worldSavedData.toString().isEmpty()) {
          messageBuilder.append(", ");
          messageBuilder.append(worldSavedData.toString());
        }

        commandSender.addChatMessage(new ChatComponentText(messageBuilder.toString()));
      } else if (commandWords.length == 2) {

        if (commandWords[1].equals(Boolean.toString(true))
            || commandWords[1].equals(Boolean.toString(false))) {
          super.processCommand(commandSender, commandWords);
        } else {

          if (worldSavedData.entityNamesToMobGriefingValue.containsKey(commandWords[1])) {
            String message = String.format("%s %s = %s", BetterMobGriefingGameRule.ORIGINAL,
                commandWords[1], worldSavedData.entityNamesToMobGriefingValue.get(commandWords[1]));
            commandSender.addChatMessage(new ChatComponentText(message));
          } else {
            String message =
                String.format("%s %s", BetterMobGriefingGameRule.ORIGINAL, commandWords[1]);
            func_152373_a(commandSender, this, "commands.gamerule.norule", new Object[] {message});
          }
        }

      } else if (commandWords.length == 3) {
        Class<?> entityClass = (Class<?>) EntityList.stringToClassMapping.get(commandWords[1]);

        if (entityClass != null && EntityLiving.class.isAssignableFrom(entityClass)) {

          if (commandWords[2].equals(Boolean.toString(true))
              || commandWords[2].equals(Boolean.toString(false))) {
            worldSavedData.entityNamesToMobGriefingValue.put(commandWords[1], commandWords[2]);
            func_152373_a(commandSender, this, "commands.gamerule.success", new Object[0]);
          } else {
            throw new WrongUsageException("/gamerule mobGriefing <entity name> true|false",
                new Object[0]);
          }
        } else {
          throw new WrongUsageException(
              String.format("%s is not a valid entity name", commandWords[1]));
        }
      }
    } else {
      super.processCommand(commandSender, commandWords);
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see net.minecraft.command.CommandGameRule#addTabCompletionOptions(net.minecraft.command.
   * ICommandSender, java.lang.String[])
   */
  @Override
  public List<?> addTabCompletionOptions(ICommandSender commandSender, String[] commandWords) {
    List<?> tabCompletionOptions = null;

    if (commandWords.length > 1 && commandWords[0].equals(BetterMobGriefingGameRule.ORIGINAL)) {
      List<String> possibleWords = new ArrayList<>();
      possibleWords.add(Boolean.toString(true));
      possibleWords.add(Boolean.toString(false));

      if (commandWords.length == 2) {
        BetterMobGriefingGameRuleWorldSavedData betterMobGriefingGameRuleWorldSavedData =
            BetterMobGriefingGameRuleWorldSavedData.forWorld(commandSender.getEntityWorld());
        List<String> entityNames = new ArrayList<>(
            betterMobGriefingGameRuleWorldSavedData.entityNamesToMobGriefingValue.keySet());
        Collections.sort(entityNames);

        possibleWords.addAll(entityNames);
        tabCompletionOptions = getListOfStringsMatchingLastWord(commandWords,
            possibleWords.toArray(new String[possibleWords.size()]));
      } else if (commandWords.length == 3) {
        Class<?> entityClass = (Class<?>) EntityList.stringToClassMapping.get(commandWords[1]);

        if (entityClass != null) {
          tabCompletionOptions = getListOfStringsMatchingLastWord(commandWords,
              possibleWords.toArray(new String[possibleWords.size()]));
        }
      }
    } else {
      tabCompletionOptions = super.addTabCompletionOptions(commandSender, commandWords);
    }

    return tabCompletionOptions;
  }
}
