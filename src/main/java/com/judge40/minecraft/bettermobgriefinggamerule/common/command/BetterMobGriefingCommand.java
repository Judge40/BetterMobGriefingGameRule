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

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import com.judge40.minecraft.bettermobgriefinggamerule.common.world.EntityMobGriefingData;
import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.context.CommandContext;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.arguments.EntitySummonArgument;
import net.minecraft.commands.synchronization.SuggestionProviders;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.GameRules;
import net.minecraft.world.level.GameRules.BooleanValue;

/**
 * A custom command handler for the mob griefing game rule, it allows auto-completion and assignment
 * of {@link MobGriefingValue EntityMobGriefingValues}.
 */
public class BetterMobGriefingCommand {

  private static final String RULE_NAME = "mobGriefing";
  private static final String RULE_PARENT = "gamerule";
  private static final String RULE_TARGET = "entity";
  private static final String RULE_VALUE = "value";
  private static final String RULE_SET_KEY = "commands.gamerule.set";
  private static final String RULE_QUERY_KEY = "commands.gamerule.query";

  /**
   * Register the {@value RULE_NAME} game rule.
   * <li>List all values: {@code /}{@value RULE_PARENT} {@value RULE_NAME}
   * <li>List specific value: {@code /}{@value RULE_PARENT} {@value RULE_NAME} {@code
   * namespace:path}
   * <li>Set specific value: {@code /}{@value RULE_PARENT} {@value RULE_NAME} {@code namespace:path
   * true|false|inherit}
   *
   * @param dispatcher The command dispatcher to register with.
   */
  public static void register(CommandDispatcher<CommandSourceStack> dispatcher) {
    dispatcher.register(
        literal(RULE_PARENT)
            .then(
                literal(RULE_NAME)
                    .requires(source -> source.hasPermission(2))
                    .executes(BetterMobGriefingCommand::listMobGriefing)
                    .then(
                        argument(RULE_VALUE, BoolArgumentType.bool())
                            .executes(BetterMobGriefingCommand::setGlobalMobGriefing)
                    )
                    .then(
                        argument(RULE_TARGET, EntitySummonArgument.id())
                            .executes(BetterMobGriefingCommand::showMobGriefing)
                            .suggests(SuggestionProviders.SUMMONABLE_ENTITIES)
                            .then(
                                argument(RULE_VALUE, new BetterMobGriefingArgument())
                                    .executes(BetterMobGriefingCommand::setEntityMobGriefing)
                            )
                    )
            )
    );
  }

  /**
   * Show the mob griefing values for all targets, the result is sent to the command source.
   *
   * @param context The command context to process.
   * @return The number of targets.
   */
  private static int listMobGriefing(CommandContext<CommandSourceStack> context) {
    CommandSourceStack source = context.getSource();
    EntityMobGriefingData data = EntityMobGriefingData.forServer(source.getServer());

    GameRules gameRules = source.getServer().getGameRules();
    BooleanValue mobGriefing = gameRules.getRule(GameRules.RULE_MOBGRIEFING);

    String values = String.format("\n%s = %s\n%s", RULE_NAME, mobGriefing, data);
    TranslatableComponent message = new TranslatableComponent(RULE_QUERY_KEY, RULE_NAME, values);
    source.sendSuccess(message, true);

    return data.size();
  }

  /**
   * Show the mob griefing value based on the command context, which requires one argument be
   * populated. {@value RULE_TARGET} is the {@link ResourceLocation} of the target to get the value
   * for. The result is sent to the command source.
   *
   * @param context The command context to process.
   * @return The int representation of the current value where 0 is false, 1 is true and 2 is
   *     inherit.
   */
  private static int showMobGriefing(CommandContext<CommandSourceStack> context) {
    CommandSourceStack source = context.getSource();
    EntityMobGriefingData data = EntityMobGriefingData.forServer(source.getServer());

    ResourceLocation targetId = context.getArgument(RULE_TARGET, ResourceLocation.class);
    String ruleName = String.format("%s %s", RULE_NAME, targetId);
    MobGriefingValue mobGriefingValue = data.getMobGriefingValue(targetId);

    TranslatableComponent message = new TranslatableComponent(RULE_QUERY_KEY, ruleName,
        mobGriefingValue);
    source.sendSuccess(message, true);

    return mobGriefingValue.ordinal();
  }

  /**
   * Set the global mob griefing value based on the command context, which requires {@value
   * RULE_VALUE} populated as the boolean to set.
   *
   * @param context The command context to process.
   * @return The int representation of the set value where 0 is false, 1 is true.
   */
  private static int setGlobalMobGriefing(CommandContext<CommandSourceStack> context) {
    CommandSourceStack source = context.getSource();
    GameRules gameRules = source.getServer().getGameRules();
    BooleanValue mobGriefing = gameRules.getRule(GameRules.RULE_MOBGRIEFING);
    mobGriefing.setFromArgument(context, RULE_VALUE);

    TranslatableComponent message = new TranslatableComponent(RULE_SET_KEY, RULE_NAME,
        mobGriefing);
    source.sendSuccess(message, true);

    return mobGriefing.getCommandResult();
  }

  /**
   * Set the entity mob griefing value based on the command context, which requires two arguments be
   * populated. {@value RULE_TARGET} is the {@link ResourceLocation} of the target to set the value
   * for and {@value RULE_VALUE} is the {@link MobGriefingValue} to set.
   *
   * @param context The command context to process.
   * @return The int representation of the set value where 0 is false, 1 is true and 2 is inherit.
   */
  private static int setEntityMobGriefing(CommandContext<CommandSourceStack> context) {
    ResourceLocation targetId = context.getArgument(RULE_TARGET, ResourceLocation.class);
    MobGriefingValue mobGriefingValue = context.getArgument(RULE_VALUE, MobGriefingValue.class);

    CommandSourceStack source = context.getSource();
    EntityMobGriefingData data = EntityMobGriefingData.forServer(source.getServer());
    data.setMobGriefingValue(targetId, mobGriefingValue);

    String ruleName = String.format("%s %s", RULE_NAME, targetId);
    TranslatableComponent message = new TranslatableComponent(RULE_SET_KEY, ruleName,
        mobGriefingValue);
    source.sendSuccess(message, true);

    return mobGriefingValue.ordinal();
  }
}
