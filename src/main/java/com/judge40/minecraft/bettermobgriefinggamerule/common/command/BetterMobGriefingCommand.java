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

import static net.minecraft.command.Commands.argument;
import static net.minecraft.command.Commands.literal;

import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import com.judge40.minecraft.bettermobgriefinggamerule.common.world.EntityMobGriefingData;
import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.context.CommandContext;
import net.minecraft.command.CommandSource;
import net.minecraft.command.arguments.EntitySummonArgument;
import net.minecraft.command.arguments.SuggestionProviders;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.GameRules;
import net.minecraft.world.GameRules.BooleanValue;

/**
 * A custom command handler for the mob griefing game rule, it allows auto-completion and assignment
 * of {@link MobGriefingValue EntityMobGriefingValues}.
 */
public class BetterMobGriefingCommand {

  private static final String RULE_NAME = "betterMobGriefing";
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
  public static void register(CommandDispatcher<CommandSource> dispatcher) {
    dispatcher.register(
        literal(RULE_PARENT)
            .then(
                literal(RULE_NAME)
                    .requires(source -> source.hasPermissionLevel(2))
                    .executes(BetterMobGriefingCommand::listMobGriefing)
                    .then(
                        argument(RULE_VALUE, BoolArgumentType.bool())
                            .executes(BetterMobGriefingCommand::setGlobalMobGriefing)
                    )
                    .then(
                        argument(RULE_TARGET, EntitySummonArgument.entitySummon())
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
  private static int listMobGriefing(CommandContext<CommandSource> context) {
    CommandSource source = context.getSource();
    EntityMobGriefingData data = EntityMobGriefingData.forServer(source.getServer());

    GameRules gameRules = source.getServer().getGameRules();
    BooleanValue mobGriefing = gameRules.get(GameRules.MOB_GRIEFING);

    String values = String.format("\n%s = %s\n%s", RULE_NAME, mobGriefing, data);
    TranslationTextComponent message = new TranslationTextComponent(RULE_QUERY_KEY, RULE_NAME,
        values);
    source.sendFeedback(message, true);

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
  private static int showMobGriefing(CommandContext<CommandSource> context) {
    CommandSource source = context.getSource();
    EntityMobGriefingData data = EntityMobGriefingData.forServer(source.getServer());

    ResourceLocation targetId = context.getArgument(RULE_TARGET, ResourceLocation.class);
    String ruleName = String.format("%s %s", RULE_NAME, targetId);
    MobGriefingValue mobGriefingValue = data.getMobGriefingValue(targetId);

    TranslationTextComponent message = new TranslationTextComponent(RULE_QUERY_KEY, ruleName,
        mobGriefingValue);
    source.sendFeedback(message, true);

    return mobGriefingValue.ordinal();
  }

  /**
   * Set the global mob griefing value based on the command context, which requires {@value
   * RULE_VALUE} populated as the boolean to set.
   *
   * @param context The command context to process.
   * @return The int representation of the set value where 0 is false, 1 is true.
   */
  private static int setGlobalMobGriefing(CommandContext<CommandSource> context) {
    CommandSource source = context.getSource();
    GameRules gameRules = source.getServer().getGameRules();
    BooleanValue mobGriefing = gameRules.get(GameRules.MOB_GRIEFING);
    mobGriefing.func_223554_b(context, RULE_VALUE);

    TranslationTextComponent message = new TranslationTextComponent(RULE_SET_KEY, RULE_NAME,
        mobGriefing);
    source.sendFeedback(message, true);

    return mobGriefing.func_223557_c();
  }

  /**
   * Set the entity mob griefing value based on the command context, which requires two arguments be
   * populated. {@value RULE_TARGET} is the {@link ResourceLocation} of the target to set the value
   * for and {@value RULE_VALUE} is the {@link MobGriefingValue} to set.
   *
   * @param context The command context to process.
   * @return The int representation of the set value where 0 is false, 1 is true and 2 is inherit.
   */
  private static int setEntityMobGriefing(CommandContext<CommandSource> context) {
    ResourceLocation targetId = context.getArgument(RULE_TARGET, ResourceLocation.class);
    MobGriefingValue mobGriefingValue = context.getArgument(RULE_VALUE, MobGriefingValue.class);

    CommandSource source = context.getSource();
    EntityMobGriefingData data = EntityMobGriefingData.forServer(source.getServer());
    data.setMobGriefingValue(targetId, mobGriefingValue);

    String ruleName = String.format("%s %s", RULE_NAME, targetId);
    TranslationTextComponent message = new TranslationTextComponent(RULE_SET_KEY, ruleName,
        mobGriefingValue);
    source.sendFeedback(message, true);

    return mobGriefingValue.ordinal();
  }
}
