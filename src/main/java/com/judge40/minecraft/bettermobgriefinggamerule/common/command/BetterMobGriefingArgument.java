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

package com.judge40.minecraft.bettermobgriefinggamerule.common.command;

import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import com.judge40.minecraft.bettermobgriefinggamerule.common.ModInfoConstants;
import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import java.util.Arrays;
import java.util.Collection;
import java.util.Locale;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import net.minecraft.network.chat.TranslatableComponent;

public class BetterMobGriefingArgument implements ArgumentType<MobGriefingValue> {

  private static final DynamicCommandExceptionType INVALID_VALUE = new DynamicCommandExceptionType(
      input -> new TranslatableComponent(
          ModInfoConstants.ID + ".parsing.mobGriefingValue.invalid", input));

  private static final Collection<String> EXAMPLES = Arrays.stream(MobGriefingValue.values())
      .map(MobGriefingValue::toString).collect(Collectors.toList());

  @Override
  public MobGriefingValue parse(StringReader reader) throws CommandSyntaxException {
    String value = reader.readString();

    try {
      return MobGriefingValue.toEnumeration(value);
    } catch (IllegalArgumentException e) {
      reader.setCursor(reader.getCursor());
      throw INVALID_VALUE.createWithContext(reader, value);
    }
  }

  @Override
  public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context,
      SuggestionsBuilder builder) {
    String remaining = builder.getRemaining().toLowerCase(Locale.ENGLISH);

    for (MobGriefingValue value : MobGriefingValue.values()) {
      String externalFrom = value.toString();

      if (externalFrom.startsWith(remaining)) {
        builder.suggest(externalFrom);
      }
    }

    return builder.buildFuture();
  }

  @Override
  public Collection<String> getExamples() {
    return EXAMPLES;
  }
}
