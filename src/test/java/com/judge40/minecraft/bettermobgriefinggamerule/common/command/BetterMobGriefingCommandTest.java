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

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.judge40.minecraft.bettermobgriefinggamerule.TestUtils;
import com.judge40.minecraft.bettermobgriefinggamerule.common.MobGriefingValue;
import com.judge40.minecraft.bettermobgriefinggamerule.common.world.EntityMobGriefingData;
import com.mojang.brigadier.Command;
import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.context.ParsedArgument;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.suggestion.Suggestion;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import com.mojang.brigadier.tree.CommandNode;
import com.mojang.brigadier.tree.RootCommandNode;
import com.mojang.datafixers.DataFixer;
import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.function.Predicate;
import net.minecraft.command.CommandSource;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.vector.Vector2f;
import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.world.GameRules;
import net.minecraft.world.GameRules.BooleanValue;
import net.minecraft.world.server.ServerWorld;
import net.minecraft.world.storage.DimensionSavedDataManager;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * The unit tests for {@link BetterMobGriefingCommand}.
 */
class BetterMobGriefingCommandTest {

  private CommandNode<CommandSource> mobGriefingCommand;

  private MinecraftServer server;
  private ServerWorld world;
  private CommandSource commandSource;
  private CommandContext<CommandSource> commandContext;

  private Map<String, ParsedArgument<CommandSource, ?>> commandArguments;

  @BeforeAll
  static void setUpBeforeAll() throws IllegalAccessException {
    TestUtils.initializeTestEnvironment();
  }

  @BeforeEach
  void setUp() {
    CommandDispatcher<CommandSource> dispatcher = new CommandDispatcher<>();
    BetterMobGriefingCommand.register(dispatcher);

    RootCommandNode<CommandSource> rootCommand = dispatcher.getRoot();
    CommandNode<CommandSource> gameRuleCommand = rootCommand.getChild("gamerule");
    mobGriefingCommand = gameRuleCommand.getChild("mobGriefing");

    server = mock(MinecraftServer.class);
    when(server.getGameRules()).thenReturn(new GameRules());
    world = mock(ServerWorld.class);
    when(server.func_241755_D_()).thenReturn(world);

    DataFixer dataFixer = mock(DataFixer.class);
    when(world.getSavedData()).thenReturn(new DimensionSavedDataManager(new File(""), dataFixer));

    commandSource = new CommandSource(server, Vector3d.ZERO, Vector2f.ZERO, world, 2, "",
        new StringTextComponent(""), server, null);
    commandSource = spy(commandSource);

    commandArguments = new HashMap<>();
    commandContext = new CommandContext<>(commandSource, null, commandArguments, null, null, null,
        null, null, null, false);

  }

  @ParameterizedTest(name = "Should return {1} from requirement check when permission level is {0}")
  @CsvSource({"0, false", "1, false", "2, true", "3, true"})
  void shouldRequirePermissionLevelTwo(int permissionLevel, boolean hasPermission) {
    // Given.
    Predicate<CommandSource> commandRequirement = mobGriefingCommand.getRequirement();

    CommandSource commandSource = new CommandSource(server, Vector3d.ZERO, Vector2f.ZERO, world,
        permissionLevel, "", new StringTextComponent(""), server, null);

    // When.
    boolean result = commandRequirement.test(commandSource);

    // Then.
    assertThat("Unexpected suggestion.", result, is(hasPermission));
  }

  @Test
  void shouldOutputListOfMobGriefing() throws CommandSyntaxException {
    // Given.
    EntityMobGriefingData data = EntityMobGriefingData.forServer(server);
    data.setMobGriefingValue(new ResourceLocation("test:entity1"), MobGriefingValue.FALSE);
    data.setMobGriefingValue(new ResourceLocation("test:entity2"), MobGriefingValue.TRUE);
    data.setMobGriefingValue(new ResourceLocation("test:entity3"), MobGriefingValue.INHERIT);

    Command<CommandSource> command = mobGriefingCommand.getCommand();

    // When.
    int result = command.run(commandContext);

    // Then.
    assertThat("Unexpected command result.", result, is(3));
    verify(commandSource).sendFeedback(any(ITextComponent.class), eq(true));
  }

  @ParameterizedTest(name = "Should suggest {1} when the argument is {0}")
  @CsvSource({"Tru, true", "fALS, false"})
  void shouldSuggestGlobalMobGriefingValue(String input, String suggestion)
      throws CommandSyntaxException, ExecutionException, InterruptedException {
    // Given.
    CommandNode<CommandSource> valueArgument = mobGriefingCommand.getChild("value");

    // When.
    CompletableFuture<Suggestions> completableFuture = valueArgument
        .listSuggestions(commandContext, new SuggestionsBuilder(input, 0));

    // Then.
    List<Suggestion> suggestions = completableFuture.get().getList();
    assertThat("Unexpected suggestion size.", suggestions.size(), CoreMatchers.is(1));
    assertThat("Unexpected suggestion.", suggestions.get(0).getText(), CoreMatchers.is(suggestion));
  }

  @ParameterizedTest(name = "Should suggest {1} values when the argument is {0}")
  @CsvSource({"'', 2", "x, 0"})
  void shouldSuggestGlobalMobGriefingValues(String input, int count)
      throws CommandSyntaxException, ExecutionException, InterruptedException {
    // Given.
    CommandNode<CommandSource> valueArgument = mobGriefingCommand.getChild("value");

    // When.
    CompletableFuture<Suggestions> completableFuture = valueArgument
        .listSuggestions(commandContext, new SuggestionsBuilder(input, 0));

    // Then.
    List<Suggestion> suggestions = completableFuture.get().getList();
    assertThat("Unexpected suggestion size.", suggestions.size(), CoreMatchers.is(count));
  }

  @ParameterizedTest(name = "Should set the global mobGriefing value when the argument is {0}")
  @ValueSource(booleans = {true, false})
  void shouldSetGlobalMobGriefingWhenBoolean(boolean input) throws CommandSyntaxException {
    // Given.
    CommandNode<CommandSource> valueArgument = mobGriefingCommand.getChild("value");
    Command<CommandSource> command = valueArgument.getCommand();

    commandArguments.put("value", new ParsedArgument<>(0, 0, input));

    // When.
    int result = command.run(commandContext);

    // Then.
    assertThat("Unexpected command result.", result, is(input ? 1 : 0));
    BooleanValue mobGriefing = server.getGameRules().get(GameRules.MOB_GRIEFING);
    assertThat("Unexpected mob griefing value.", mobGriefing.get(), is(input));
    verify(commandSource).sendFeedback(any(ITextComponent.class), eq(true));
  }

  @ParameterizedTest(name = "Should suggest {1} when the argument is {0}")
  @CsvSource({"creeper, minecraft:creeper", "GHas, minecraft:ghast"})
  void shouldSuggestEntityName(String input, String suggestion)
      throws CommandSyntaxException, ExecutionException, InterruptedException {
    // Given.
    CommandNode<CommandSource> entityArgument = mobGriefingCommand.getChild("entity");

    // When.
    CompletableFuture<Suggestions> completableFuture = entityArgument
        .listSuggestions(commandContext, new SuggestionsBuilder(input, 0));

    // Then.
    List<Suggestion> suggestions = completableFuture.get().getList();
    assertThat("Unexpected suggestion size.", suggestions.size(), CoreMatchers.is(1));
    assertThat("Unexpected suggestion.", suggestions.get(0).getText(), CoreMatchers.is(suggestion));
  }

  @ParameterizedTest(name = "Should suggest {1} entity names when the argument is {0}")
  @CsvSource({"ender, 5", "'', 106", "xyz, 0"})
  void shouldSuggestEntityNames(String input, int count)
      throws CommandSyntaxException, ExecutionException, InterruptedException {
    // Given.
    CommandNode<CommandSource> entityArgument = mobGriefingCommand.getChild("entity");

    // When.
    CompletableFuture<Suggestions> completableFuture = entityArgument
        .listSuggestions(commandContext, new SuggestionsBuilder(input, 0));

    // Then.
    List<Suggestion> suggestions = completableFuture.get().getList();
    assertThat("Unexpected suggestion size.", suggestions.size(), CoreMatchers.is(count));
  }

  @ParameterizedTest(name = "Should show the entity mobGriefing value when the argument is {0} and"
      + " value is {1}")
  @CsvSource({"test:entity1, FALSE", "test:entity2, TRUE", "test:entity3, INHERIT"})
  void shouldShowEntityMobGriefingWhenEntity(String entityName, MobGriefingValue value)
      throws CommandSyntaxException {
    // Given.
    EntityMobGriefingData data = EntityMobGriefingData.forServer(server);
    ResourceLocation entityId = new ResourceLocation(entityName);
    data.setMobGriefingValue(entityId, value);

    CommandNode<CommandSource> entityArgument = mobGriefingCommand.getChild("entity");
    Command<CommandSource> command = entityArgument.getCommand();

    commandArguments.put("entity", new ParsedArgument<>(0, 0, entityId));

    // When.
    int result = command.run(commandContext);

    // Then.
    assertThat("Unexpected command result.", result, is(value.ordinal()));
    verify(commandSource).sendFeedback(any(ITextComponent.class), eq(true));
  }

  @ParameterizedTest(name = "Should suggest {1} when the argument is {0}")
  @CsvSource({"Tru, true", "fALS, false", "INHER, inherit"})
  void shouldSuggestEntityMobGriefingValue(String input, String suggestion)
      throws CommandSyntaxException, ExecutionException, InterruptedException {
    // Given.
    CommandNode<CommandSource> entityArgument = mobGriefingCommand.getChild("entity");
    CommandNode<CommandSource> valueArgument = entityArgument.getChild("value");

    // When.
    CompletableFuture<Suggestions> completableFuture = valueArgument
        .listSuggestions(commandContext, new SuggestionsBuilder(input, 0));

    // Then.
    List<Suggestion> suggestions = completableFuture.get().getList();
    assertThat("Unexpected suggestion size.", suggestions.size(), CoreMatchers.is(1));
    assertThat("Unexpected suggestion.", suggestions.get(0).getText(), CoreMatchers.is(suggestion));
  }

  @ParameterizedTest(name = "Should suggest {1} values when the argument is {0}")
  @CsvSource({"'', 3", "x, 0"})
  void shouldSuggestEntityMobGriefingValues(String input, int count)
      throws CommandSyntaxException, ExecutionException, InterruptedException {
    // Given.
    CommandNode<CommandSource> entityArgument = mobGriefingCommand.getChild("entity");
    CommandNode<CommandSource> valueArgument = entityArgument.getChild("value");

    // When.
    CompletableFuture<Suggestions> completableFuture = valueArgument
        .listSuggestions(commandContext, new SuggestionsBuilder(input, 0));

    // Then.
    List<Suggestion> suggestions = completableFuture.get().getList();
    assertThat("Unexpected suggestion size.", suggestions.size(), CoreMatchers.is(count));
  }

  @ParameterizedTest(name = "Should set the entity mobGriefing value when the argument is {0} {1}")
  @CsvSource({"test:entity1, FALSE", "test:entity2, TRUE", "test:entity3, INHERIT"})
  void shouldSetEntityMobGriefingWhenEntityAndValue(String entityName, MobGriefingValue value)
      throws CommandSyntaxException {
    // Given.
    EntityMobGriefingData data = EntityMobGriefingData.forServer(server);
    ResourceLocation entityId = new ResourceLocation(entityName);
    data.setMobGriefingValue(entityId, MobGriefingValue.INHERIT);

    CommandNode<CommandSource> entityArgument = mobGriefingCommand.getChild("entity");
    CommandNode<CommandSource> valueArgument = entityArgument.getChild("value");
    Command<CommandSource> command = valueArgument.getCommand();

    commandArguments.put("entity", new ParsedArgument<>(0, 0, entityId));
    commandArguments.put("value", new ParsedArgument<>(0, 0, value));

    // When.
    int result = command.run(commandContext);

    // Then.
    assertThat("Unexpected command result.", result, is(value.ordinal()));
    assertThat("Unexpected mob griefing value.", data.getMobGriefingValue(entityId), is(value));
    verify(commandSource).sendFeedback(any(ITextComponent.class), eq(true));
  }
}
