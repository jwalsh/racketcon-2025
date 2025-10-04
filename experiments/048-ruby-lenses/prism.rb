#!/usr/bin/env ruby
# frozen_string_literal: true

require_relative 'lens'

module Lenses
  # ============================================================================
  # PRISM: Partial/Optional Optic (0 or 1 focus)
  # ============================================================================

  class Prism
    # Special value for absent focus
    ABSENT = Object.new.freeze

    attr_reader :project, :inject

    def initialize(project, inject)
      @project = project
      @inject = inject
    end

    # Project: Try to extract focus (returns ABSENT if fails)
    def view(target, default = ABSENT)
      result = @project.call(target)
      result == ABSENT ? default : result
    end
    alias project_from view

    # Inject: Convert focus to target
    def review(focus)
      @inject.call(focus)
    end
    alias inject_into review

    # Compose prisms
    def compose(other)
      Prism.new(
        ->(target) {
          intermediate = @project.call(target)
          intermediate == ABSENT ? ABSENT : other.project.call(intermediate)
        },
        ->(focus) {
          intermediate = other.inject.call(focus)
          @inject.call(intermediate)
        }
      )
    end
    alias >> compose

    # Convert to traversal (0 or 1 element)
    def to_traversal
      Traversal.new(
        ->(target) {
          result = @project.call(target)
          result == ABSENT ? [] : [result]
        },
        ->(target, fn) {
          result = @project.call(target)
          if result == ABSENT
            target
          else
            new_focus = fn.call(result)
            @inject.call(new_focus)
          end
        }
      )
    end
  end

  # ============================================================================
  # BUILT-IN PRISMS
  # ============================================================================

  # String <-> Integer prism
  STRING_INTEGER = Prism.new(
    ->(str) {
      Integer(str)
    rescue ArgumentError, TypeError
      Prism::ABSENT
    },
    ->(int) { int.to_s }
  )

  # Guard prism (predicate-based)
  def self.guard(predicate)
    Prism.new(
      ->(value) { predicate.call(value) ? value : Prism::ABSENT },
      ->(value) { value }
    )
  end

  # Optional/Maybe prism (nil handling)
  SOME = Prism.new(
    ->(value) { value.nil? ? Prism::ABSENT : value },
    ->(value) { value }
  )

  # Array head prism (non-empty arrays)
  HEAD = Prism.new(
    ->(array) { array.empty? ? Prism::ABSENT : array.first },
    ->(value) { [value] }
  )

  # Hash key prism (may not exist)
  def self.has_key?(key)
    Prism.new(
      ->(hash) { hash.key?(key) ? hash[key] : Prism::ABSENT },
      ->(value) { { key => value } }
    )
  end

  # Type prism (class checking)
  def self.is_a?(klass)
    Prism.new(
      ->(value) { value.is_a?(klass) ? value : Prism::ABSENT },
      ->(value) { value }
    )
  end

  # ============================================================================
  # TRAVERSAL (for composition with prisms)
  # ============================================================================

  class Traversal
    attr_reader :to_list, :over

    def initialize(to_list, over)
      @to_list = to_list
      @over = over
    end

    def view_all(target)
      @to_list.call(target)
    end

    def map(target, &block)
      @over.call(target, block)
    end
  end
end

# ============================================================================
# EXAMPLES
# ============================================================================

if __FILE__ == $PROGRAM_NAME
  include Lenses

  puts "╔═══════════════════════════════════════════════════════════════╗"
  puts "║              RUBY PRISMS: Partial Optics                     ║"
  puts "╚═══════════════════════════════════════════════════════════════╝"
  puts

  # ============================================================================
  # STRING-INTEGER PRISM
  # ============================================================================

  puts "═══ STRING-INTEGER PRISM ═══"
  puts

  puts "Project (string -> integer):"
  puts "  view('42') => #{STRING_INTEGER.view('42')}"
  puts "  view('123') => #{STRING_INTEGER.view('123')}"
  puts

  puts "Handles failure:"
  puts "  view('hello') => #{STRING_INTEGER.view('hello')}"
  puts "  view('hello', 0) => #{STRING_INTEGER.view('hello', 0)}"
  puts

  puts "Inject (integer -> string):"
  puts "  review(42) => #{STRING_INTEGER.review(42).inspect}"
  puts "  review(123) => #{STRING_INTEGER.review(123).inspect}"
  puts

  # ============================================================================
  # PRISM LAWS
  # ============================================================================

  puts "═══ PRISM LAWS ═══"
  puts

  puts "Law 1: PROJECT-INJECT"
  puts "  prism.view(prism.review(focus)) == focus"
  test_int = 42
  injected = STRING_INTEGER.review(test_int)
  projected = STRING_INTEGER.view(injected)
  puts "  #{test_int} -> inject -> #{injected.inspect} -> project -> #{projected}"
  puts "  Equal? #{test_int == projected} ✓"
  puts

  puts "Law 2: INJECT-PROJECT (when focus exists)"
  puts "  prism.review(prism.view(target)) == target"
  test_str = "99"
  num = STRING_INTEGER.view(test_str)
  back = STRING_INTEGER.review(num)
  puts "  #{test_str.inspect} -> project -> #{num} -> inject -> #{back.inspect}"
  puts "  Equal? #{test_str == back} ✓"
  puts

  # ============================================================================
  # GUARD PRISM
  # ============================================================================

  puts "═══ GUARD PRISM ═══"
  puts

  even_prism = Lenses.guard(->(n) { n.even? })
  positive_prism = Lenses.guard(->(n) { n.positive? })

  puts "even_prism:"
  puts "  view(10) => #{even_prism.view(10)}"
  puts "  view(7) => #{even_prism.view(7)}"
  puts "  view(7, nil) => #{even_prism.view(7, nil).inspect}"
  puts

  puts "positive_prism:"
  puts "  view(5) => #{positive_prism.view(5)}"
  puts "  view(-3) => #{positive_prism.view(-3)}"
  puts

  # ============================================================================
  # CUSTOM PRISMS
  # ============================================================================

  puts "═══ CUSTOM PRISMS ═══"
  puts

  # Safe division (non-zero denominator)
  safe_reciprocal = Prism.new(
    ->(n) { n.zero? ? Prism::ABSENT : 1.0 / n },
    ->(r) { 1.0 / r }
  )

  puts "safe_reciprocal prism:"
  puts "  view(4) => #{safe_reciprocal.view(4)}"
  puts "  view(0) => #{safe_reciprocal.view(0)}"
  puts "  review(0.25) => #{safe_reciprocal.review(0.25)}"
  puts

  # Email validation prism
  email_prism = Prism.new(
    ->(str) {
      str.match?(/\A[\w+\-.]+@[a-z\d\-]+(\.[a-z\d\-]+)*\.[a-z]+\z/i) ? str : Prism::ABSENT
    },
    ->(email) { email }
  )

  puts "email_prism:"
  puts "  view('user@example.com') => #{email_prism.view('user@example.com').inspect}"
  puts "  view('invalid') => #{email_prism.view('invalid')}"
  puts

  # ============================================================================
  # PRISM COMPOSITION
  # ============================================================================

  puts "═══ PRISM COMPOSITION ═══"
  puts

  # Compose: symbol -> string -> integer
  symbol_string = Prism.new(
    ->(sym) { sym.is_a?(Symbol) ? sym.to_s : Prism::ABSENT },
    ->(str) { str.to_sym }
  )

  symbol_integer = symbol_string.compose(STRING_INTEGER)

  puts "symbol_string >> STRING_INTEGER:"
  puts "  view(:'42') => #{symbol_integer.view(:'42')}"
  puts "  view(:hello) => #{symbol_integer.view(:hello)}"
  puts "  review(99) => #{symbol_integer.review(99).inspect}"
  puts

  # ============================================================================
  # PRISM AS TRAVERSAL
  # ============================================================================

  puts "═══ PRISM AS TRAVERSAL ═══"
  puts

  trav = STRING_INTEGER.to_traversal

  puts "STRING_INTEGER as traversal:"
  puts "  view_all('42') => #{trav.view_all('42')}"
  puts "  view_all('bad') => #{trav.view_all('bad')}"
  puts

  puts "Map over traversal:"
  result1 = trav.map('5') { |n| n * 2 }
  result2 = trav.map('bad') { |n| n * 2 }
  puts "  map('5') { |n| n * 2 } => #{result1.inspect}"
  puts "  map('bad') { |n| n * 2 } => #{result2.inspect}"
  puts

  # ============================================================================
  # PRACTICAL EXAMPLES
  # ============================================================================

  puts "═══ PRACTICAL EXAMPLES ═══"
  puts

  puts "Example 1: Safe parsing with fallback"
  def parse_int_or(str, fallback)
    result = STRING_INTEGER.view(str)
    result == Prism::ABSENT ? fallback : result
  end

  puts "  parse_int_or('42', 0) => #{parse_int_or('42', 0)}"
  puts "  parse_int_or('bad', 0) => #{parse_int_or('bad', 0)}"
  puts

  puts "Example 2: Validation pipeline"
  def validate_positive_number(str)
    num = STRING_INTEGER.view(str)
    return "not a number" if num == Prism::ABSENT

    positive = positive_prism.view(num)
    return "not positive" if positive == Prism::ABSENT

    num
  end

  puts "  validate_positive_number('42') => #{validate_positive_number('42')}"
  puts "  validate_positive_number('-5') => #{validate_positive_number('-5')}"
  puts "  validate_positive_number('xyz') => #{validate_positive_number('xyz')}"
  puts

  puts "Example 3: Optional field access"
  Config = Struct.new(:name, :port, :ssl) do
    def to_s
      "Config(#{name}, #{port}, ssl=#{ssl})"
    end
  end

  ssl_enabled = Lenses.guard(->(cfg) { cfg.ssl == true })

  cfg1 = Config.new("server1", 8080, true)
  cfg2 = Config.new("server2", 3000, false)

  puts "  Config 1: #{cfg1}"
  puts "  Config 2: #{cfg2}"
  puts "  SSL-enabled prism on cfg1: #{ssl_enabled.view(cfg1)}"
  puts "  SSL-enabled prism on cfg2: #{ssl_enabled.view(cfg2)}"
  puts

  # ============================================================================
  # TYPE PRISMS
  # ============================================================================

  puts "═══ TYPE PRISMS ═══"
  puts

  int_prism = Lenses.is_a?(Integer)
  string_prism = Lenses.is_a?(String)

  values = [42, "hello", 3.14, "world", 100]

  puts "Filter by type using prisms:"
  puts "  Values: #{values}"
  puts

  integers = values.map { |v| int_prism.view(v) }.compact
  strings = values.map { |v| string_prism.view(v) }.compact

  puts "  Integers: #{integers}"
  puts "  Strings: #{strings}"
  puts

  # ============================================================================
  # SUMMARY
  # ============================================================================

  puts "╔═══════════════════════════════════════════════════════════════╗"
  puts "║                          SUMMARY                             ║"
  puts "╚═══════════════════════════════════════════════════════════════╝"
  puts

  puts "PRISM OPERATIONS:"
  puts "  • view/project:    try to extract (may fail)"
  puts "  • review/inject:   convert focus to target"
  puts "  • compose/>>:      combine prisms"
  puts "  • to_traversal:    convert to traversal"
  puts

  puts "BUILT-IN PRISMS:"
  puts "  • STRING_INTEGER:  string <-> integer"
  puts "  • guard(pred):     predicate-based"
  puts "  • SOME:            nil handling"
  puts "  • HEAD:            array first element"
  puts "  • is_a?(class):    type checking"
  puts

  puts "USE CASES:"
  puts "  • Optional fields"
  puts "  • Type conversions"
  puts "  • Validation"
  puts "  • Pattern matching"
  puts

  puts "═══════════════════════════════════════════════════════════════"
  puts "Ruby version: #{RUBY_VERSION}"
  puts "RacketCon 2025 - Experiment 048: Ruby Prisms"
  puts "═══════════════════════════════════════════════════════════════"
end
