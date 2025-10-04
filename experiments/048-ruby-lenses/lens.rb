#!/usr/bin/env ruby
# frozen_string_literal: true

# Ruby Lenses: Core Implementation
# Ported from Haskell lens and Racket ocular-patdown

module Lenses
  # ============================================================================
  # LENS: First-class getter and setter
  # ============================================================================

  class Lens
    attr_reader :getter, :setter

    def initialize(getter, setter)
      @getter = getter
      @setter = setter
    end

    # View: Extract focus from target
    def view(target)
      @getter.call(target)
    end
    alias get view

    # Set: Update focus in target
    def set(target, new_value)
      @setter.call(target, new_value)
    end

    # Over/Modify: Transform focus using function
    def over(target, &block)
      old_value = view(target)
      new_value = block.call(old_value)
      set(target, new_value)
    end
    alias modify over

    # Compose lenses
    def compose(other)
      Lens.new(
        ->(target) { other.view(view(target)) },
        ->(target, value) {
          intermediate = view(target)
          new_intermediate = other.set(intermediate, value)
          set(target, new_intermediate)
        }
      )
    end
    alias >> compose

    # Operator-style access
    def [](target)
      view(target)
    end

    def []=(target, value)
      set(target, value)
    end
  end

  # ============================================================================
  # IMMUTABLE STRUCT SUPPORT
  # ============================================================================

  # Helper to create lenses for Struct fields
  def self.struct_lens(struct_class, field)
    Lens.new(
      ->(target) { target.send(field) },
      ->(target, value) {
        # Create new struct with updated field
        values = struct_class.members.map do |member|
          member == field ? value : target.send(member)
        end
        struct_class.new(*values)
      }
    )
  end

  # ============================================================================
  # BUILT-IN LENSES
  # ============================================================================

  # Identity lens (focuses on entire target)
  IDENTITY = Lens.new(
    ->(target) { target },
    ->(target, value) { value }
  )

  # Array index lens
  def self.at(index)
    Lens.new(
      ->(array) { array[index] },
      ->(array, value) {
        new_array = array.dup
        new_array[index] = value
        new_array
      }
    )
  end

  # Hash key lens
  def self.key(k)
    Lens.new(
      ->(hash) { hash[k] },
      ->(hash, value) { hash.merge(k => value) }
    )
  end

  # First element of array
  FIRST = at(0)

  # Last element of array
  def self.last_lens
    Lens.new(
      ->(array) { array.last },
      ->(array, value) {
        new_array = array.dup
        new_array[-1] = value
        new_array
      }
    )
  end
  LAST = last_lens
end

# ============================================================================
# EXAMPLES
# ============================================================================

if __FILE__ == $PROGRAM_NAME
  include Lenses

  puts "╔═══════════════════════════════════════════════════════════════╗"
  puts "║              RUBY LENSES: Core Implementation                ║"
  puts "╚═══════════════════════════════════════════════════════════════╝"
  puts

  # ============================================================================
  # BASIC LENS OPERATIONS
  # ============================================================================

  puts "═══ BASIC LENS OPERATIONS ═══"
  puts

  Point = Struct.new(:x, :y) do
    def to_s
      "(#{x}, #{y})"
    end
  end

  x_lens = Lenses.struct_lens(Point, :x)
  y_lens = Lenses.struct_lens(Point, :y)

  p = Point.new(10, 20)

  puts "Point: #{p}"
  puts

  puts "lens.view (get value):"
  puts "  x_lens.view(p) => #{x_lens.view(p)}"
  puts "  y_lens.view(p) => #{y_lens.view(p)}"
  puts

  puts "lens.set (update value):"
  p2 = x_lens.set(p, 99)
  puts "  x_lens.set(p, 99) => #{p2}"
  puts "  Original unchanged: #{p}"
  puts

  puts "lens.over (transform value):"
  p3 = x_lens.over(p) { |x| x * 2 }
  puts "  x_lens.over(p) { |x| x * 2 } => #{p3}"
  puts

  # ============================================================================
  # LENS COMPOSITION
  # ============================================================================

  puts "═══ LENS COMPOSITION ═══"
  puts

  Rect = Struct.new(:top_left, :width, :height) do
    def to_s
      "Rect(#{top_left}, #{width}x#{height})"
    end
  end

  top_left_lens = Lenses.struct_lens(Rect, :top_left)
  width_lens = Lenses.struct_lens(Rect, :width)

  # Compose: rect -> point -> x
  rect_x_lens = top_left_lens.compose(x_lens)
  # Or using >> operator:
  # rect_x_lens = top_left_lens >> x_lens

  r = Rect.new(Point.new(5, 10), 50, 30)

  puts "Rect: #{r}"
  puts

  puts "Composed lens (rect -> point -> x):"
  puts "  rect_x_lens.view(r) => #{rect_x_lens.view(r)}"

  r2 = rect_x_lens.set(r, 100)
  puts "  rect_x_lens.set(r, 100) => #{r2}"
  puts

  # ============================================================================
  # ARRAY AND HASH LENSES
  # ============================================================================

  puts "═══ ARRAY AND HASH LENSES ═══"
  puts

  arr = [10, 20, 30]

  puts "Array: #{arr}"
  puts

  first_lens = Lenses::FIRST
  puts "First element:"
  puts "  FIRST.view(arr) => #{first_lens.view(arr)}"
  puts "  FIRST.set(arr, 99) => #{first_lens.set(arr, 99)}"
  puts

  hash = { name: "Alice", age: 30 }

  puts "Hash: #{hash}"
  puts

  name_lens = Lenses.key(:name)
  age_lens = Lenses.key(:age)

  puts "Hash key lenses:"
  puts "  key(:name).view(hash) => #{name_lens.view(hash)}"
  puts "  key(:age).set(hash, 31) => #{age_lens.set(hash, 31)}"
  puts

  # ============================================================================
  # LENS LAWS VERIFICATION
  # ============================================================================

  puts "═══ LENS LAWS VERIFICATION ═══"
  puts

  puts "Law 1: GET-PUT"
  puts "  lens.set(target, lens.view(target)) == target"
  law1_result = x_lens.set(p, x_lens.view(p))
  puts "  Result: #{law1_result}"
  puts "  Equal? #{law1_result == p} ✓"
  puts

  puts "Law 2: PUT-GET"
  puts "  lens.view(lens.set(target, value)) == value"
  p_updated = x_lens.set(p, 99)
  law2_result = x_lens.view(p_updated)
  puts "  Result: #{law2_result}"
  puts "  Equal to 99? #{law2_result == 99} ✓"
  puts

  puts "Law 3: PUT-PUT"
  puts "  lens.set(lens.set(target, v1), v2) == lens.set(target, v2)"
  law3_left = x_lens.set(x_lens.set(p, 10), 20)
  law3_right = x_lens.set(p, 20)
  puts "  Left:  #{law3_left}"
  puts "  Right: #{law3_right}"
  puts "  Equal? #{law3_left == law3_right} ✓"
  puts

  # ============================================================================
  # PRACTICAL EXAMPLES
  # ============================================================================

  puts "═══ PRACTICAL EXAMPLES ═══"
  puts

  Config = Struct.new(:debug, :port, :log_level) do
    def to_s
      "Config(debug=#{debug}, port=#{port}, log_level=#{log_level})"
    end
  end

  port_lens = Lenses.struct_lens(Config, :port)
  debug_lens = Lenses.struct_lens(Config, :debug)

  config = Config.new(true, 8080, :info)

  puts "Example 1: Config management"
  puts "  Original: #{config}"

  config2 = port_lens.set(config, 3000)
  puts "  Change port: #{config2}"
  puts

  puts "Example 2: Chain updates"
  config3 = debug_lens.set(
    port_lens.set(config, 9000),
    false
  )
  puts "  Disable debug and change port: #{config3}"
  puts

  # ============================================================================
  # OPERATOR STYLE
  # ============================================================================

  puts "═══ OPERATOR STYLE (experimental) ═══"
  puts

  puts "Using [] operator for view:"
  puts "  x_lens[p] => #{x_lens[p]}"
  puts

  # Note: []= doesn't work for immutable updates in Ruby
  # It would need a different approach

  # ============================================================================
  # SUMMARY
  # ============================================================================

  puts "╔═══════════════════════════════════════════════════════════════╗"
  puts "║                          SUMMARY                             ║"
  puts "╚═══════════════════════════════════════════════════════════════╝"
  puts

  puts "LENS OPERATIONS:"
  puts "  • view/get:    extract focus"
  puts "  • set:         update focus (immutable)"
  puts "  • over/modify: transform focus"
  puts "  • compose/>>:  combine lenses"
  puts

  puts "BUILT-IN LENSES:"
  puts "  • struct_lens: for Struct fields"
  puts "  • at(index):   array element"
  puts "  • key(k):      hash key"
  puts "  • FIRST, LAST: array ends"
  puts

  puts "KEY FEATURES:"
  puts "  • Immutable updates (copy-on-write)"
  puts "  • Composable paths"
  puts "  • Follows lens laws"
  puts "  • Ruby-idiomatic API"
  puts

  puts "═══════════════════════════════════════════════════════════════"
  puts "Ruby version: #{RUBY_VERSION}"
  puts "RacketCon 2025 - Experiment 048: Ruby Lenses"
  puts "═══════════════════════════════════════════════════════════════"
end
