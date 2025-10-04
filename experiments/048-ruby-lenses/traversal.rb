#!/usr/bin/env ruby
# frozen_string_literal: true

require_relative 'lens'

module Lenses
  # ============================================================================
  # TRAVERSAL: Multiple Foci (0 to N)
  # ============================================================================

  class Traversal
    attr_reader :to_list, :over_fn

    def initialize(to_list, over_fn)
      @to_list = to_list
      @over_fn = over_fn
    end

    # Extract all foci as array
    def view_all(target)
      @to_list.call(target)
    end
    alias to_a view_all

    # Map function over all foci
    def map(target, &block)
      @over_fn.call(target, block)
    end
    alias over map

    # Fold over foci
    def fold(target, init, &block)
      view_all(target).reduce(init, &block)
    end

    # Compose traversals
    def compose(other)
      Traversal.new(
        ->(target) {
          view_all(target).flat_map { |focus| other.view_all(focus) }
        },
        ->(target, fn) {
          map(target) { |focus| other.map(focus, &fn) }
        }
      )
    end
    alias >> compose
  end

  # ============================================================================
  # BUILT-IN TRAVERSALS
  # ============================================================================

  # Traversal over all array elements
  EACH = Traversal.new(
    ->(array) { array.to_a },
    ->(array, fn) { array.map(&fn) }
  )

  # Filtered traversal
  def self.filtered(predicate)
    Traversal.new(
      ->(array) { array.select(&predicate) },
      ->(array, fn) {
        array.map { |elem| predicate.call(elem) ? fn.call(elem) : elem }
      }
    )
  end

  # Hash values traversal
  HASH_VALUES = Traversal.new(
    ->(hash) { hash.values },
    ->(hash, fn) {
      hash.transform_values(&fn)
    }
  )

  # Hash entries traversal
  HASH_ENTRIES = Traversal.new(
    ->(hash) { hash.to_a },
    ->(hash, fn) {
      hash.transform_keys { |k| fn.call([k, hash[k]])[0] }
          .transform_values { |v| fn.call([hash.key(v), v])[1] }
    }
  )
end

# ============================================================================
# EXAMPLES
# ============================================================================

if __FILE__ == $PROGRAM_NAME
  include Lenses

  puts "╔═══════════════════════════════════════════════════════════════╗"
  puts "║         RUBY TRAVERSALS: Multiple Foci                      ║"
  puts "╚═══════════════════════════════════════════════════════════════╝"
  puts

  # ============================================================================
  # BASIC TRAVERSAL OPERATIONS
  # ============================================================================

  puts "═══ BASIC TRAVERSAL OPERATIONS ═══"
  puts

  arr = [1, 2, 3, 4, 5]

  puts "Array: #{arr}"
  puts

  puts "view_all (extract all foci):"
  puts "  EACH.view_all(arr) => #{EACH.view_all(arr)}"
  puts

  puts "map (transform all foci):"
  result1 = EACH.map(arr) { |x| x * 2 }
  puts "  EACH.map(arr) { |x| x * 2 } => #{result1}"

  result2 = EACH.map(arr, &:succ)
  puts "  EACH.map(arr, &:succ) => #{result2}"
  puts

  puts "fold (reduce over foci):"
  sum = EACH.fold(arr, 0) { |acc, x| acc + x }
  puts "  EACH.fold(arr, 0, &:+) => #{sum}"

  product = EACH.fold(arr, 1) { |acc, x| acc * x }
  puts "  EACH.fold(arr, 1, &:*) => #{product}"
  puts

  # ============================================================================
  # FILTERED TRAVERSAL
  # ============================================================================

  puts "═══ FILTERED TRAVERSAL ═══"
  puts

  evens = Lenses.filtered(&:even?)

  puts "Filtered (even numbers only):"
  puts "  Array: #{arr}"
  puts "  view_all: #{evens.view_all(arr)}"

  result = evens.map(arr) { |x| x * 10 }
  puts "  map { |x| x * 10 }: #{result}"
  puts "  (only even numbers transformed)"
  puts

  scores = [55, 85, 65, 90, 70, 45, 95]
  failing = Lenses.filtered { |s| s < 70 }

  puts "Boost failing scores (<70):"
  puts "  Scores: #{scores}"

  boosted = failing.map(scores) { |s| s + 10 }
  puts "  Boosted: #{boosted}"
  puts

  # ============================================================================
  # TRAVERSAL COMPOSITION
  # ============================================================================

  puts "═══ TRAVERSAL COMPOSITION ═══"
  puts

  nested = [[1, 2], [3, 4], [5, 6]]

  puts "Nested array: #{nested}"
  puts

  all_elements = EACH.compose(EACH)
  # Or: EACH >> EACH

  puts "Composed traversal (outer >> inner):"
  puts "  view_all: #{all_elements.view_all(nested)}"

  doubled = all_elements.map(nested) { |x| x * 10 }
  puts "  map { |x| x * 10 }: #{doubled}"
  puts

  # ============================================================================
  # LENS + TRAVERSAL COMPOSITION
  # ============================================================================

  puts "═══ LENS + TRAVERSAL COMPOSITION ═══"
  puts

  Student = Struct.new(:name, :scores) do
    def to_s
      "#{name}: #{scores}"
    end
  end

  students = [
    Student.new("Alice", [85, 90, 92]),
    Student.new("Bob", [78, 88, 95]),
    Student.new("Charlie", [90, 85, 88])
  ]

  puts "Students:"
  students.each { |s| puts "  #{s}" }
  puts

  scores_lens = Lenses.struct_lens(Student, :scores)

  # Traversal: students -> scores array -> each score
  all_scores = EACH
    .compose(scores_lens.to_traversal)
    .compose(EACH)

  puts "All scores (via composed traversal):"
  puts "  #{all_scores.view_all(students)}"
  puts

  puts "Add 5 bonus points to all scores:"
  updated = all_scores.map(students) { |s| s + 5 }
  updated.each { |s| puts "  #{s}" }
  puts

  # Helper to convert lens to traversal
  class Lens
    def to_traversal
      Traversal.new(
        ->(target) { [view(target)] },
        ->(target, fn) { set(target, fn.call(view(target))) }
      )
    end
  end

  # ============================================================================
  # HASH TRAVERSALS
  # ============================================================================

  puts "═══ HASH TRAVERSALS ═══"
  puts

  config = { debug: true, port: 8080, timeout: 30 }

  puts "Hash: #{config}"
  puts

  puts "HASH_VALUES traversal:"
  puts "  view_all: #{HASH_VALUES.view_all(config)}"

  updated_config = HASH_VALUES.map(config) { |v| v.is_a?(Integer) ? v * 2 : v }
  puts "  Double integers: #{updated_config}"
  puts

  # ============================================================================
  # PRACTICAL EXAMPLES
  # ============================================================================

  puts "═══ PRACTICAL EXAMPLES ═══"
  puts

  puts "Example 1: Normalize scores"
  raw_scores = [43, 47, 50, 48, 45]
  max_score = 50

  puts "  Raw (out of #{max_score}): #{raw_scores}"

  percentages = EACH.map(raw_scores) { |s| (s.to_f / max_score * 100).round }
  puts "  Percentages: #{percentages}"
  puts

  puts "Example 2: Matrix operations"
  matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

  puts "  Matrix: #{matrix}"

  doubled_matrix = (EACH >> EACH).map(matrix) { |x| x * 2 }
  puts "  Doubled: #{doubled_matrix}"

  sum = (EACH >> EACH).fold(matrix, 0, &:+)
  puts "  Sum of all: #{sum}"
  puts

  puts "Example 3: Deep config update"
  deep_config = {
    database: { host: "localhost", port: 5432 },
    cache: { host: "localhost", port: 6379 }
  }

  puts "  Config: #{deep_config}"

  # Update all port values
  port_values = HASH_VALUES >> Lenses.key(:port).to_traversal
  updated_deep = port_values.map(deep_config) { |p| p + 1000 }

  puts "  Increment all ports by 1000: #{updated_deep}"
  puts

  # ============================================================================
  # TRAVERSALS vs LENSES
  # ============================================================================

  puts "═══ TRAVERSALS vs LENSES ═══"
  puts

  puts "┌──────────────┬─────────────┬─────────────────┐"
  puts "│ Feature      │ Lens        │ Traversal       │"
  puts "├──────────────┼─────────────┼─────────────────┤"
  puts "│ # of foci    │ Exactly 1   │ 0 to N          │"
  puts "│ Get op       │ view        │ view_all        │"
  puts "│ Returns      │ Single val  │ Array           │"
  puts "│ Use case     │ Field       │ Collection      │"
  puts "│ Composition  │ lens >> lens│ trav >> trav    │"
  puts "└──────────────┴─────────────┴─────────────────┘"
  puts

  puts "RELATIONSHIP:"
  puts "  • Lens can be converted to Traversal (1 focus)"
  puts "  • Lens >> Traversal = Traversal"
  puts "  • Traversal >> Lens = Traversal"
  puts "  • Traversal >> Traversal = Traversal"
  puts

  # ============================================================================
  # SUMMARY
  # ============================================================================

  puts "╔═══════════════════════════════════════════════════════════════╗"
  puts "║                          SUMMARY                             ║"
  puts "╚═══════════════════════════════════════════════════════════════╝"
  puts

  puts "TRAVERSAL OPERATIONS:"
  puts "  • view_all:    extract all foci"
  puts "  • map/over:    transform all foci"
  puts "  • fold:        reduce over foci"
  puts "  • compose/>>:  combine traversals"
  puts

  puts "BUILT-IN TRAVERSALS:"
  puts "  • EACH:          all array elements"
  puts "  • filtered:      matching elements"
  puts "  • HASH_VALUES:   hash values"
  puts

  puts "KEY FEATURES:"
  puts "  • Multiple foci (0 to N)"
  puts "  • Composable with lenses"
  puts "  • Immutable updates"
  puts "  • Functional collection ops"
  puts

  puts "═══════════════════════════════════════════════════════════════"
  puts "Ruby version: #{RUBY_VERSION}"
  puts "RacketCon 2025 - Experiment 048: Ruby Traversals"
  puts "═══════════════════════════════════════════════════════════════"
end
