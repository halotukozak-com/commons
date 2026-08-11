package commons

class MetaContainsOnlyTest extends munit.FunSuite:

  trait Meta

  class A1 extends annotation.RefiningAnnotation
  class A2 extends annotation.RefiningAnnotation

  test("Loop: (Meta @A1) *: EmptyTuple containsOnly Meta") {
    summon[containsOnly.Loop[(Meta @A1) *: EmptyTuple, Meta] =:= true]
  }

  test("Loop: (Meta @A1, Meta @A2) containsOnly Meta") {
    summon[containsOnly.Loop[(Meta @A1) *: (Meta @A2) *: EmptyTuple, Meta] =:= true]
  }

  test("given: evidence summons for (Meta @A1, Meta @A2)") {
    summon[((Meta @A1) *: (Meta @A2) *: EmptyTuple) containsOnly Meta]
  }
