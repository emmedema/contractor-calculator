package com.nymeria.calc

import org.scalatest.{Matchers, FlatSpec}

class CalculatorTest extends FlatSpec with Matchers {

  behavior of "The calculator"

  // these test cases test for wrong input
  it should "work correctly with zero income" in {
    val salary = 0
    val dividends = 0

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(salary,dividends)

    salaryTax shouldBe 0
    dividendsTax shouldBe 0
  }

  it should "work correctly with negative salary and ok dividends" in {
    val salary = 0
    val dividends = 0

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(salary,dividends)

    fail("not yet implemented")
  }

  it should "work correctly with negative dividends and ok salary" in {
    val salary = 0
    val dividends = 0

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(salary,dividends)

    fail("not yet implemented")
  }

  // The following examples are from http://www.itcontracting.com/new-dividend-tax-april-2016/
  it should "work correctly with Example 1" in {
    val salary = 11000
    val dividends = 50000

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(salary,dividends)

    salaryTax shouldBe 0
    dividendsTax shouldBe 7875.00
  }

  it should "work correctly with Example 2" in {
    val salary = 8060
    val dividends = 80000

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(salary,dividends)

    salaryTax shouldBe 0
    dividendsTax shouldBe 16669.50
  }

  it should "work correctly with Example 3" in {
    val salary = 8060
    val dividends = 100000

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(salary,dividends)

    salaryTax shouldBe 218.00
    dividendsTax shouldBe 24397.50
  }

  // The following test cases test only the salary calculations behaviour
  it should "work correctly with no dividends and salary of 20000" in {
    val salary = 20000

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(salary,0)

    salaryTax shouldBe 1800.00
    dividendsTax shouldBe 0
  }

  it should "work correctly with no dividends and salary of 50000" in {
    val salary = 50000

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(salary,0)

    salaryTax shouldBe 9200.00
    dividendsTax shouldBe 0
  }

  it should "work correctly with no dividends and salary of 120000" in {
    val salary = 120000

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(salary,0)

    salaryTax shouldBe 41200.00
    dividendsTax shouldBe 0
  }

  it should "work correctly with no dividends and salary of 200000" in {
    val salary = 200000

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(salary,0)

    salaryTax shouldBe 76100.00
    dividendsTax shouldBe 0
  }

  // The following test cases test the calculations behaviour of high dividends and zero salary
  it should "work correctly with no salary and dividends of 20000" in {
    val dividends = 20000

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(0,dividends)

    salaryTax shouldBe 0
    dividendsTax shouldBe 300.00
  }

  it should "work correctly with no salary and dividends of 50000" in {
    val dividends = 50000

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(0,dividends)

    salaryTax shouldBe 0
    dividendsTax shouldBe 4300.00
  }

  it should "work correctly with no salary and dividends of 80000" in {
    val dividends = 80000

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(0,dividends)

    salaryTax shouldBe 0
    dividendsTax shouldBe 14050.00
  }

  it should "work correctly with no salary and dividends of 120000" in {
    val dividends = 120000

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(0,dividends)

    salaryTax shouldBe 0
    dividendsTax shouldBe 30300.00
  }

  it should "work correctly with no salary and dividends of 210000" in {
    val dividends = 210000

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(0,dividends)

    salaryTax shouldBe 0
    dividendsTax shouldBe 63235.00
  }

  // The following test cases test the calculations behaviour of both high salary and high dividends
  it should "work correctly with salary of 100000 and dividends of 100000" in {
    val salary = 100000
    val dividends = 100000

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(salary,dividends)

    salaryTax shouldBe 33600.00
    dividendsTax shouldBe 33675.00
  }

  it should "work correctly with salary of 11000 and dividends of 90000" in {
    val salary = 11000
    val dividends = 90000

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(salary,dividends)

    salaryTax shouldBe 100.00
    dividendsTax shouldBe 21000.00
  }

  it should "work correctly with salary of 11000 and dividends of 200000" in {
    val salary = 11000
    val dividends = 200000

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(salary,dividends)

    salaryTax shouldBe 2200.00
    dividendsTax shouldBe 62791.00
  }

  it should "work correctly with salary of 50000 and dividends of 130000" in {
    val salary = 50000
    val dividends = 130000

    val (salaryTax, dividendsTax) = Calculator.calculateTotalTax(salary,dividends)

    salaryTax shouldBe 13600.00
    dividendsTax shouldBe 42305.00
  }

}
