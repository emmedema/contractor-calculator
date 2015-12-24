package com.nymeria.calc

import scala.math.BigDecimal.RoundingMode

object Calculator {

  val basicRateThreshold: BigDecimal = 32000
  val higherRateThreshold: BigDecimal = 150000
  val effectiveHigherRateThreshold: BigDecimal = higherRateThreshold - basicRateThreshold
  val personalAllowance: BigDecimal = 11000

  def calculateTotalTax(salaryIncome: BigDecimal, dividendsIncome: BigDecimal): Option[(BigDecimal, BigDecimal)] = {
    if(salaryIncome < 0 || dividendsIncome < 0) return None

    val generalAllowance = effectiveAllowance(salaryIncome + dividendsIncome)

    val (sBasic, sHigher, sTop, remainingAllowance) = Salary.calculateTax(salaryIncome, generalAllowance)
    val salaryTaxableAmounts = Salary.calculateTaxableIncomes(salaryIncome, generalAllowance)
    val (dBasic, dHigher, dTop) = Dividends.calculateTax(dividendsIncome, remainingAllowance + Dividends.dividendsAllowance, salaryTaxableAmounts)

    Some(((sBasic + sHigher + sTop).setScale(2, RoundingMode.HALF_UP), (dBasic + dHigher + dTop).setScale(2, RoundingMode.HALF_UP)))
  }

  private[Calculator] def effectiveAllowance(totalIncome: BigDecimal): BigDecimal = {
    val beyondThreshold = totalIncome - 100000

    if (beyondThreshold > 0) {
      val result = personalAllowance - beyondThreshold / 2
      if (result < 0) 0 else result
    } else { personalAllowance }
  }

  private[Calculator] object Salary {
    val basicRate: BigDecimal = 0.20
    val higherRate: BigDecimal = 0.40
    val topRate: BigDecimal = 0.45

    // Returns a 4-tuple of (tax at basic rate, tax at higher rate, tax at top rate, remaining allowance)
    def calculateTax(salaryIncome: BigDecimal, allowance: BigDecimal): (BigDecimal, BigDecimal, BigDecimal, BigDecimal) = {
      val (basic, higher, top) = calculateTaxableIncomes(salaryIncome, allowance)

      (
        basic * basicRate,
        higher * higherRate,
        top * topRate,
        if (allowance > salaryIncome) allowance - salaryIncome else 0
        )
    }

    def calculateTaxableIncomes(salaryIncome: BigDecimal, allowance: BigDecimal): (BigDecimal, BigDecimal, BigDecimal) = {
      if(salaryIncome <= 0) return (0,0,0)

      val (basic, moreThanBasic) = basicTaxableIncome(salaryIncome, allowance)
      val (higher, top) = higherTaxableIncome(moreThanBasic)

      (basic, higher, top)
    }

    // Returns a tuple of (income taxed at this level, income still to ba calculated)
    def basicTaxableIncome(salaryIncome: BigDecimal, remainingAllowance: BigDecimal): (BigDecimal, BigDecimal) = {
      val taxableIncome = salaryIncome - remainingAllowance

      if (taxableIncome < 0)
        (0, 0)
      else {
        if (taxableIncome <= basicRateThreshold) {
          (taxableIncome, 0)
        } else {
          (basicRateThreshold, taxableIncome - basicRateThreshold)
        }
      }
    }

    // Returns a tuple of (income taxed at this level, income still to ba calculated)
    def higherTaxableIncome(remainingIncome: BigDecimal): (BigDecimal, BigDecimal) = {
      if (remainingIncome <= effectiveHigherRateThreshold) {
        (remainingIncome, 0)
      } else {
        (effectiveHigherRateThreshold, remainingIncome - effectiveHigherRateThreshold)
      }
    }
  }

  private[Calculator] object Dividends {
    val dividendsAllowance: BigDecimal = 5000
    val basicRate: BigDecimal = 0.075
    val higherRate: BigDecimal = 0.325
    val topRate: BigDecimal = 0.381

    // Returns a 4-tuple of (tax at basic rate, tax at higher rate, tax at top rate, remaining allowance)
    def calculateTax(dividendsIncome: BigDecimal, allowance: BigDecimal, salaryTaxableAmounts: (BigDecimal, BigDecimal, BigDecimal)) : (BigDecimal, BigDecimal, BigDecimal)  = {
      val (basic, higher, top) = calculateTaxableIncomes(dividendsIncome, allowance, salaryTaxableAmounts)

      (
        basic * basicRate,
        higher * higherRate,
        top * topRate
        )
    }

    def calculateTaxableIncomes(dividendsIncome: BigDecimal, allowance: BigDecimal, salaryTaxableAmounts: (BigDecimal, BigDecimal, BigDecimal)): (BigDecimal, BigDecimal, BigDecimal) = {
      if(dividendsIncome <= 0) return (0,0,0)

      val (sBasicQty, sHigherQty, sTopQty) = salaryTaxableAmounts

      if (sTopQty > 0) {
        (0, 0, dividendsIncome)
      } else {
        if (sHigherQty > 0) {
          val remainingIncome = dividendsIncome - allowance
          val remainingHigherRateThreshold = effectiveHigherRateThreshold - sHigherQty

          val (higher, top: BigDecimal) = if (remainingIncome <= remainingHigherRateThreshold) {
            (remainingIncome, 0)
          } else {
            val remainingThreashold = remainingHigherRateThreshold - allowance
            (remainingThreashold, remainingIncome - remainingThreashold)
          }

          (0, higher, top)
        } else {
          val (basic, moreThanBasic) = basicTaxableIncome(dividendsIncome, allowance - dividendsAllowance, basicRateThreshold - sBasicQty - dividendsAllowance)
          val (higher, top) = higherTaxableIncome(moreThanBasic)

          (basic, higher, top)
        }
      }
    }

    // Returns a tuple of (income taxed at this level, income still to ba calculated)
    def basicTaxableIncome(dividendsIncome: BigDecimal, remainingAllowance: BigDecimal, remainingBasicRateThreshold: BigDecimal): (BigDecimal, BigDecimal) = {
      val taxableIncome = dividendsIncome - remainingAllowance - dividendsAllowance

      if (taxableIncome < 0)
        (0, 0)
      else {
        if (taxableIncome <= remainingBasicRateThreshold) {
          (taxableIncome, 0)
        } else {
          (remainingBasicRateThreshold, taxableIncome - remainingBasicRateThreshold)
        }
      }
    }

    // Returns a tuple of (income taxed at this level, income still to ba calculated)
    def higherTaxableIncome(remainingIncome: BigDecimal): (BigDecimal, BigDecimal) = {
      if (remainingIncome <= effectiveHigherRateThreshold) {
        (remainingIncome, 0)
      } else {
        (effectiveHigherRateThreshold, remainingIncome - effectiveHigherRateThreshold)
      }
    }

  }


}
