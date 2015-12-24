import com.nymeria.calc.Calculator._

effectiveAllowance(88060)

Salary.calculateTaxableIncomes(8060,effectiveAllowance(88060))
Salary.calculateTax(8060,effectiveAllowance(88060))

Dividends.calculateTaxableIncomes(80000,5000 + 2940,Salary.calculateTaxableIncomes(8060,effectiveAllowance(88060)))
Dividends.calculateTax(80000,5000,Salary.calculateTaxableIncomes(8060,effectiveAllowance(88060)))


calculateTotalTax(8060, 80000)