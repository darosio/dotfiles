# fitreview.md

# Usage: /fitreview [file]

# Invoke as: /fitreview src/chloride_fitting/models.py

Review the fitting code in $ARGUMENTS for correctness and robustness.
Check for:

1. lmfit usage

   - Parameters have appropriate bounds and initial values
   - Residual functions return correct shape
   - minimize() / Model.fit() called correctly
   - fit results checked for success (result.success, result.message)

2. ODR usage

   - sx and sy provided where measurement uncertainties are available
   - Output attributes (.beta, .sd_beta) cast to NDArray[np.float64]
   - RealData used instead of Data when uncertainties are asymmetric

3. PyMC4 usage

   - Student-t likelihood used for outlier robustness (not Gaussian)
   - HDI reported at 94% (not 95%)
   - No PyMC3 legacy API patterns
   - Proper use of pm.Data for observed values

4. General

   - No silent swallowing of fit failures
   - Parameter names stable and consistent with downstream usage
   - Log-space fitting for Kd values

Suggest improvements without changing public API or parameter names.
