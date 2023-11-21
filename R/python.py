import numpy as np
from scipy.stats import truncnorm

def ragg(N, mu_0=None, sd_0=None, a=0, b=np.inf):
    if mu_0 is not None and sd_0 is not None and a == -np.inf and b == np.inf:
        # Normal distribution
        return np.random.normal(mu_0, sd_0, N)
    elif mu_0 is not None and sd_0 is not None and a == 0 and b == np.inf:
        # Truncated normal
        a, b = (a - mu_0) / sd_0, (b - mu_0) / sd_0  # Convert bounds to standard normal
        return truncnorm.rvs(a, b, loc=mu_0, scale=sd_0, size=N)
    elif mu_0 is not None and sd_0 is None and a == 0 and b == np.inf:
        # Exponential
        return np.random.exponential(scale=mu_0, size=N)
    elif mu_0 is None and sd_0 is None and np.isfinite(a) and np.isfinite(b):
        # Uniform
        return np.random.uniform(a, b, N)
    else:
        raise ValueError('Case not implemented atm.')


