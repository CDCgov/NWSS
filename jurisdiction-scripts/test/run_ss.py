import os
import sys
import subprocess
import numpy as np

from pathlib import Path

MODULE_NAME = "qsbart"
LIB_DIR = "library"

def build_module():
    so_exists = any(Path('.').glob(f"{MODULE_NAME}*.so"))
    if so_exists:
        return
    cmd = [
        sys.executable, "-m", "numpy.f2py", "-c",
        "library/qsbart.f", "library/sbart.f", "library/sgram.f",
        "library/stxwx.f", "library/sslvrg.f", "library/sinerp.f",
        "library/bvalue.f", "library/dpbfa.f", "library/dpbsl.f",
        "library/bsplvd.f", "library/interv.f", "library/rwarn.f",
        "-m", MODULE_NAME,
    ]
    env = dict(**os.environ)
    env.setdefault("FFLAGS", "-ffixed-line-length-none")
    env.setdefault("LDFLAGS", "-lopenblas")
    subprocess.check_call(cmd, env=env)


# Files needed to build the Fortran extension
FORTRAN_FILES = [
    "qsbart.f", "sbart.f", "sgram.f", "stxwx.f", "sslvrg.f",
    "sinerp.f", "bvalue.f", "dpbfa.f", "dpbsl.f", "bsplvd.f",
    "interv.f", "rwarn.f",
]


def ensure_module():
    try:
        import importlib
        return importlib.import_module(MODULE_NAME)
    except ImportError:
        build_extension()
        # Ensure the build directory is on sys.path for import
        sys.path.insert(0, LIB_DIR)
        import importlib
        return importlib.import_module(MODULE_NAME)


def build_extension():
    compile_cmd = [
        sys.executable, "-m", "numpy.f2py", "-c",
        *FORTRAN_FILES,
        "-m", MODULE_NAME, "--dep", "blas",
    ]
    env = dict(os.environ)
    env.setdefault("FFLAGS", "-ffixed-line-length-none")
    subprocess.check_call(compile_cmd, cwd=LIB_DIR, env=env)


def nknots_smspl(n: int) -> int:
    if n < 50:
        return n
    a1 = np.log2(50)
    a2 = np.log2(100)
    a3 = np.log2(140)
    a4 = np.log2(200)
    if n < 200:
        return int(np.trunc(2 ** (a1 + (a2 - a1) * (n - 50) / 150)))
    elif n < 800:
        return int(np.trunc(2 ** (a2 + (a3 - a2) * (n - 200) / 600)))
    elif n < 3200:
        return int(np.trunc(2 ** (a3 + (a4 - a3) * (n - 800) / 2400)))
    else:
        return int(np.trunc(200 + (n - 3200) ** 0.2))


def smooth_spline_mod(x, y, spar=0.5):
    # qsbart = ensure_module()
    
    import qsbart

    x = np.asarray(x, dtype=np.float64)
    y = np.asarray(y, dtype=np.float64)
    if np.any(~np.isfinite(x)) or np.any(~np.isfinite(y)):
        raise ValueError("x and y must be finite")
    if x.size < 4:
        raise ValueError("need at least four unique 'x' values")

    # Simplified handling for unique x and unit weights
    xbar = (x - x[0]) / (x[-1] - x[0])
    nknots = nknots_smspl(x.size)
    knot = np.concatenate([np.repeat(xbar[0], 3), xbar, np.repeat(xbar[-1], 3)])
    nk = nknots + 2

    coef = np.zeros(nk, dtype=np.float64)
    ty = np.zeros(x.size, dtype=np.float64)
    lev = np.zeros(x.size, dtype=np.float64)
    crit = np.array(0.0, dtype=np.float64)
    iparms = np.array([1, 1, 500, 0], dtype=np.int32)
    parms = np.array([-1.5, 1.5, 1e-4, 2e-8, -1.0], dtype=np.float64)
    scratch = np.zeros((17 + 1) * nk + 1, dtype=np.float64)
    ier = np.array(0, dtype=np.int32)

    qsbart.rbart(1.0, 0.0, xbar, y, np.ones_like(xbar), 0.0, knot,
                 coef, ty, lev, crit, iparms, float(spar), parms,
                 scratch, 4, 1, ier)

    if ier != 0:
        raise RuntimeError(f"rbart returned error code {ier}")
    return ty


def main():
    build_module()
    x = np.arange(1, 11, dtype=np.float64)
    y = np.array([2, 4, 3, 5, 6, 7, 8, 1, 10, 0], dtype=np.float64)
    fitted_values = smooth_spline_mod(x, y, spar=0.5)
    print(fitted_values)


if __name__ == "__main__":
    main()
