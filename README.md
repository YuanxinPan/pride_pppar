# PRIDE PPP-AR

This repository contains the GNS-only version (v1) of the [PRIDE PPP-AR](https://github.com/PrideLab/PRIDE-PPPAR)
software. It was originally released by the PRIDELab group in 2019, and maintained by me at Wuhan University until 2021. 


## Why use this older version?

There are specific reasons why you might prefer this older version (v1) over the newer release (v3):

1. **Robustness**: The latest version (v3) may encounter
   issues with certain RINEX observation files (especially RINEX-2), while this older version handles them properly.
2. **Batch processing**: The v1 script `pride_pppar` includes a batch processing feature,
   allowing you to process multiple stations and multiple days at once. In contrast, the newer v3
   script `pdp3` processes only one RINEX file per call.

If you do not face any of these specific issues, it is recommended to use the latest version of the software.


## Installation

### Linux

Ensure the `gfortran` compiler is installed on your system. If it is not, you can 
install it via your package manager: `sudo apt install gfortran` on Ubuntu/Debian.

Then, run the following commands in your terminal:

```shell
git clone git@github.com:YuanxinPan/pride_pppar.git
cd pride_pppar
bash install.sh
```

After installation, restart your terminal to apply the changes.


## Citation

If you use this software for your research, please cite the following papers:
- Geng J, Chen X, Pan Y, Zhao Q (2019). A modified phase clock/bias model to improve PPP ambiguity resolution at Wuhan University.
  Journal of Geodesy, 93(10):2053-2067. doi:10.1007/s00190-019-01301-6
- Geng J, Chen X, Pan Y, Mao S, Li C, Zhou J, Zhang K (2019). PRIDE PPP‑AR: an open‑source software for GPS PPP ambiguity resolution.
  GPS Solutions, 23(91):1-10. doi:10.1007/s10291-019-0888-1


## License
Copyright (C) 2023 by Wuhan University, All rights reserved.
