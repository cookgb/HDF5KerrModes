# HDF5KerrModes
This repository contains *Mathematica paclet* called __HDF5KerrModes\`__ for reading and plotting Kerr-mode data stored in HDF5 format.

---
## Paclet

#### HDF5KerrModes\`
The actual *Mathematica paclet* is found in the HDF5KerrModes directory in the top level of the repository.

---
## Build and Installing the Paclet

The file __CreatePaclets.nb__ is included in the top level of the repository.  It can be used to build the paclet from source.  Once built, the paclet can be found in the source directory __HDF5KerrModes/build__ with the name __HDF5KerrModes-#.#.#.paclet__. Public releases will also be available containing pre-built versions of the __HDF5KerrModes\`__ paclet which can be installed directly into your *Mathematica* installation.

Paclet installation is accomplished by using the *Mathematica* function __PacletInstall__.

---
## Tables of Quasinormal Modes.

The __HDF5KerrModes\`__ paclet requires a set of tabulated Kerr modes stored in a set of HDF5 files.  These files are not included in the Github repository, but can be found on [Zenodo](https://doi.org/10.5281/zenodo.14024959 "Kerr Mode Repo").  Note that __HDF5KerrModes\`__ requires the files from __*Version V3*__.  Prior versions use a data format that will not work correctly.  The tabulated quasinormal modes are stores in files named __KerrQNM___*nn*__.h5__, where *nn* is a 2-digit number indicating the value of the overtones for the modes stored in each file.  The QNM data was constructed using the methods outlined in [Cook & Zalutskiy, Phys. Rev. D 90 (2014) pp. 124021](https://doi.org/10.1103/PhysRevD.90.124021).  The tabulated Total Transmission modes are stores in files named __KerrTTML___*nn*__.h5__ and __KerrTTMR___*nn*__.h5__.  The 2-digit overtone number *nn* is used to designate one of three known TTM families as described in [Cook & Lu, Phys. Rev. D 107 (2023) pp. 044043](https://doi.org/10.1103/PhysRevD.107.044043).
