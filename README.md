wavyr
================

<img src="man/figures/wavyr_logo.png" data-align="right" width="120" />

`wavyr` is an R package designed for studying traveling waves in space
and time.

### Signal Processing of Traveling Waves

<figure>
<img src="man/figures/signals_spectra_schematic.png"
alt="Schematic diagram of a general signal processing system." />
<figcaption aria-hidden="true">Schematic diagram of a general signal
processing system.</figcaption>
</figure>

### Rational Signals and Rational Spectra

When we transform a traveling wave from the physical domain to the
spectral domain, uncertainty is introduced. Gabor described this limit
in the time-frequency dimension as
$\Delta t \Delta \omega \geq \frac{1}{2}$. Rational fractions in a
signal provide valuable insights into wave structures, but they are
often difficult to find or feel arbitrary. In our new method, we combine
transform uncertainty with the power of rational fractions. We use the
infinite complete binary Stern-Brocot tree to locate rational fractions
within the bounds of the uncertainty limit so that we can create
parameter-free rational approximations for any signal or spectra. This
process, the Stern-Brocot Fourier Transform, offers a novel approach to
discovering fundamental waves and feature extraction.

**The Stern-Brocot Fourier Transform:**

*Time-Frequency Dimension*

$$
\phi(\omega) = Q_{SB}(\omega, \Delta t \Delta \omega) \int_{-\infty}^\infty \psi(t) e^{-i \omega t} \, dt
$$

*Space-Wavenumber Dimension*

$$
\phi(k) = Q_{SB}(k, \Delta x \Delta k) \int_{-\infty}^\infty \psi(x) e^{-i k x} \, dx
$$

**The Inverse Stern-Brocot Fourier Transform:**

*Time-Frequency Dimension*

$$
\psi(t) = \frac{1}{2\pi} \int_{-\infty}^\infty Q_{SB}(\omega, \Delta t \Delta \omega) \phi(\omega) e^{i \omega t} \, d\omega
$$

*Space-Wavenumber Dimension*

$$
\psi(x) = \frac{1}{2\pi} \int_{-\infty}^\infty Q_{SB}(k, \Delta x \Delta k) \phi(k) e^{i k x} \, dk
$$

#### Uncertainty Principles

*Time-Frequency Uncertainty:*

$$
\Delta t \Delta \omega \geq \frac{1}{2}
$$

*Space-Wavenumber Uncertainty:*

$$
\Delta x \Delta k \geq \frac{1}{2}
$$

*Combined Time-Frequency and Space-Wavenumber Uncertainty:*

$$
\Delta t \Delta \omega \Delta x \Delta k \geq \frac{1}{4}
$$

### Relationships among Wave Properties

<figure>
<img src="man/figures/wave_properties_directed_graph.png"
alt="Directed graph of wave properties." />
<figcaption aria-hidden="true">Directed graph of wave
properties.</figcaption>
</figure>
