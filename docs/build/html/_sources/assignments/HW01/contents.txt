Homework Assignment #1
======================

Homework assignment can be downloaed from :download:`here <./resources/HW_1.pdf>`


Problem #1
----------

Consider :math:`M_{\infty }` = 2.0 flow of air around the 15% thick diamond airfoil shown. Is the Full Potential Equation valid at any location in the surrouding flow? Fully explain your answer.

.. image:: ./images/HW1_pr1.png
   :width: 50%


Problem #2
----------

The Thompson scheme for generating grids is based on equations 10.21 and 10.22 in the textbook. Starting from the following equations used to define the grid:

.. math::
   \triangledown ^{2}x = 0

   \triangledown ^{2}y = 0

Derive equations 10.22. In your derivation of equations 10.22, you will need to use equations 10.21 which define the control terms P and Q.


Problem #3
----------

Show that for the parallelogram-shaped grid cell enclosed by :math:`\overline{ac}` and :math:`\overline{ab}` the cell area is equal to :math:`\frac{1}{J}` for the 2-D generalized transformation (assume :math:`\Delta \xi = \Delta \eta = 1`).

.. image:: ./images/HW1_pr3.png
   :width: 50%


Problem #4
----------

In class it was mentioned that the inverse metrics (e.g. :math:`x_{\xi}`, :math:`x_{\eta}`, :math: `y_{\xi}`, :math:`y_{\eta}`) could be obtained from finite difference approximations for these derivatives. For example, if we want to find :math:`x_{\xi}` at point b in the grid we can use:

.. math::
   \left ( x_{\xi} \right )_{b} = \left ( \frac{\partial x}{\partial \xi} \right )_{b} \approx \frac{x_{c} - x_{a}}{\xi_{c} - \xi_{a}}

.. image:: ./images/HW1_pr4.png
   :width: 50%

However, we do not use finite difference approximations to find the grid metrics (e.g. :math:`\xi_{x}`, :math:`\xi_{y}`, :math:`\eta_{x}`, :math:`\eta_{y}`). Explain why we do not use approximations like:

.. math::
   \left ( \xi_{x} \right )_{b} = \left ( \frac{\partial \xi}{\partial x} \right )_{b} \approx \frac{\xi_{c} - \xi_{a}}{x_{c} - x_{a}}

to obtain the grid metrics.


Problem #5
----------

Complete Problem 5.37 in the textbook.
