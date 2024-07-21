      *********************************************************
      * Control data for DSGRAPH canned dialog.               *
      *********************************************************
      *                                                       *
      * Function-Flag codes :-                                *
      *     1  - Initialize DSGRAPH                           *
      *     2  - Closedown  DSGRAPH                           *
      *     3  - Resize the DSGRAPH window                    *
      *     4  - Redraw the DSGRAPH window                    *
      *                                                       *
      * GraphType codes :-                                    *
      *     1  - 2D pie chart                                 *
      *     2  - 3D pie chart                                 *
      *     3  - 2D bar chart                                 *
      *     4  - 3D bar chart                                 *
      *     5  - Gantt chart                                  *
      *     6  - Line graph                                   *
      *     7  - Log-linear graph                             *
      *     8  - Area graph                                   *
      *     10 - Polar graph                                  *
      *     11 - HLC                                          *
      *     13 - Tape graph                                   *
      *                                                       *
      * Max number of data sets = 10                          *
      * Max no of points within set = 33                      *
      *                                                       *
      *********************************************************

       01 DSGRAPH-CONTROL-BLOCK.
          05 Win-1-Handle                 PIC X(4)  COMP-X.
          05 Function-Flag                PIC X(2)  COMP-X.
          05 GraphType                    PIC X(2)  COMP-5.
          05 Title-Text                   PIC X(30).
          05 Title-Length                 PIC X(2)  COMP-X.
          05 X-Axis-Text                  PIC X(30).
          05 X-Axis-Text-Length           PIC X(2)  COMP-X.
          05 Y-Axis-Text                  PIC X(30).
          05 Y-Axis-Text-Length           PIC X(2)  COMP-X.
          05 Set-Legend-Text              PIC X(100).
          05 Point-Label-Text             PIC X(100).
          05 No-Of-Sets                   PIC X(4)  COMP-5.
          05 No-Of-Points                 PIC X(4)  COMP-5.

       01 DSGRAPH-DATA-BLOCK.
          05 DSGRAPH-DATA                 PIC X(1000).
