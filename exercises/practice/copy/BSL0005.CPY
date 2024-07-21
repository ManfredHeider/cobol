      *   Micro Focus Enterprise Developer for Eclipse  5.0.00256
      *   Micro Focus BMS Screen Painter (ver BMSIDE_5-0-01)
      *   MapSet Name   BSL0005
      *   Date Created  09/28/2022
      *   Time Created  14:34:38

      *  Input Data For Map BSL0005
         01 BSL0005I.
            03 FILLER                         PIC X(12).
            03 EAUSWL                         PIC S9(4) COMP.
            03 EAUSWF                         PIC X.
            03 FILLER REDEFINES EAUSWF.
               05 EAUSWA                         PIC X.
            03 EAUSWI                         PIC X(2).

      *  Output Data For Map BSL0005
         01 BSL0005O REDEFINES BSL0005I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 EAUSWO                         PIC X(2).

