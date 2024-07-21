      *   MICRO FOCUS ENTERPRISE DEVELOPER FOR ECLIPSE  5.0.00256
      *   MICRO FOCUS BMS SCREEN PAINTER (VER BMSIDE_5-0-01)
      *   MAPSET NAME   BAT0031
      *   DATE CREATED  01/25/2021
      *   TIME CREATED  15:55:56

      *  INPUT DATA FOR MAP SIGNMAQ
         01 SIGNMAQI.
            03 FILLER                         PIC X(12).
            03 MESSHL                         PIC S9(4) COMP.
            03 MESSHF                         PIC X.
            03 FILLER REDEFINES MESSHF.
               05 MESSHA                         PIC X.
            03 MESSHI                         PIC X(79).

      *  OUTPUT DATA FOR MAP SIGNMAQ
         01 SIGNMAQO REDEFINES SIGNMAQI.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 MESSHO                         PIC X(79).

