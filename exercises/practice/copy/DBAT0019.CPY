      *   MICRO FOCUS ENTERPRISE DEVELOPER FOR ECLIPSE  5.0.00256
      *   MICRO FOCUS BMS SCREEN PAINTER (VER BMSIDE_5-0-01)
      *   MAPSET NAME   BAT0019
      *   DATE CREATED  01/25/2021
      *   TIME CREATED  15:45:43

      *  INPUT DATA FOR MAP SIGNMAP
         01 SIGNMAPI.
            03 FILLER                         PIC X(12).
            03 DATIDL                         PIC S9(4) COMP.
            03 DATIDF                         PIC X.
            03 FILLER REDEFINES DATIDF.
               05 DATIDA                         PIC X.
            03 DATIDI                         PIC X(8).
            03 NEWS1L                         PIC S9(4) COMP.
            03 NEWS1F                         PIC X.
            03 FILLER REDEFINES NEWS1F.
               05 NEWS1A                         PIC X.
            03 NEWS1I                         PIC X(34).
            03 NEWS2L                         PIC S9(4) COMP.
            03 NEWS2F                         PIC X.
            03 FILLER REDEFINES NEWS2F.
               05 NEWS2A                         PIC X.
            03 NEWS2I                         PIC X(34).
            03 NEWS3L                         PIC S9(4) COMP.
            03 NEWS3F                         PIC X.
            03 FILLER REDEFINES NEWS3F.
               05 NEWS3A                         PIC X.
            03 NEWS3I                         PIC X(34).
            03 NEWS4L                         PIC S9(4) COMP.
            03 NEWS4F                         PIC X.
            03 FILLER REDEFINES NEWS4F.
               05 NEWS4A                         PIC X.
            03 NEWS4I                         PIC X(34).
            03 NEWS5L                         PIC S9(4) COMP.
            03 NEWS5F                         PIC X.
            03 FILLER REDEFINES NEWS5F.
               05 NEWS5A                         PIC X.
            03 NEWS5I                         PIC X(34).
            03 NEWS6L                         PIC S9(4) COMP.
            03 NEWS6F                         PIC X.
            03 FILLER REDEFINES NEWS6F.
               05 NEWS6A                         PIC X.
            03 NEWS6I                         PIC X(34).
            03 NEWS7L                         PIC S9(4) COMP.
            03 NEWS7F                         PIC X.
            03 FILLER REDEFINES NEWS7F.
               05 NEWS7A                         PIC X.
            03 NEWS7I                         PIC X(34).
            03 NEWS8L                         PIC S9(4) COMP.
            03 NEWS8F                         PIC X.
            03 FILLER REDEFINES NEWS8F.
               05 NEWS8A                         PIC X.
            03 NEWS8I                         PIC X(34).
            03 NEWS9L                         PIC S9(4) COMP.
            03 NEWS9F                         PIC X.
            03 FILLER REDEFINES NEWS9F.
               05 NEWS9A                         PIC X.
            03 NEWS9I                         PIC X(34).
            03 NEWS10L                        PIC S9(4) COMP.
            03 NEWS10F                        PIC X.
            03 FILLER REDEFINES NEWS10F.
               05 NEWS10A                        PIC X.
            03 NEWS10I                        PIC X(34).
            03 NEWS11L                        PIC S9(4) COMP.
            03 NEWS11F                        PIC X.
            03 FILLER REDEFINES NEWS11F.
               05 NEWS11A                        PIC X.
            03 NEWS11I                        PIC X(34).
            03 NEWS12L                        PIC S9(4) COMP.
            03 NEWS12F                        PIC X.
            03 FILLER REDEFINES NEWS12F.
               05 NEWS12A                        PIC X.
            03 NEWS12I                        PIC X(34).
            03 USERIDL                        PIC S9(4) COMP.
            03 USERIDF                        PIC X.
            03 FILLER REDEFINES USERIDF.
               05 USERIDA                        PIC X.
            03 USERIDI                        PIC X(7).
            03 NEWS13L                        PIC S9(4) COMP.
            03 NEWS13F                        PIC X.
            03 FILLER REDEFINES NEWS13F.
               05 NEWS13A                        PIC X.
            03 NEWS13I                        PIC X(34).
            03 NEWS14L                        PIC S9(4) COMP.
            03 NEWS14F                        PIC X.
            03 FILLER REDEFINES NEWS14F.
               05 NEWS14A                        PIC X.
            03 NEWS14I                        PIC X(34).
            03 PWORDL                         PIC S9(4) COMP.
            03 PWORDF                         PIC X.
            03 FILLER REDEFINES PWORDF.
               05 PWORDA                         PIC X.
            03 PWORDI                         PIC X(127).
            03 NPWORD1L                       PIC S9(4) COMP.
            03 NPWORD1F                       PIC X.
            03 FILLER REDEFINES NPWORD1F.
               05 NPWORD1A                       PIC X.
            03 NPWORD1I                       PIC X(127).
            03 NPWORD2L                       PIC S9(4) COMP.
            03 NPWORD2F                       PIC X.
            03 FILLER REDEFINES NPWORD2F.
               05 NPWORD2A                       PIC X.
            03 NPWORD2I                       PIC X(127).
            03 ANWENDL                        PIC S9(4) COMP.
            03 ANWENDF                        PIC X.
            03 FILLER REDEFINES ANWENDF.
               05 ANWENDA                        PIC X.
            03 ANWENDI                        PIC X(4).
            03 OBL                            PIC S9(4) COMP.
            03 OBF                            PIC X.
            03 FILLER REDEFINES OBF.
               05 OBA                            PIC X.
            03 OBI                            PIC X(32).
            03 MESSL                          PIC S9(4) COMP.
            03 MESSF                          PIC X.
            03 FILLER REDEFINES MESSF.
               05 MESSA                          PIC X.
            03 MESSI                          PIC X(79).

      *  OUTPUT DATA FOR MAP SIGNMAP
         01 SIGNMAPO REDEFINES SIGNMAPI.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 DATIDO                         PIC X(8).
            03 FILLER                         PIC X(3).
            03 NEWS1O                         PIC X(34).
            03 FILLER                         PIC X(3).
            03 NEWS2O                         PIC X(34).
            03 FILLER                         PIC X(3).
            03 NEWS3O                         PIC X(34).
            03 FILLER                         PIC X(3).
            03 NEWS4O                         PIC X(34).
            03 FILLER                         PIC X(3).
            03 NEWS5O                         PIC X(34).
            03 FILLER                         PIC X(3).
            03 NEWS6O                         PIC X(34).
            03 FILLER                         PIC X(3).
            03 NEWS7O                         PIC X(34).
            03 FILLER                         PIC X(3).
            03 NEWS8O                         PIC X(34).
            03 FILLER                         PIC X(3).
            03 NEWS9O                         PIC X(34).
            03 FILLER                         PIC X(3).
            03 NEWS10O                        PIC X(34).
            03 FILLER                         PIC X(3).
            03 NEWS11O                        PIC X(34).
            03 FILLER                         PIC X(3).
            03 NEWS12O                        PIC X(34).
            03 FILLER                         PIC X(3).
            03 USERIDO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 NEWS13O                        PIC X(34).
            03 FILLER                         PIC X(3).
            03 NEWS14O                        PIC X(34).
            03 FILLER                         PIC X(3).
            03 PWORDO                         PIC X(127).
            03 FILLER                         PIC X(3).
            03 NPWORD1O                       PIC X(127).
            03 FILLER                         PIC X(3).
            03 NPWORD2O                       PIC X(127).
            03 FILLER                         PIC X(3).
            03 ANWENDO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 OBO                            PIC X(32).
            03 FILLER                         PIC X(3).
            03 MESSO                          PIC X(79).

