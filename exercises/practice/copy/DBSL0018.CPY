      ***************************************************************
      * AENDERUNGEN:
101121* JIRA     ! DATUM  ! AENDERUNG                     ! NAME     
101121* -------------------------------------------------------------
101121* VLA-3687 !10.11.21! EQF-2 HINZU UND PVSC-KOPF     !          
101121*          !        ! ENTFERNT                      ! KLEEMEIER
      *          !        !                               !          
      *
      * ENDE-LOGBUCH
      ***************************************************************
      *
      *---------------------------------------------------------------*
      *   MICRO FOCUS ENTERPRISE DEVELOPER FOR ECLIPSE  5.0.00256
      *   MICRO FOCUS BMS SCREEN PAINTER (VER BMSIDE_5-0-01)
      *   MAPSET NAME   BSL0018
      *   DATE CREATED  11/10/2021
      *   TIME CREATED  13:14:06

      *  INPUT DATA FOR MAP BSL0018
         01 BSL0018I.
            03 FILLER                         PIC X(12).
            03 EGESL                          PIC S9(4) COMP.
            03 EGESF                          PIC X.
            03 FILLER REDEFINES EGESF.
               05 EGESA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EGESI                          PIC X(1).
            03 EBSTATL                        PIC S9(4) COMP.
            03 EBSTATF                        PIC X.
            03 FILLER REDEFINES EBSTATF.
               05 EBSTATA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EBSTATI                        PIC X(7).
            03 EARTL                          PIC S9(4) COMP.
            03 EARTF                          PIC X.
            03 FILLER REDEFINES EARTF.
               05 EARTA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EARTI                          PIC X(1).
            03 ATEXT1L                        PIC S9(4) COMP.
            03 ATEXT1F                        PIC X.
            03 FILLER REDEFINES ATEXT1F.
               05 ATEXT1A                        PIC X.
            03 FILLER                         PIC X(1).
            03 ATEXT1I                        PIC X(9).
            03 EEQFL                          PIC S9(4) COMP.
            03 EEQFF                          PIC X.
            03 FILLER REDEFINES EEQFF.
               05 EEQFA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EEQFI                          PIC X(4).
            03 ATEXT2L                        PIC S9(4) COMP.
            03 ATEXT2F                        PIC X.
            03 FILLER REDEFINES ATEXT2F.
               05 ATEXT2A                        PIC X.
            03 FILLER                         PIC X(1).
            03 ATEXT2I                        PIC X(14).
            03 ATEXT5L                        PIC S9(4) COMP.
            03 ATEXT5F                        PIC X.
            03 FILLER REDEFINES ATEXT5F.
               05 ATEXT5A                        PIC X.
            03 FILLER                         PIC X(1).
            03 ATEXT5I                        PIC X(9).
            03 EEQF2L                         PIC S9(4) COMP.
            03 EEQF2F                         PIC X.
            03 FILLER REDEFINES EEQF2F.
               05 EEQF2A                         PIC X.
            03 FILLER                         PIC X(1).
            03 EEQF2I                         PIC X(4).
            03 ATEXT6L                        PIC S9(4) COMP.
            03 ATEXT6F                        PIC X.
            03 FILLER REDEFINES ATEXT6F.
               05 ATEXT6A                        PIC X.
            03 FILLER                         PIC X(1).
            03 ATEXT6I                        PIC X(32).
            03 ATEXT7L                        PIC S9(4) COMP.
            03 ATEXT7F                        PIC X.
            03 FILLER REDEFINES ATEXT7F.
               05 ATEXT7A                        PIC X.
            03 FILLER                         PIC X(1).
            03 ATEXT7I                        PIC X(24).
            03 ATEXT8L                        PIC S9(4) COMP.
            03 ATEXT8F                        PIC X.
            03 FILLER REDEFINES ATEXT8F.
               05 ATEXT8A                        PIC X.
            03 FILLER                         PIC X(1).
            03 ATEXT8I                        PIC X(10).
            03 EPROZL                         PIC S9(4) COMP.
            03 EPROZF                         PIC X.
            03 FILLER REDEFINES EPROZF.
               05 EPROZA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EPROZI                         PIC X(3).
            03 ATEXT21L                       PIC S9(4) COMP.
            03 ATEXT21F                       PIC X.
            03 FILLER REDEFINES ATEXT21F.
               05 ATEXT21A                       PIC X.
            03 FILLER                         PIC X(1).
            03 ATEXT21I                       PIC X(1).
            03 ATEXT15L                       PIC S9(4) COMP.
            03 ATEXT15F                       PIC X.
            03 FILLER REDEFINES ATEXT15F.
               05 ATEXT15A                       PIC X.
            03 FILLER                         PIC X(1).
            03 ATEXT15I                       PIC X(10).
            03 EVBVERL                        PIC S9(4) COMP.
            03 EVBVERF                        PIC X.
            03 FILLER REDEFINES EVBVERF.
               05 EVBVERA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EVBVERI                        PIC X(4).
            03 ATEXT14L                       PIC S9(4) COMP.
            03 ATEXT14F                       PIC X.
            03 FILLER REDEFINES ATEXT14F.
               05 ATEXT14A                       PIC X.
            03 FILLER                         PIC X(1).
            03 ATEXT14I                       PIC X(11).
            03 ATEXT24L                       PIC S9(4) COMP.
            03 ATEXT24F                       PIC X.
            03 FILLER REDEFINES ATEXT24F.
               05 ATEXT24A                       PIC X.
            03 FILLER                         PIC X(1).
            03 ATEXT24I                       PIC X(14).
            03 EBSL                           PIC S9(4) COMP.
            03 EBSF                           PIC X.
            03 FILLER REDEFINES EBSF.
               05 EBSA                           PIC X.
            03 FILLER                         PIC X(1).
            03 EBSI                           PIC X(1).
            03 ATEXT25L                       PIC S9(4) COMP.
            03 ATEXT25F                       PIC X.
            03 FILLER REDEFINES ATEXT25F.
               05 ATEXT25A                       PIC X.
            03 FILLER                         PIC X(1).
            03 ATEXT25I                       PIC X(7).
            03 ATEXT3L                        PIC S9(4) COMP.
            03 ATEXT3F                        PIC X.
            03 FILLER REDEFINES ATEXT3F.
               05 ATEXT3A                        PIC X.
            03 FILLER                         PIC X(1).
            03 ATEXT3I                        PIC X(5).
            03 EPROZBL                        PIC S9(4) COMP.
            03 EPROZBF                        PIC X.
            03 FILLER REDEFINES EPROZBF.
               05 EPROZBA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EPROZBI                        PIC X(3).
            03 ATEXT4L                        PIC S9(4) COMP.
            03 ATEXT4F                        PIC X.
            03 FILLER REDEFINES ATEXT4F.
               05 ATEXT4A                        PIC X.
            03 FILLER                         PIC X(1).
            03 ATEXT4I                        PIC X(34).
            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER REDEFINES AFCANZF.
               05 AFCANZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(76).

      *  OUTPUT DATA FOR MAP BSL0018
         01 BSL0018O REDEFINES BSL0018I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 EGESH                          PIC X.
            03 EGESO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 EBSTATH                        PIC X.
            03 EBSTATO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 EARTH                          PIC X.
            03 EARTO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 ATEXT1H                        PIC X.
            03 ATEXT1O                        PIC X(9).
            03 FILLER                         PIC X(3).
            03 EEQFH                          PIC X.
            03 EEQFO                          PIC X(4).
            03 FILLER                         PIC X(3).
            03 ATEXT2H                        PIC X.
            03 ATEXT2O                        PIC X(14).
            03 FILLER                         PIC X(3).
            03 ATEXT5H                        PIC X.
            03 ATEXT5O                        PIC X(9).
            03 FILLER                         PIC X(3).
            03 EEQF2H                         PIC X.
            03 EEQF2O                         PIC X(4).
            03 FILLER                         PIC X(3).
            03 ATEXT6H                        PIC X.
            03 ATEXT6O                        PIC X(32).
            03 FILLER                         PIC X(3).
            03 ATEXT7H                        PIC X.
            03 ATEXT7O                        PIC X(24).
            03 FILLER                         PIC X(3).
            03 ATEXT8H                        PIC X.
            03 ATEXT8O                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EPROZH                         PIC X.
            03 EPROZO                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 ATEXT21H                       PIC X.
            03 ATEXT21O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 ATEXT15H                       PIC X.
            03 ATEXT15O                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 EVBVERH                        PIC X.
            03 EVBVERO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 ATEXT14H                       PIC X.
            03 ATEXT14O                       PIC X(11).
            03 FILLER                         PIC X(3).
            03 ATEXT24H                       PIC X.
            03 ATEXT24O                       PIC X(14).
            03 FILLER                         PIC X(3).
            03 EBSH                           PIC X.
            03 EBSO                           PIC X(1).
            03 FILLER                         PIC X(3).
            03 ATEXT25H                       PIC X.
            03 ATEXT25O                       PIC X(7).
            03 FILLER                         PIC X(3).
            03 ATEXT3H                        PIC X.
            03 ATEXT3O                        PIC X(5).
            03 FILLER                         PIC X(3).
            03 EPROZBH                        PIC X.
            03 EPROZBO                        PIC X(3).
            03 FILLER                         PIC X(3).
            03 ATEXT4H                        PIC X.
            03 ATEXT4O                        PIC X(34).
            03 FILLER                         PIC X(3).
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(76).
