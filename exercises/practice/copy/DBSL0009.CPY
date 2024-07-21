      *---------------------------------------------------------------*
      *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------+--------+----+---------------------------+----------
      * ASL00341!12.10.09!2.2 ! NEU/WIK HINZU             ! KLEEMEIER
      *         !        !    !                           !
      *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME
      * ---------+--------+-------------------------------+----------
      * VLA-2905 !01.07.21! 4 AV-FELDER HINZU + HINWEISE  ! KLEEMEIER
      *          !        !                               !
      *
      *   MICRO FOCUS ENTERPRISE DEVELOPER FOR ECLIPSE  5.0.00256
      *   MICRO FOCUS BMS SCREEN PAINTER (VER BMSIDE_5-0-01)
      *   MAPSET NAME   BSL0009
      *   DATE CREATED  07/02/2021
      *   TIME CREATED  11:42:43

      *  INPUT DATA FOR MAP BSL0009
         01 BSL0009I.
            03 FILLER                         PIC X(12).
            03 ESTNRL                         PIC S9(4) COMP.
            03 ESTNRF                         PIC X.
            03 FILLER REDEFINES ESTNRF.
               05 ESTNRA                         PIC X.
            03 ESTNRI                         PIC X(7).
            03 ESTPRZL                        PIC S9(4) COMP.
            03 ESTPRZF                        PIC X.
            03 FILLER REDEFINES ESTPRZF.
               05 ESTPRZA                        PIC X.
            03 ESTPRZI                        PIC X(1).
            03 AINFOZLL                       PIC S9(4) COMP.
            03 AINFOZLF                       PIC X.
            03 FILLER REDEFINES AINFOZLF.
               05 AINFOZLA                       PIC X.
            03 AINFOZLI                       PIC X(65).
            03 ANAMEL                         PIC S9(4) COMP.
            03 ANAMEF                         PIC X.
            03 FILLER REDEFINES ANAMEF.
               05 ANAMEA                         PIC X.
            03 ANAMEI                         PIC X(20).
            03 AVORNAML                       PIC S9(4) COMP.
            03 AVORNAMF                       PIC X.
            03 FILLER REDEFINES AVORNAMF.
               05 AVORNAMA                       PIC X.
            03 AVORNAMI                       PIC X(15).
            03 ABTRNAML                       PIC S9(4) COMP.
            03 ABTRNAMF                       PIC X.
            03 FILLER REDEFINES ABTRNAMF.
               05 ABTRNAMA                       PIC X.
            03 ABTRNAMI                       PIC X(16).
            03 AERF1L                         PIC S9(4) COMP.
            03 AERF1F                         PIC X.
            03 FILLER REDEFINES AERF1F.
               05 AERF1A                         PIC X.
            03 AERF1I                         PIC X(2).
            03 EERF1L                         PIC S9(4) COMP.
            03 EERF1F                         PIC X.
            03 FILLER REDEFINES EERF1F.
               05 EERF1A                         PIC X.
            03 EERF1I                         PIC X(2).
            03 AINFO1L                        PIC S9(4) COMP.
            03 AINFO1F                        PIC X.
            03 FILLER REDEFINES AINFO1F.
               05 AINFO1A                        PIC X.
            03 AINFO1I                        PIC X(21).
            03 ATEXT1L                        PIC S9(4) COMP.
            03 ATEXT1F                        PIC X.
            03 FILLER REDEFINES ATEXT1F.
               05 ATEXT1A                        PIC X.
            03 ATEXT1I                        PIC X(16).
            03 ANEU1L                         PIC S9(4) COMP.
            03 ANEU1F                         PIC X.
            03 FILLER REDEFINES ANEU1F.
               05 ANEU1A                         PIC X.
            03 ANEU1I                         PIC X(3).
            03 ENEU1L                         PIC S9(4) COMP.
            03 ENEU1F                         PIC X.
            03 FILLER REDEFINES ENEU1F.
               05 ENEU1A                         PIC X.
            03 ENEU1I                         PIC X(3).
            03 AERF2L                         PIC S9(4) COMP.
            03 AERF2F                         PIC X.
            03 FILLER REDEFINES AERF2F.
               05 AERF2A                         PIC X.
            03 AERF2I                         PIC X(2).
            03 EERF2L                         PIC S9(4) COMP.
            03 EERF2F                         PIC X.
            03 FILLER REDEFINES EERF2F.
               05 EERF2A                         PIC X.
            03 EERF2I                         PIC X(2).
            03 AINFO2L                        PIC S9(4) COMP.
            03 AINFO2F                        PIC X.
            03 FILLER REDEFINES AINFO2F.
               05 AINFO2A                        PIC X.
            03 AINFO2I                        PIC X(21).
            03 ATEXT2L                        PIC S9(4) COMP.
            03 ATEXT2F                        PIC X.
            03 FILLER REDEFINES ATEXT2F.
               05 ATEXT2A                        PIC X.
            03 ATEXT2I                        PIC X(16).
            03 ANEU2L                         PIC S9(4) COMP.
            03 ANEU2F                         PIC X.
            03 FILLER REDEFINES ANEU2F.
               05 ANEU2A                         PIC X.
            03 ANEU2I                         PIC X(3).
            03 ENEU2L                         PIC S9(4) COMP.
            03 ENEU2F                         PIC X.
            03 FILLER REDEFINES ENEU2F.
               05 ENEU2A                         PIC X.
            03 ENEU2I                         PIC X(3).
            03 AERF3L                         PIC S9(4) COMP.
            03 AERF3F                         PIC X.
            03 FILLER REDEFINES AERF3F.
               05 AERF3A                         PIC X.
            03 AERF3I                         PIC X(2).
            03 EERF3L                         PIC S9(4) COMP.
            03 EERF3F                         PIC X.
            03 FILLER REDEFINES EERF3F.
               05 EERF3A                         PIC X.
            03 EERF3I                         PIC X(2).
            03 AINFO3L                        PIC S9(4) COMP.
            03 AINFO3F                        PIC X.
            03 FILLER REDEFINES AINFO3F.
               05 AINFO3A                        PIC X.
            03 AINFO3I                        PIC X(21).
            03 ATEXT3L                        PIC S9(4) COMP.
            03 ATEXT3F                        PIC X.
            03 FILLER REDEFINES ATEXT3F.
               05 ATEXT3A                        PIC X.
            03 ATEXT3I                        PIC X(16).
            03 ANEU3L                         PIC S9(4) COMP.
            03 ANEU3F                         PIC X.
            03 FILLER REDEFINES ANEU3F.
               05 ANEU3A                         PIC X.
            03 ANEU3I                         PIC X(3).
            03 ENEU3L                         PIC S9(4) COMP.
            03 ENEU3F                         PIC X.
            03 FILLER REDEFINES ENEU3F.
               05 ENEU3A                         PIC X.
            03 ENEU3I                         PIC X(3).
            03 AEDL                           PIC S9(4) COMP.
            03 AEDF                           PIC X.
            03 FILLER REDEFINES AEDF.
               05 AEDA                           PIC X.
            03 AEDI                           PIC X(2).
            03 EEDL                           PIC S9(4) COMP.
            03 EEDF                           PIC X.
            03 FILLER REDEFINES EEDF.
               05 EEDA                           PIC X.
            03 EEDI                           PIC X(2).
            03 ABETRL                         PIC S9(4) COMP.
            03 ABETRF                         PIC X.
            03 FILLER REDEFINES ABETRF.
               05 ABETRA                         PIC X.
            03 ABETRI                         PIC X(5).
            03 ABETR1L                        PIC S9(4) COMP.
            03 ABETR1F                        PIC X.
            03 FILLER REDEFINES ABETR1F.
               05 ABETR1A                        PIC X.
            03 ABETR1I                        PIC X(5).
            03 EBETR1L                        PIC S9(4) COMP.
            03 EBETR1F                        PIC X.
            03 FILLER REDEFINES EBETR1F.
               05 EBETR1A                        PIC X.
            03 EBETR1I                        PIC X(5).
            03 ABETR2L                        PIC S9(4) COMP.
            03 ABETR2F                        PIC X.
            03 FILLER REDEFINES ABETR2F.
               05 ABETR2A                        PIC X.
            03 ABETR2I                        PIC X(5).
            03 EBETR2L                        PIC S9(4) COMP.
            03 EBETR2F                        PIC X.
            03 FILLER REDEFINES EBETR2F.
               05 EBETR2A                        PIC X.
            03 EBETR2I                        PIC X(5).
            03 ASUM1L                         PIC S9(4) COMP.
            03 ASUM1F                         PIC X.
            03 FILLER REDEFINES ASUM1F.
               05 ASUM1A                         PIC X.
            03 ASUM1I                         PIC X(5).
            03 ABETR3L                        PIC S9(4) COMP.
            03 ABETR3F                        PIC X.
            03 FILLER REDEFINES ABETR3F.
               05 ABETR3A                        PIC X.
            03 ABETR3I                        PIC X(5).
            03 EBETR3L                        PIC S9(4) COMP.
            03 EBETR3F                        PIC X.
            03 FILLER REDEFINES EBETR3F.
               05 EBETR3A                        PIC X.
            03 EBETR3I                        PIC X(5).
            03 ASUM2L                         PIC S9(4) COMP.
            03 ASUM2F                         PIC X.
            03 FILLER REDEFINES ASUM2F.
               05 ASUM2A                         PIC X.
            03 ASUM2I                         PIC X(5).
            03 ABETR4L                        PIC S9(4) COMP.
            03 ABETR4F                        PIC X.
            03 FILLER REDEFINES ABETR4F.
               05 ABETR4A                        PIC X.
            03 ABETR4I                        PIC X(5).
            03 EBETR4L                        PIC S9(4) COMP.
            03 EBETR4F                        PIC X.
            03 FILLER REDEFINES EBETR4F.
               05 EBETR4A                        PIC X.
            03 EBETR4I                        PIC X(5).

      *  OUTPUT DATA FOR MAP BSL0009
         01 BSL0009O REDEFINES BSL0009I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 ESTNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 ESTPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AINFOZLO                       PIC X(65).
            03 FILLER                         PIC X(3).
            03 ANAMEO                         PIC X(20).
            03 FILLER                         PIC X(3).
            03 AVORNAMO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 ABTRNAMO                       PIC X(16).
            03 FILLER                         PIC X(3).
            03 AERF1O                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 EERF1O                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 AINFO1O                        PIC X(21).
            03 FILLER                         PIC X(3).
            03 ATEXT1O                        PIC X(16).
            03 FILLER                         PIC X(3).
            03 ANEU1O                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 ENEU1O                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 AERF2O                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 EERF2O                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 AINFO2O                        PIC X(21).
            03 FILLER                         PIC X(3).
            03 ATEXT2O                        PIC X(16).
            03 FILLER                         PIC X(3).
            03 ANEU2O                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 ENEU2O                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 AERF3O                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 EERF3O                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 AINFO3O                        PIC X(21).
            03 FILLER                         PIC X(3).
            03 ATEXT3O                        PIC X(16).
            03 FILLER                         PIC X(3).
            03 ANEU3O                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 ENEU3O                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 AEDO                           PIC X(2).
            03 FILLER                         PIC X(3).
            03 EEDO                           PIC X(2).
            03 FILLER                         PIC X(3).
            03 ABETRO                         PIC X(5).
            03 FILLER                         PIC X(3).
            03 ABETR1O                        PIC X(5).
            03 FILLER                         PIC X(3).
            03 EBETR1O                        PIC X(5).
            03 FILLER                         PIC X(3).
            03 ABETR2O                        PIC X(5).
            03 FILLER                         PIC X(3).
            03 EBETR2O                        PIC X(5).
            03 FILLER                         PIC X(3).
            03 ASUM1O                         PIC X(5).
            03 FILLER                         PIC X(3).
            03 ABETR3O                        PIC X(5).
            03 FILLER                         PIC X(3).
            03 EBETR3O                        PIC X(5).
            03 FILLER                         PIC X(3).
            03 ASUM2O                         PIC X(5).
            03 FILLER                         PIC X(3).
            03 ABETR4O                        PIC X(5).
            03 FILLER                         PIC X(3).
            03 EBETR4O                        PIC X(5).
