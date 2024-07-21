      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBAT0032.CPY  $
      *    $Revision:   1.1  $
      *
      *    $Log:   L:/MF/CPY/DBAT0032.CPv  $
      *
      *   Rev 1.1   12 Nov 1998 15:23:10   T13EX26
      *apa00033 - Neuerstellung
      *
      *   Rev 1.0   20 Mar 1998 11:29:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   Micro Focus Advanced BMS Screens (ver 4.0.20)
      *   MapSet Name   BAT0032
      *   Date Created  03/20/98
      *   Time Created  11:20:48
      *   Alignment     NO

      *  Input Data For Map BAT0032
         01 BAT0032I.
            03 FILLER                           PIC X(12).
            03 AKEYTX1L                         PIC S9(4) COMP.
            03 AKEYTX1F                         PIC X.
            03 AKEYTX1A REDEFINES AKEYTX1F      PIC X.
            03 FILLER                           PIC X(1).
            03 AKEYTX1I                         PIC X(79).
            03 AKEYTX2L                         PIC S9(4) COMP.
            03 AKEYTX2F                         PIC X.
            03 AKEYTX2A REDEFINES AKEYTX2F      PIC X.
            03 FILLER                           PIC X(1).
            03 AKEYTX2I                         PIC X(79).
            03 AFCSFBL                          PIC S9(4) COMP.
            03 AFCSFBF                          PIC X.
            03 AFCSFBA REDEFINES AFCSFBF        PIC X.
            03 FILLER                           PIC X(1).
            03 AFCSFBI                          PIC X(5).
            03 EFCSL                            PIC S9(4) COMP.
            03 EFCSF                            PIC X.
            03 EFCSA REDEFINES EFCSF            PIC X.
            03 FILLER                           PIC X(1).
            03 EFCSI                            PIC X(3).
            03 AFCSANZL                         PIC S9(4) COMP.
            03 AFCSANZF                         PIC X.
            03 AFCSANZA REDEFINES AFCSANZF      PIC X.
            03 FILLER                           PIC X(1).
            03 AFCSANZI                         PIC X(20).
            03 ESEIAKTL                         PIC S9(4) COMP.
            03 ESEIAKTF                         PIC X.
            03 ESEIAKTA REDEFINES ESEIAKTF      PIC X.
            03 FILLER                           PIC X(1).
            03 ESEIAKTI                         PIC X(3).
            03 ASEISTRL                         PIC S9(4) COMP.
            03 ASEISTRF                         PIC X.
            03 ASEISTRA REDEFINES ASEISTRF      PIC X.
            03 FILLER                           PIC X(1).
            03 ASEISTRI                         PIC X(1).
            03 ASEIGESL                         PIC S9(4) COMP.
            03 ASEIGESF                         PIC X.
            03 ASEIGESA REDEFINES ASEIGESF      PIC X.
            03 FILLER                           PIC X(1).
            03 ASEIGESI                         PIC X(3).
            03 EFCD OCCURS 10.
               05 EFCL                          PIC S9(4) COMP.
               05 EFCF                          PIC X.
               05 EFCA REDEFINES EFCF           PIC X.
               05 FILLER                        PIC X(1).
               05 EFCI                          PIC X(1).
            03 AESTDATD OCCURS 10.
               05 AESTDATL                      PIC S9(4) COMP.
               05 AESTDATF                      PIC X.
               05 AESTDATA REDEFINES AESTDATF   PIC X.
               05 FILLER                        PIC X(1).
               05 AESTDATI                      PIC X(10).
            03 ANTZABMD OCCURS 10.
               05 ANTZABML                      PIC S9(4) COMP.
               05 ANTZABMF                      PIC X.
               05 ANTZABMA REDEFINES ANTZABMF   PIC X.
               05 FILLER                        PIC X(1).
               05 ANTZABMI                      PIC X(10).
            03 ANTZZEID OCCURS 10.
               05 ANTZZEIL                      PIC S9(4) COMP.
               05 ANTZZEIF                      PIC X.
               05 ANTZZEIA REDEFINES ANTZZEIF   PIC X.
               05 FILLER                        PIC X(1).
               05 ANTZZEII                      PIC X(44).
            03 ANTZKNGD OCCURS 10.
               05 ANTZKNGL                      PIC S9(4) COMP.
               05 ANTZKNGF                      PIC X.
               05 ANTZKNGA REDEFINES ANTZKNGF   PIC X.
               05 FILLER                        PIC X(1).
               05 ANTZKNGI                      PIC X(9).
            03 AFCANZL                          PIC S9(4) COMP.
            03 AFCANZF                          PIC X.
            03 AFCANZA REDEFINES AFCANZF        PIC X.
            03 FILLER                           PIC X(1).
            03 AFCANZI                          PIC X(79).

      *  Output Data For Map BAT0032
         01 BAT0032O REDEFINES BAT0032I.
            03 FILLER                           PIC X(12).
            03 FILLER                           PIC X(3).
            03 AKEYTX1H                         PIC X.
            03 AKEYTX1O                         PIC X(79).
            03 FILLER                           PIC X(3).
            03 AKEYTX2H                         PIC X.
            03 AKEYTX2O                         PIC X(79).
            03 FILLER                           PIC X(3).
            03 AFCSFBH                          PIC X.
            03 AFCSFBO                          PIC X(5).
            03 FILLER                           PIC X(3).
            03 EFCSH                            PIC X.
            03 EFCSO                            PIC X(3).
            03 FILLER                           PIC X(3).
            03 AFCSANZH                         PIC X.
            03 AFCSANZO                         PIC X(20).
            03 FILLER                           PIC X(3).
            03 ESEIAKTH                         PIC X.
            03 ESEIAKTO                         PIC X(3).
            03 FILLER                           PIC X(3).
            03 ASEISTRH                         PIC X.
            03 ASEISTRO                         PIC X(1).
            03 FILLER                           PIC X(3).
            03 ASEIGESH                         PIC X.
            03 ASEIGESO                         PIC X(3).
            03 DFHMS1 OCCURS 10.
               05 FILLER                        PIC X(3).
               05 EFCH                          PIC X.
               05 EFCO                          PIC X(1).
            03 DFHMS2 OCCURS 10.
               05 FILLER                        PIC X(3).
               05 AESTDATH                      PIC X.
               05 AESTDATO                      PIC X(10).
            03 DFHMS3 OCCURS 10.
               05 FILLER                        PIC X(3).
               05 ANTZABMH                      PIC X.
               05 ANTZABMO                      PIC X(10).
            03 DFHMS4 OCCURS 10.
               05 FILLER                        PIC X(3).
               05 ANTZZEIH                      PIC X.
               05 ANTZZEIO                      PIC X(44).
            03 DFHMS5 OCCURS 10.
               05 FILLER                        PIC X(3).
               05 ANTZKNGH                      PIC X.
               05 ANTZKNGO                      PIC X(9).
            03 FILLER                           PIC X(3).
            03 AFCANZH                          PIC X.
            03 AFCANZO                          PIC X(79).
