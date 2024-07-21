      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBAT0033.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DBAT0033.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 15:23:50   T13EX26
      *apa00033 - Neuerstellung
      *
      *   Rev 1.0   20 Mar 1998 11:30:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *   Micro Focus Advanced BMS Screens (ver 4.0.22)
      *   MapSet Name   BAT0033
      *   Date Created  04/09/98
      *   Time Created  09:45:57
      *   Alignment     NO

      *  Input Data For Map BAT0033
         01 BAT0033I.
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
            03 AESTDATL                         PIC S9(4) COMP.
            03 AESTDATF                         PIC X.
            03 AESTDATA REDEFINES AESTDATF      PIC X.
            03 FILLER                           PIC X(1).
            03 AESTDATI                         PIC X(10).
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
            03 ANTZABML                         PIC S9(4) COMP.
            03 ANTZABMF                         PIC X.
            03 ANTZABMA REDEFINES ANTZABMF      PIC X.
            03 FILLER                           PIC X(1).
            03 ANTZABMI                         PIC X(10).
            03 ANTZKNGL                         PIC S9(4) COMP.
            03 ANTZKNGF                         PIC X.
            03 ANTZKNGA REDEFINES ANTZKNGF      PIC X.
            03 FILLER                           PIC X(1).
            03 ANTZKNGI                         PIC X(8).
            03 ENTZZEID OCCURS 10.
               05 ENTZZEIL                      PIC S9(4) COMP.
               05 ENTZZEIF                      PIC X.
               05 ENTZZEIA REDEFINES ENTZZEIF   PIC X.
               05 FILLER                        PIC X(1).
               05 ENTZZEII                      PIC X(79).
            03 AFCANZL                          PIC S9(4) COMP.
            03 AFCANZF                          PIC X.
            03 AFCANZA REDEFINES AFCANZF        PIC X.
            03 FILLER                           PIC X(1).
            03 AFCANZI                          PIC X(79).

      *  Output Data For Map BAT0033
         01 BAT0033O REDEFINES BAT0033I.
            03 FILLER                           PIC X(12).
            03 FILLER                           PIC X(3).
            03 AKEYTX1H                         PIC X.
            03 AKEYTX1O                         PIC X(79).
            03 FILLER                           PIC X(3).
            03 AKEYTX2H                         PIC X.
            03 AKEYTX2O                         PIC X(79).
            03 FILLER                           PIC X(3).
            03 AESTDATH                         PIC X.
            03 AESTDATO                         PIC X(10).
            03 FILLER                           PIC X(3).
            03 ESEIAKTH                         PIC X.
            03 ESEIAKTO                         PIC X(3).
            03 FILLER                           PIC X(3).
            03 ASEISTRH                         PIC X.
            03 ASEISTRO                         PIC X(1).
            03 FILLER                           PIC X(3).
            03 ASEIGESH                         PIC X.
            03 ASEIGESO                         PIC X(3).
            03 FILLER                           PIC X(3).
            03 ANTZABMH                         PIC X.
            03 ANTZABMO                         PIC X(10).
            03 FILLER                           PIC X(3).
            03 ANTZKNGH                         PIC X.
            03 ANTZKNGO                         PIC X(8).
            03 DFHMS1 OCCURS 10.
               05 FILLER                        PIC X(3).
               05 ENTZZEIH                      PIC X.
               05 ENTZZEIO                      PIC X(79).
            03 FILLER                           PIC X(3).
            03 AFCANZH                          PIC X.
            03 AFCANZO                          PIC X(79).
