      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBCV0001.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DBCV0001.CPv  $
      *
      *   Rev 2.1   Jun 07 1999 13:38:22   T13AE35
      * 
      *
      *   Rev 2.0   Jun 02 1999 14:44:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   Micro Focus BMS Screen Painter (ver MFBMW 1.1.14)
      *   MapSet Name   BCV0001
      *   Date Created  06/07/1999
      *   Time Created  12:15:48

      *  Input Data For Map BCV0001
         01 BCV0001I.
            03 FILLER                         PIC X(12).
            03 CPNRL                          PIC S9(4) COMP.
            03 CPNRF                          PIC X.
            03 FILLER REDEFINES CPNRF.
               05 CPNRA                          PIC X.
            03 CPNRI                          PIC X(7).
            03 CDATL                          PIC S9(4) COMP.
            03 CDATF                          PIC X.
            03 FILLER REDEFINES CDATF.
               05 CDATA                          PIC X.
            03 CDATI                          PIC X(8).
            03 CUHRL                          PIC S9(4) COMP.
            03 CUHRF                          PIC X.
            03 FILLER REDEFINES CUHRF.
               05 CUHRA                          PIC X.
            03 CUHRI                          PIC X(5).
            03 CFKTL                          PIC S9(4) COMP.
            03 CFKTF                          PIC X.
            03 FILLER REDEFINES CFKTF.
               05 CFKTA                          PIC X.
            03 CFKTI                          PIC X(1).
            03 CBRFL                          PIC S9(4) COMP.
            03 CBRFF                          PIC X.
            03 FILLER REDEFINES CBRFF.
               05 CBRFA                          PIC X.
            03 CBRFI                          PIC X(16).
            03 CVBNRL                         PIC S9(4) COMP.
            03 CVBNRF                         PIC X.
            03 FILLER REDEFINES CVBNRF.
               05 CVBNRA                         PIC X.
            03 CVBNRI                         PIC X(7).
            03 CFEHLL                         PIC S9(4) COMP.
            03 CFEHLF                         PIC X.
            03 FILLER REDEFINES CFEHLF.
               05 CFEHLA                         PIC X.
            03 CFEHLI                         PIC X(79).

      *  Output Data For Map BCV0001
         01 BCV0001O REDEFINES BCV0001I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 CPNRO                          PIC X(7).
            03 FILLER                         PIC X(3).
            03 CDATO                          PIC X(8).
            03 FILLER                         PIC X(3).
            03 CUHRO                          PIC X(5).
            03 FILLER                         PIC X(3).
            03 CFKTO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 CBRFO                          PIC X(16).
            03 FILLER                         PIC X(3).
            03 CVBNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 CFEHLO                         PIC X(79).

