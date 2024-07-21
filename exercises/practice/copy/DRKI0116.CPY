      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0116.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0116.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:52:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 May 1998 10:33:58   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:48:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      * --------------------------------------------------------------
      * KONVERTIERUNG AML - PERSONENROLLE
      * PARAMETERS FOR UPKI0038
      * --------------------------------------------------------------
       01  DRKI0116-PARAM.
           03  DRKI0116-AML-PERSROLLE     PIC 9(4).
               88 DRKI0116-AML-K1 VALUE 0001.
               88 DRKI0116-AML-K2 VALUE 0002.
               88 DRKI0116-AML-K29 VALUE 0029.
               88 DRKI0116-AML-K3 VALUE 0003.
               88 DRKI0116-AML-K4 VALUE 0004.
               88 DRKI0116-AML-K5 VALUE 0005.
           03  DRKI0116-VRBPERSONENROLLEKZ     PIC X.
           03  DRKI0116-S-VRBPKZ-GEFUELLT      PIC X.
           03  DRKI0116-VTRPERSONENROLLEKZ     PIC X.
           03  DRKI0116-S-VTRPKZ-GEFUELLT      PIC X.
           03  DRKI0116-VTRBEZIEHUNGSARTKZ     PIC X.
           03  DRKI0116-S-VTRBEZ-GEFUELLT      PIC X.
           03  DRKI0116-RETURN-CODE        PIC S9.
