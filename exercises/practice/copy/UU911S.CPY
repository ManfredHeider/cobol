      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU911S.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/UU911S.CPv  $
      *
      *   Rev 2.1   Oct 05 2001 11:53:08   t13ae34
      *AAN00311 - PZM 2.release
      *
      *   Rev 2.0   Feb 27 1999 15:15:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:05:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   1    10.08.92 17:08:32
      *    --------------------------------------------------------
      *
      *    BILD-TEXT-TABELLE FUER KEY-BILD
      *
      *    TRANSAKTIONEN:
      *    ANTRAGSBEARBEITUNG              ERSTELLT: H.SCHORN (AGS)
      *                                       DATUM: 14.01.80
      *    GEAENDERT:   10.08.92   § 34C GEWO        MUELLER
      *    --------------------------------------------------------
      *****************************************************************
      * LOGBUCH:                                                     *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00311!19.09.01!2.1 ! PZM 2.RELEASE             ! HEIDER   *
      *         !        !    !                           !          *
      *****************************************************************
      * ENDE LOGBUCH                                                 *
      *****************************************************************
CCC009*    SKIP1
       01  UU911S.
           02 FILLER PIC X(32) VALUE '001      VERMOEGENSBERATER-NR.:1'.
           02 FILLER PIC X(32) VALUE '002          GESELLSCHAFTS-NR.:2'.
           02 FILLER PIC X(32) VALUE '004    UND   GESELLSCHAFTS-NR.:2'.
           02 FILLER PIC X(32) VALUE '008UND EVTL. GESELLSCHAFTS-NR.:2'.
190901*    02 FILLER PIC X(32) VALUE '016UND EVTL.      VERTRAGS-NR.:3'.
190901     02 FILLER PIC X(32) VALUE '016UND EVTL. VERTRNR/VORSCHLNR:3'.
           02 FILLER PIC X(32) VALUE '032    ODER         MATCH-CODE:4'.
           02 FILLER PIC X(32) VALUE '064    UND        VERTRAGS-NR.:3'.
           02 FILLER PIC X(32) VALUE '128    UND   PRODUKTSCHLUESSEL:3'.
           02 FILLER PIC X(32) VALUE '256        VB-NR §34C ZEICHNER:4'.
CCC009*    SKIP1
       01  TEXT-TAB  REDEFINES  TEXT-TAB1.
           02  TEXT-TB1                OCCURS 9.
               03  TEXTNR1             PIC 9(3).
               03  TEXT1               PIC X(28).
               03  ZEILE1              PIC 9.

