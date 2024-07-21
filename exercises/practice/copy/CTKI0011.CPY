      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0011.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0011.CPv  $
      *
      *   Rev 2.0   Jun 29 1999 10:41:10   P13SP20
      *FRZ00028 - Korrektur PVCS-Kopf
      *
      *   REV 1.2   16 APR 1999 15:08:04   P13SP99
      *AKI00027
      *
      *   REV 1.1   09 MAY 1998 13:26:46   P13SP99
      *NEUERSTELLUNG
      *
      *   REV 1.0   MAY 20 1997 13:47:34   P13PVCS
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.410 19990401'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  BERUF  TABLE
CCC004*    ( BERUFID                 INTEGER  NOT NULL,
CCC004*      BERUFTEXT               CHAR (80) NOT NULL,
CCC004*      AMBERUFSCHLUESSEL       CHAR (6) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  BERUF-GESAMT.
        05  NI-BERUF-TAB.
           10 NI-BERUF-ELE PIC S9(04) COMP OCCURS 3 TIMES.
        05  NI-BERUF  REDEFINES  NI-BERUF-TAB.
           10 NI-BERUFID                  PIC S9(4) COMP.
           10 NI-BERUFTEXT                PIC S9(4) COMP.
           10 NI-AMBERUFSCHLUESSEL        PIC S9(4) COMP.
        05  DCL-BERUF.
           10 BERUFID                     PIC S9(9) COMP.
           10 BERUFTEXT                   PIC X(80).
           10 AMBERUFSCHLUESSEL           PIC X(6).
      ******************************************************************
