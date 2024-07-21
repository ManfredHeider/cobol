      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0376.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0376.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   25 Mar 1998 18:21:08   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   17 Jan 1998 12:08:28   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  BEIHILFEVORSCHRIFT  TABLE
CCC004*    ( BEIHILFEVORSCHRID       SMALLINT NOT NULL,
CCC004*      BEIHILFEVORSCHRTXT      CHAR (50) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  BEIHILFEVORSCHRIFT-GESAMT.
        05  NI-BEIHILFEVORSCHRIFT-TAB.
           10 NI-BEIHILFEVORSCHRIFT-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-BEIHILFEVORSCHRIFT  REDEFINES  NI-BEIHILFEVORSCHRIFT-TAB.
           10 NI-BEIHILFEVORSCHRID        PIC S9(4) COMP.
           10 NI-BEIHILFEVORSCHRTXT       PIC S9(4) COMP.
        05  DCL-BEIHILFEVORSCHRIFT.
           10 BEIHILFEVORSCHRID           PIC S9(4) COMP.
           10 BEIHILFEVORSCHRTXT          PIC X(50).
      ******************************************************************
