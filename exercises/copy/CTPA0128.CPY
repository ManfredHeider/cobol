      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctpa0128.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\ctpa0128.cpv  $
      *
      *   Rev 2.1   Feb 17 2000 17:13:12   T13EX13
      *APA00081 - Pgeb-Ges in SQL-Statements
      *ergaenzt
      *
      *   Rev 2.0   May 27 1999 14:36:40   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   04 Aug 1997 13:13:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      * DCLGEN TABLE(TBPA0128)                                         *
      *                                                                *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0128 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PRINTE                         DECIMAL(15, 0) NOT NULL,
CCC004*      PAEV_AKT_EINH                  DECIMAL(8, 2) NOT NULL,
CCC004*      KNR_ZUSTAND_EH_MAX             SMALLINT NOT NULL,
CCC004*      PAEV_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PAEV_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0128                           *
      ******************************************************************
       01  TBPA0128-GESAMT.
         05  DCL-TBPA0128.
010200     10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PRINTE               PIC S9(15)V USAGE COMP-3.
           10 PAEV-AKT-EINH        PIC S9(6)V9(2) USAGE COMP-3.
           10 KNR-ZUSTAND-EH-MAX   PIC S9(4) USAGE COMP.
           10 PAEV-LAST-UPDATE     PIC X(26).
           10 PAEV-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
