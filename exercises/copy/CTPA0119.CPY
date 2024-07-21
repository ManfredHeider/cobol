      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctpa0119.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\ctpa0119.cpv  $
      *
      *   Rev 2.2   Feb 17 2000 17:12:28   T13EX13
      *APA00081 - Pgeb-Ges in SQL-Statements
      *ergaenzt
      *
      *   Rev 2.1   Mar 15 1999 14:09:36   T01JM2S
      *NT-Umstellung; Bereinigung
      *
      *   Rev 2.0   Feb 27 1999 14:26:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   04 Aug 1997 13:12:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0119)                                         *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0119 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PEST_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PEST_AVERM                     INTEGER NOT NULL,
CCC004*      PEST_KD_BETR                   INTEGER NOT NULL,
CCC004*      PEST_VM                        INTEGER NOT NULL,
CCC004*      PEST_VM_GESCHFT_KZ             CHAR(1) NOT NULL,
CCC004*      KNR_PEST_ENTSTEH               SMALLINT NOT NULL,
CCC004*      PEST_AKZ_EVS                   CHAR(1) NOT NULL,
CCC004*      PEST_AKZ_N_D_PROV              CHAR(1) NOT NULL,
CCC004*      PEST_AKZ_S_BEREIN              CHAR(1) NOT NULL,
CCC004*      PEST_AKZ_KDB_ANPAS             CHAR(1) NOT NULL,
CCC004*      PEST_STRUK_AEN_DAT             DATE NOT NULL,
CCC004*      PEST_STRUK_DAT                 DATE NOT NULL,
CCC004*      PEST_VOR_PEST_ID               DECIMAL(15, 0) NOT NULL,
CCC004*      PEST_VSTAT                     CHAR(4) NOT NULL,
CCC004*      PEST_VSTAT_UNR                 SMALLINT NOT NULL,
CCC004*      PEST_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PEST_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      PEST_PEBD_DATEN                VARCHAR(240) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0119                           *
      ******************************************************************
       01  TBPA0119-GESAMT.
         05  DCL-TBPA0119.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PEST-ID              PIC S9(15)V USAGE COMP-3.
           10 PEST-AVERM           PIC S9(9) USAGE COMP.
           10 PEST-KD-BETR         PIC S9(9) USAGE COMP.
           10 PEST-VM              PIC S9(9) USAGE COMP.
           10 PEST-VM-GESCHFT-KZ   PIC X(1).
           10 KNR-PEST-ENTSTEH     PIC S9(4) USAGE COMP.
           10 PEST-AKZ-EVS         PIC X(1).
           10 PEST-AKZ-N-D-PROV    PIC X(1).
           10 PEST-AKZ-S-BEREIN    PIC X(1).
           10 PEST-AKZ-KDB-ANPAS   PIC X(1).
           10 PEST-STRUK-AEN-DAT   PIC X(10).
           10 PEST-STRUK-DAT       PIC X(10).
           10 PEST-VOR-PEST-ID     PIC S9(15)V USAGE COMP-3.
           10 PEST-VSTAT           PIC X(4).
           10 PEST-VSTAT-UNR       PIC S9(4) USAGE COMP.
           10 PEST-LAST-UPDATE     PIC X(26).
           10 PEST-KENNUNG         PIC X(8).
           10 PEST-PEBD-DATEN.
              49 PEST-PEBD-DATEN-LEN  PIC S9(4) USAGE COMP.
              49 PEST-PEBD-DATEN-TEXT  PIC X(240).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
