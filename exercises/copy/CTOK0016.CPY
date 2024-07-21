 
      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTOK0016.CPY  $
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/CTOK0016.CPv  $
      *
      *   Rev 2.11   Mar 31 2016 08:22:08   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.10   Feb 12 2016 08:26:54   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.8   Nov 19 2015 08:17:32   t13ae34
      *AAN01211: Datentyp EA_PIN in Elektr_Antraege ändern
      *
      *   Rev 2.7   Nov 09 2015 15:23:30   t13ae34
      *AAN01211: Datentyp EA_PIN in Elektr_Antraege ändern
      *
      *   Rev 2.6   May 28 2015 09:55:58   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.5   Apr 23 2015 13:26:22   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.4   29 Jul 2008 16:21:14   T13EX32
      *AKI00733: Elektr. Anträge. Anpassen an neue Datenlänge für A
      *dvocard
      *
      *   Rev 2.3   29 Apr 2008 10:16:20   T13EX32
      *AKI00556: Datenabgleich HOST-DB <-> Online-DB
      *
      *   Rev 2.2   07 Apr 2008 14:04:36   T13EX32
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.1   12 Jul 2007 16:08:08   t13ex43
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.0   22 Feb 2007 16:13:16   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    22.02.2007                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AKI00556!22.02.07!2.1 ! NEU                       ! KLEEMEIER *
      * AAN01211!09.11.15!2.7 ! EA_PIN DECIMAL 10         ! HEIDER    *
      * AAN01230!12.02.16!2.10! RÜCKBAU ELEKTR_ANTRAEGE   ! HEIDER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      * DCLGEN TABLE(TCOK0016) ELEKTR_ANTRAEGE                        *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTOK0016)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE TCOK0016 TABLE
CCC004*    ( EA_DATEINAME                   CHAR(50) NOT NULL,
CCC004*      EA_STATUS_KTY                  SMALLINT NOT NULL,
CCC004*      EA_STATUS_KNR                  SMALLINT NOT NULL,
CCC004*      EA_GES_KTY                     SMALLINT,
CCC004*      EA_GES_KNR                     SMALLINT,
CCC004*      EA_KUNDE_NR                    INTEGER,
CCC004*      EA_MITARBEITER_NR              INTEGER,
CCC004*      EA_AUSLESE_JOB                 DECIMAL(18, 0),
CCC004*      EA_O_LETZTE_AEND               CHAR(26),
CCC004*      EA_VA_KZ                       CHAR(1) NOT NULL,
CCC004*      EA_VERARBEITET_AM              TIMESTAMP,
CCC004*      EA_BLACK_BOX_DATA              VARCHAR(32000) NOT NULL,
CCC004*      EA_ESIGN                       CHAR(1) NOT NULL,
CCC004*      EA_PIN                         DECIMAL(10, 0)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TCOK0016                           *
      ******************************************************************
       01  TCOK0016-GESAMT.
        05  NI-TCOK0016-TAB.
           10 NI-TCOK0016-ELE      PIC S9(04) BINARY OCCURS  14 TIMES.
        05  NI-TCOK0016            REDEFINES   NI-TCOK0016-TAB.
           10 NI-EA-DATEINAME             PIC S9(04)       BINARY.
           10 NI-EA-STATUS-KTY            PIC S9(04)       BINARY.
           10 NI-EA-STATUS-KNR            PIC S9(04)       BINARY.
           10 NI-EA-GES-KTY               PIC S9(04)       BINARY.
           10 NI-EA-GES-KNR               PIC S9(04)       BINARY.
           10 NI-EA-KUNDE-NR              PIC S9(04)       BINARY.
           10 NI-EA-MITARBEITER-NR        PIC S9(04)       BINARY.
           10 NI-EA-AUSLESE-JOB           PIC S9(04)       BINARY.
           10 NI-EA-O-LETZTE-AEND         PIC S9(04)       BINARY.
           10 NI-EA-VA-KZ                 PIC S9(04)       BINARY.
           10 NI-EA-VERARBEITET-AM        PIC S9(04)       BINARY.
           10 NI-EA-BLACK-BOX-DATA        PIC S9(04)       BINARY.
           10 NI-EA-ESIGN                 PIC S9(04)       BINARY.
           10 NI-EA-PIN                   PIC S9(04)       BINARY.
 
 
        05  DCL-TCOK0016.
           10 EA-DATEINAME         PIC X(50).
           10 EA-STATUS-KTY        PIC S9(4) USAGE COMP.
           10 EA-STATUS-KNR        PIC S9(4) USAGE COMP.
           10 EA-GES-KTY           PIC S9(4) USAGE COMP.
           10 EA-GES-KNR           PIC S9(4) USAGE COMP.
           10 EA-KUNDE-NR          PIC S9(9) USAGE COMP.
           10 EA-MITARBEITER-NR    PIC S9(9) USAGE COMP.
           10 EA-AUSLESE-JOB       PIC S9(18)V USAGE COMP-3.
           10 EA-O-LETZTE-AEND     PIC X(26).
           10 EA-VA-KZ             PIC X(1).
           10 EA-VERARBEITET-AM    PIC X(26).
           10 EA-BLACK-BOX-DATA.
              49 EA-BLACK-BOX-DATA-LEN
                 PIC S9(4) USAGE COMP.
              49 EA-BLACK-BOX-DATA-TEXT
                 PIC X(32000).
           10 EA-ESIGN             PIC X(1).
           10 EA-PIN               PIC S9(10)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
 
 


