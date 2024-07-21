      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.05.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0031.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTPA0031.CPv  $
      *
      *   Rev 2.2   Dec 20 2013 10:01:58   t13ae37
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): SST-Abrechnung
      *
      *   Rev 2.1   May 08 2013 11:47:26   t13ae37
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): SST-Abrechnung
      *
      *   Rev 2.0   May 06 2013 14:13:56   t13ae37
      *Archiverstellung
      *
      *
      ******************************************************************
      * DCLGEN TABLE(TBPA0031)                                         *
      *        LIBRARY(T130005.SRCCDCL(CTPA0031))                      *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE T13DIS.TBPA0031 TABLE
CCC004*    ( KNR_TEXT_TYP                   SMALLINT NOT NULL,
CCC004*      PKTX_ID_VON                    DECIMAL(6, 0) NOT NULL,
CCC004*      PKTX_ID_BIS                    DECIMAL(6, 0) NOT NULL,
CCC004*      KNR_DYN_KZ_2                   SMALLINT NOT NULL,
CCC004*      PKTX_TEXT_1                    VARCHAR(42) NOT NULL,
CCC004*      PKTX_TEXT_2                    VARCHAR(25) NOT NULL,
CCC004*      PKTX_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      PKTX_LAST_UPDATE               TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0031                           *
      ******************************************************************
       01  TBPA0031-GESAMT.
         05 DCL-TBPA0031.
           10 KNR-TEXT-TYP         PIC S9(4) USAGE COMP.
           10 PKTX-ID-VON          PIC S9(6)V USAGE COMP-3.
           10 PKTX-ID-BIS          PIC S9(6)V USAGE COMP-3.
           10 KNR-DYN-KZ-2         PIC S9(4) USAGE COMP.
           10 PKTX-TEXT-1.
              49 PKTX-TEXT-1-LEN   PIC S9(4) USAGE COMP.
              49 PKTX-TEXT-1-TEXT  PIC X(42).
           10 PKTX-TEXT-2.
              49 PKTX-TEXT-2-LEN   PIC S9(4) USAGE COMP.
              49 PKTX-TEXT-2-TEXT  PIC X(25).
           10 PKTX-KENNUNG         PIC X(8).
           10 PKTX-LAST-UPDATE     PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
