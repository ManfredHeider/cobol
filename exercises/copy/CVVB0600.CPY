      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.09.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0600.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CVVB0600.CPv  $
      *
      *   Rev 2.3   Dec 14 2018 11:59:04   t13ex44
      *APA02659: komplette maschinelle Berechnung des Ausgleichsans
      *pruches gemäß VB-Vertrag
      *
      *   Rev 2.2   Dec 14 2018 11:33:14   t13ex44
      *APA02659: komplette maschinelle Berechnung des Ausgleichsans
      *pruches gemäß VB-Vertrag
      *
      *   Rev 2.1   Sep 21 2017 12:49:08   t13ex44
      *APA02519: Auswertung Ausgleichsansprüche
      *
      *   Rev 2.0   Sep 20 2017 10:44:22   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0600)                                         *
      *        LIBRARY(T13EX44.USERTEST.COBCOPY(CVVB0600))             *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        INDVAR(YES)                                             *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0600 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      GESH_DAT_VON                   TIMESTAMP NOT NULL,
CCC004*      GESH_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      ST_PRUEFZIFFER                 CHAR(1) NOT NULL,
CCC004*      KTY_ST_TYP                     SMALLINT NOT NULL,
CCC004*      KNR_ST_TYP                     SMALLINT NOT NULL,
CCC004*      KTY_GESELL_TYP                 SMALLINT NOT NULL,
CCC004*      KNR_GESELL_TYP                 SMALLINT NOT NULL,
CCC004*      GESH_FREI                      CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0600                           *
      ******************************************************************
       01  DCL-VIVB0600.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 GESH-DAT-VON         PIC X(26).
           10 GESH-LAST-UPDATE     PIC X(26).
           10 ST-PRUEFZIFFER       PIC X(1).
           10 KTY-ST-TYP           PIC S9(4) USAGE COMP.
           10 KNR-ST-TYP           PIC S9(4) USAGE COMP.
           10 KTY-GESELL-TYP       PIC S9(4) USAGE COMP.
           10 KNR-GESELL-TYP       PIC S9(4) USAGE COMP.
           10 GESH-FREI            PIC X(1).
      ******************************************************************
      * INDICATOR VARIABLE STRUCTURE                                   *
      ******************************************************************
       01  NI-VIVB0600.
           10 NI-ST-NR             PIC S9(4) USAGE COMP.
           10 NI-GESH-DAT-VON      PIC S9(4) USAGE COMP.
           10 NI-GESH-LAST-UPDATE  PIC S9(4) USAGE COMP.
           10 NI-ST-PRUEFZIFFER    PIC S9(4) USAGE COMP.
           10 NI-KTY-ST-TYP        PIC S9(4) USAGE COMP.
           10 NI-KNR-ST-TYP        PIC S9(4) USAGE COMP.
           10 NI-KTY-GESELL-TYP    PIC S9(4) USAGE COMP.
           10 NI-KNR-GESELL-TYP    PIC S9(4) USAGE COMP.
           10 NI-GESH-FREI         PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
 
 

