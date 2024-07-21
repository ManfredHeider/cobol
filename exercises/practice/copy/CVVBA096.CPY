      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.01.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA096.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA096.CPv  $
      *
      *   Rev 2.1   Feb 03 2012 13:03:40   t13ex13
      *AVB01198: Bef.bed.2011 in EBB aufnehmen
      *
      *   Rev 2.0   Jan 05 2012 16:18:34   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(BBH9.T13AES.VIVBA096)                             *
      *        LIBRARY(T130005.AE010000.SRCDCL(CVVBA096))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0096 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      KNR_TYP_BEFBED                 SMALLINT NOT NULL,
CCC004*      EBB_DAT                        DATE NOT NULL,
CCC004*      KTY_TYP_BEFBED                 SMALLINT NOT NULL,
CCC004*      EBB_EH_EIG                     DECIMAL(10, 1),
CCC004*      EBB_EH_EIG_KZ                  CHAR(1),
CCC004*      EBB_EQF_EIG                    DECIMAL(5, 2),
CCC004*      EBB_EQF_EIG_KZ                 CHAR(1),
CCC004*      EBB_EH_GRU                     DECIMAL(10, 1),
CCC004*      EBB_EH_GRU_KZ                  CHAR(1),
CCC004*      EBB_EQF_GRU                    DECIMAL(5, 2),
CCC004*      EBB_EQF_GRU_KZ                 CHAR(1),
CCC004*      EBB_EH_GES                     DECIMAL(10, 1),
CCC004*      EBB_EH_GES_KZ                  CHAR(1),
CCC004*      EBB_EQF_GES                    DECIMAL(5, 2),
CCC004*      EBB_EQF_GES_KZ                 CHAR(1),
CCC004*      EBB_PROZ_KLAUS                 DECIMAL(10, 1),
CCC004*      EBB_EIGEN_MAX                  DECIMAL(10, 1),
CCC004*      EBB_KORR_EH_GRU                DECIMAL(10, 1),
CCC004*      EBB_KORR_EH_GES                DECIMAL(10, 1),
CCC004*      EBB_KORR_KZ                    CHAR(1),
CCC004*      EBB_PART                       SMALLINT,
CCC004*      EBB_AP_KZ                      CHAR(1),
CCC004*      EBB_KZ                         CHAR(1) NOT NULL,
CCC004*      EBB_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      EBB_EH_UV_GES                  DECIMAL(10, 1),
CCC004*      EBB_EH_UV_GES_KZ               CHAR(1),
CCC004*      EBB_PART_ERS                   SMALLINT,
CCC004*      EBB_PART_RGS                   SMALLINT,
CCC004*      EBB_AP_RGS_KZ                  CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE BBH9.T13AES.VIVBA096               *
      ******************************************************************
       01  VIVBA096-GESAMT.
        05  NI-VIVBA096-TAB.
           10 NI-VIVBA096-ELE      PIC S9(04) BINARY OCCURS 30 TIMES.
        05  NI-VIVBA096            REDEFINES  NI-VIVBA096-TAB.
           10 NI-MA-ST-NR          PIC S9(4)    BINARY.
           10 NI-KNR-TYP-BEFBED    PIC S9(4)    BINARY.
           10 NI-EBB-DAT           PIC S9(4)    BINARY.
           10 NI-KTY-TYP-BEFBED    PIC S9(4)    BINARY.
           10 NI-EBB-EH-EIG        PIC S9(4)    BINARY.
           10 NI-EBB-EH-EIG-KZ     PIC S9(4)    BINARY.
           10 NI-EBB-EQF-EIG       PIC S9(4)    BINARY.
           10 NI-EBB-EQF-EIG-KZ    PIC S9(4)    BINARY.
           10 NI-EBB-EH-GRU        PIC S9(4)    BINARY.
           10 NI-EBB-EH-GRU-KZ     PIC S9(4)    BINARY.
           10 NI-EBB-EQF-GRU       PIC S9(4)    BINARY.
           10 NI-EBB-EQF-GRU-KZ    PIC S9(4)    BINARY.
           10 NI-EBB-EH-GES        PIC S9(4)    BINARY.
           10 NI-EBB-EH-GES-KZ     PIC S9(4)    BINARY.
           10 NI-EBB-EQF-GES       PIC S9(4)    BINARY.
           10 NI-EBB-EQF-GES-KZ    PIC S9(4)    BINARY.
           10 NI-EBB-PROZ-KLAUS    PIC S9(4)    BINARY.
           10 NI-EBB-EIGEN-MAX     PIC S9(4)    BINARY.
           10 NI-EBB-KORR-EH-GRU   PIC S9(4)    BINARY.
           10 NI-EBB-KORR-EH-GES   PIC S9(4)    BINARY.
           10 NI-EBB-KORR-KZ       PIC S9(4)    BINARY.
           10 NI-EBB-PART          PIC S9(4)    BINARY.
           10 NI-EBB-AP-KZ         PIC S9(4)    BINARY.
           10 NI-EBB-KZ            PIC S9(4)    BINARY.
           10 NI-EBB-LAST-UPDATE   PIC S9(4)    BINARY.
           10 NI-EBB-EH-UV-GES     PIC S9(4)    BINARY.
           10 NI-EBB-EH-UV-GES-KZ  PIC S9(4)    BINARY.
           10 NI-EBB-PART-ERS      PIC S9(4)    BINARY.
           10 NI-EBB-PART-RGS      PIC S9(4)    BINARY.
           10 NI-EBB-AP-RGS-KZ     PIC S9(4)    BINARY.
        05  DCL-VIVBA096.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 KNR-TYP-BEFBED       PIC S9(4) USAGE COMP.
           10 EBB-DAT              PIC X(10).
           10 KTY-TYP-BEFBED       PIC S9(4) USAGE COMP.
           10 EBB-EH-EIG           PIC S9(9)V9(1) USAGE COMP-3.
           10 EBB-EH-EIG-KZ        PIC X(1).
           10 EBB-EQF-EIG          PIC S9(3)V9(2) USAGE COMP-3.
           10 EBB-EQF-EIG-KZ       PIC X(1).
           10 EBB-EH-GRU           PIC S9(9)V9(1) USAGE COMP-3.
           10 EBB-EH-GRU-KZ        PIC X(1).
           10 EBB-EQF-GRU          PIC S9(3)V9(2) USAGE COMP-3.
           10 EBB-EQF-GRU-KZ       PIC X(1).
           10 EBB-EH-GES           PIC S9(9)V9(1) USAGE COMP-3.
           10 EBB-EH-GES-KZ        PIC X(1).
           10 EBB-EQF-GES          PIC S9(3)V9(2) USAGE COMP-3.
           10 EBB-EQF-GES-KZ       PIC X(1).
           10 EBB-PROZ-KLAUS       PIC S9(9)V9(1) USAGE COMP-3.
           10 EBB-EIGEN-MAX        PIC S9(9)V9(1) USAGE COMP-3.
           10 EBB-KORR-EH-GRU      PIC S9(9)V9(1) USAGE COMP-3.
           10 EBB-KORR-EH-GES      PIC S9(9)V9(1) USAGE COMP-3.
           10 EBB-KORR-KZ          PIC X(1).
           10 EBB-PART             PIC S9(4) USAGE COMP.
           10 EBB-AP-KZ            PIC X(1).
           10 EBB-KZ               PIC X(1).
           10 EBB-LAST-UPDATE      PIC X(26).
           10 EBB-EH-UV-GES        PIC S9(9)V9(1) USAGE COMP-3.
           10 EBB-EH-UV-GES-KZ     PIC X(1).
           10 EBB-PART-ERS         PIC S9(4) USAGE COMP.
           10 EBB-PART-RGS         PIC S9(4) USAGE COMP.
           10 EBB-AP-RGS-KZ        PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 30      *
      ******************************************************************
