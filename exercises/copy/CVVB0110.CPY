      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0110.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0110.CPv  $
      *
      *   Rev 2.1   31 Oct 2007 12:05:32   t13ae13
      *AVB00905: Allfinanz Stufe 1 im VB-System: VB14, VB22, VB23 (
      *SWL)
      *
      *   Rev 2.0   25 Sep 2007 12:03:58   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVVB0110                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    25.09.2007                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00905!25.09.07!2.1 ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
CCC005*    EXEC SQL DECLARE VIVB0110 TABLE
CCC004*    ( AMUM_AM_AG_NR                  INTEGER NOT NULL,
CCC004*      AMUM_LFD_NR                    SMALLINT,
CCC004*      AMUM_NACHNAME                  CHAR(40),
CCC004*      AMUM_VORNAME                   CHAR(20),
CCC004*      KTY_ANREDE                     SMALLINT,
CCC004*      KNR_ANREDE                     SMALLINT,
CCC004*      AMUM_AD_GES_TITEL              CHAR(10),
CCC004*      AMUM_BL_KZ                     CHAR(1),
CCC004*      AMUM_EU_REG_NR                 CHAR(30),
CCC004*      AMUM_PLZ                       CHAR(9),
CCC004*      AMUM_ORT                       CHAR(40),
CCC004*      AMUM_STR                       CHAR(40),
CCC004*      AMUM_INT_AG_NR                 SMALLINT,
CCC004*      AMUM_NAME                      CHAR(40),
CCC004*      AMUM_DIR                       SMALLINT,
CCC004*      AMUM_MSNR                      SMALLINT,
CCC004*      AMUM_STUFE                     CHAR(30),
CCC004*      AMUM_PRAXIS                    SMALLINT,
CCC004*      AMUM_AMAG_NR_B                 INTEGER,
CCC004*      AMUM_NAME_B                    CHAR(40),
CCC004*      AMUM_STUFE_B                   CHAR(30),
CCC004*      AMUM_PRAXIS_B                  SMALLINT,
CCC004*      AMUM_STEUER_NR                 CHAR(20),
CCC004*      AMUM_KTO_NR                    CHAR(20),
CCC004*      AMUM_BLZ                       CHAR(11),
CCC004*      AMUM_BANK                      CHAR(40),
CCC004*      AMUM_GEB_DAT                   DATE,
CCC004*      AMUM_EIN_DAT                   DATE,
CCC004*      AMUM_UMSTEIG_KZ                CHAR(1),
CCC004*      AMUM_EIGENREG_KZ               CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0110                           *
      ******************************************************************
       01  VIVB0110-GESAMT.
        05  NI-VIVB0110-TAB.
           10  NI-VIVB0110-ELE     PIC S9(04)  COMP   OCCURS  30 TIMES.
        05  NI-VIVB0110            REDEFINES   NI-VIVB0110-TAB.
           10 NI-AMUM-AM-AG-NR     PIC S9(04)  COMP.
           10 NI-AMUM-LFD-NR       PIC S9(04)  COMP.
           10 NI-AMUM-NACHNAME     PIC S9(04)  COMP.
           10 NI-AMUM-VORNAME      PIC S9(04)  COMP.
           10 NI-KTY-ANREDE        PIC S9(04)  COMP.
           10 NI-KNR-ANREDE        PIC S9(04)  COMP.
           10 NI-AMUM-AD-GES-TITEL PIC S9(04)  COMP.
           10 NI-AMUM-BL-KZ        PIC S9(04)  COMP.
           10 NI-AMUM-EU-REG-NR    PIC S9(04)  COMP.
           10 NI-AMUM-PLZ          PIC S9(04)  COMP.
           10 NI-AMUM-ORT          PIC S9(04)  COMP.
           10 NI-AMUM-STR          PIC S9(04)  COMP.
           10 NI-AMUM-INT-AG-NR    PIC S9(04)  COMP.
           10 NI-AMUM-NAME         PIC S9(04)  COMP.
           10 NI-AMUM-DIR          PIC S9(04)  COMP.
           10 NI-AMUM-MSNR         PIC S9(04)  COMP.
           10 NI-AMUM-STUFE        PIC S9(04)  COMP.
           10 NI-AMUM-PRAXIS       PIC S9(04)  COMP.
           10 NI-AMUM-AMAG-NR-B    PIC S9(04)  COMP.
           10 NI-AMUM-NAME-B       PIC S9(04)  COMP.
           10 NI-AMUM-STUFE-B      PIC S9(04)  COMP.
           10 NI-AMUM-PRAXIS-B     PIC S9(04)  COMP.
           10 NI-AMUM-STEUER-NR    PIC S9(04)  COMP.
           10 NI-AMUM-KTO-NR       PIC S9(04)  COMP.
           10 NI-AMUM-BLZ          PIC S9(04)  COMP.
           10 NI-AMUM-BANK         PIC S9(04)  COMP.
           10 NI-AMUM-GEB-DAT      PIC S9(04)  COMP.
           10 NI-AMUM-EIN-DAT      PIC S9(04)  COMP.
           10 NI-AMUM-UMSTEIG-KZ   PIC S9(04)  COMP.
           10 NI-AMUM-EIGENREG-KZ  PIC S9(04)  COMP.
        05  DCL-VIVB0110.
           10 AMUM-AM-AG-NR       PIC S9(09) USAGE COMP.
           10 AMUM-LFD-NR         PIC S9(04) USAGE COMP.
           10 AMUM-NACHNAME       PIC X(40).
           10 AMUM-VORNAME        PIC X(20).
           10 KTY-ANREDE          PIC S9(04) USAGE COMP.
           10 KNR-ANREDE          PIC S9(04) USAGE COMP.
           10 AMUM-AD-GES-TITEL   PIC X(10).
           10 AMUM-BL-KZ          PIC X(01).
           10 AMUM-EU-REG-NR      PIC X(30).
           10 AMUM-PLZ            PIC X(09).
           10 AMUM-ORT            PIC X(40).
           10 AMUM-STR            PIC X(40).
           10 AMUM-INT-AG-NR      PIC S9(04) USAGE COMP.
           10 AMUM-NAME           PIC X(40).
           10 AMUM-DIR            PIC S9(04) USAGE COMP.
           10 AMUM-MSNR           PIC S9(04) USAGE COMP.
           10 AMUM-STUFE          PIC X(30).
           10 AMUM-PRAXIS         PIC S9(04) USAGE COMP.
           10 AMUM-AMAG-NR-B      PIC S9(09) USAGE COMP.
           10 AMUM-NAME-B         PIC X(40).
           10 AMUM-STUFE-B        PIC X(30).
           10 AMUM-PRAXIS-B       PIC S9(04) USAGE COMP.
           10 AMUM-STEUER-NR      PIC X(20).
           10 AMUM-KTO-NR         PIC X(20).
           10 AMUM-BLZ            PIC X(11).
           10 AMUM-BANK           PIC X(40).
           10 AMUM-GEB-DAT        PIC X(10).
           10 AMUM-EIN-DAT        PIC X(10).
           10 AMUM-UMSTEIG-KZ     PIC X(01).
           10 AMUM-EIGENREG-KZ    PIC X(01).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 30      *
      ******************************************************************
