      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.02.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0112.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0112.CPv  $
      *
      *   Rev 2.1   Mar 18 2008 07:56:36   t13ae13
      *AVB00961: FVD Stufe 1 -> VB15, VB22, VB21 (swl)
      *
      *   Rev 2.0   28 Feb 2008 10:02:50   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVVB0112                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    28.02.2008                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00961!28.02.08!2.1 ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
CCC005*    EXEC SQL DECLARE VIVB0112 TABLE
CCC004*    ( FVDUM_AM_AG_NR                  INTEGER NOT NULL,
CCC004*      FVDUM_LFD_NR                    SMALLINT,
CCC004*      FVDUM_NACHNAME                  CHAR(40),
CCC004*      FVDUM_VORNAME                   CHAR(20),
CCC004*      KTY_ANREDE                      SMALLINT,
CCC004*      KNR_ANREDE                      SMALLINT,
CCC004*      FVDUM_AD_GES_TITEL              CHAR(10),
CCC004*      FVDUM_BL_KZ                     CHAR(1),
CCC004*      FVDUM_EU_REG_NR                 CHAR(30),
CCC004*      FVDUM_PLZ                       CHAR(9),
CCC004*      FVDUM_ORT                       CHAR(40),
CCC004*      FVDUM_STR                       CHAR(40),
CCC004*      FVDUM_INT_AG_NR                 SMALLINT,
CCC004*      FVDUM_NAME                      CHAR(40),
CCC004*      FVDUM_DIR                       SMALLINT,
CCC004*      FVDUM_MSNR                      SMALLINT,
CCC004*      FVDUM_STUFE                     CHAR(30),
CCC004*      FVDUM_PRAXIS                    SMALLINT,
CCC004*      FVDUM_AMAG_NR_B                 INTEGER,
CCC004*      FVDUM_NAME_B                    CHAR(40),
CCC004*      FVDUM_STUFE_B                   CHAR(30),
CCC004*      FVDUM_PRAXIS_B                  SMALLINT,
CCC004*      FVDUM_STEUER_NR                 CHAR(20),
CCC004*      FVDUM_KTO_NR                    CHAR(20),
CCC004*      FVDUM_BLZ                       CHAR(11),
CCC004*      FVDUM_BANK                      CHAR(40),
CCC004*      FVDUM_GEB_DAT                   DATE,
CCC004*      FVDUM_EIN_DAT                   DATE,
CCC004*      FVDUM_UMSTEIG_KZ                CHAR(1),
CCC004*      FVDUM_EIGENREG_KZ               CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0112                           *
      ******************************************************************
       01  VIVB0112-GESAMT.
        05  NI-VIVB0112-TAB.
           10  NI-VIVB0112-ELE      PIC S9(04)  COMP   OCCURS  30 TIMES.
        05  NI-VIVB0112             REDEFINES   NI-VIVB0112-TAB.
           10 NI-FVDUM-AM-AG-NR     PIC S9(04)  COMP.
           10 NI-FVDUM-LFD-NR       PIC S9(04)  COMP.
           10 NI-FVDUM-NACHNAME     PIC S9(04)  COMP.
           10 NI-FVDUM-VORNAME      PIC S9(04)  COMP.
           10 NI-KTY-ANREDE         PIC S9(04)  COMP.
           10 NI-KNR-ANREDE         PIC S9(04)  COMP.
           10 NI-FVDUM-AD-GES-TITEL PIC S9(04)  COMP.
           10 NI-FVDUM-BL-KZ        PIC S9(04)  COMP.
           10 NI-FVDUM-EU-REG-NR    PIC S9(04)  COMP.
           10 NI-FVDUM-PLZ          PIC S9(04)  COMP.
           10 NI-FVDUM-ORT          PIC S9(04)  COMP.
           10 NI-FVDUM-STR          PIC S9(04)  COMP.
           10 NI-FVDUM-INT-AG-NR    PIC S9(04)  COMP.
           10 NI-FVDUM-NAME         PIC S9(04)  COMP.
           10 NI-FVDUM-DIR          PIC S9(04)  COMP.
           10 NI-FVDUM-MSNR         PIC S9(04)  COMP.
           10 NI-FVDUM-STUFE        PIC S9(04)  COMP.
           10 NI-FVDUM-PRAXIS       PIC S9(04)  COMP.
           10 NI-FVDUM-AMAG-NR-B    PIC S9(04)  COMP.
           10 NI-FVDUM-NAME-B       PIC S9(04)  COMP.
           10 NI-FVDUM-STUFE-B      PIC S9(04)  COMP.
           10 NI-FVDUM-PRAXIS-B     PIC S9(04)  COMP.
           10 NI-FVDUM-STEUER-NR    PIC S9(04)  COMP.
           10 NI-FVDUM-KTO-NR       PIC S9(04)  COMP.
           10 NI-FVDUM-BLZ          PIC S9(04)  COMP.
           10 NI-FVDUM-BANK         PIC S9(04)  COMP.
           10 NI-FVDUM-GEB-DAT      PIC S9(04)  COMP.
           10 NI-FVDUM-EIN-DAT      PIC S9(04)  COMP.
           10 NI-FVDUM-UMSTEIG-KZ   PIC S9(04)  COMP.
           10 NI-FVDUM-EIGENREG-KZ  PIC S9(04)  COMP.
        05  DCL-VIVB0112.
           10 FVDUM-AM-AG-NR       PIC S9(09) USAGE COMP.
           10 FVDUM-LFD-NR         PIC S9(04) USAGE COMP.
           10 FVDUM-NACHNAME       PIC X(40).
           10 FVDUM-VORNAME        PIC X(20).
           10 KTY-ANREDE           PIC S9(04) USAGE COMP.
           10 KNR-ANREDE           PIC S9(04) USAGE COMP.
           10 FVDUM-AD-GES-TITEL   PIC X(10).
           10 FVDUM-BL-KZ          PIC X(01).
           10 FVDUM-EU-REG-NR      PIC X(30).
           10 FVDUM-PLZ            PIC X(09).
           10 FVDUM-ORT            PIC X(40).
           10 FVDUM-STR            PIC X(40).
           10 FVDUM-INT-AG-NR      PIC S9(04) USAGE COMP.
           10 FVDUM-NAME           PIC X(40).
           10 FVDUM-DIR            PIC S9(04) USAGE COMP.
           10 FVDUM-MSNR           PIC S9(04) USAGE COMP.
           10 FVDUM-STUFE          PIC X(30).
           10 FVDUM-PRAXIS         PIC S9(04) USAGE COMP.
           10 FVDUM-AMAG-NR-B      PIC S9(09) USAGE COMP.
           10 FVDUM-NAME-B         PIC X(40).
           10 FVDUM-STUFE-B        PIC X(30).
           10 FVDUM-PRAXIS-B       PIC S9(04) USAGE COMP.
           10 FVDUM-STEUER-NR      PIC X(20).
           10 FVDUM-KTO-NR         PIC X(20).
           10 FVDUM-BLZ            PIC X(11).
           10 FVDUM-BANK           PIC X(40).
           10 FVDUM-GEB-DAT        PIC X(10).
           10 FVDUM-EIN-DAT        PIC X(10).
           10 FVDUM-UMSTEIG-KZ     PIC X(01).
           10 FVDUM-EIGENREG-KZ    PIC X(01).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 30      *
      ******************************************************************
