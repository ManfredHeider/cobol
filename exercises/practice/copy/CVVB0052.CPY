
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0052.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0052.CPv  $
      *
      *   Rev 2.1   Jul 08 2015 12:52:28   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - BVSP, EHNPB
      *
      *   Rev 2.0   Feb 27 1999 15:56:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:21:44   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*

      *****************************************************************
      *    ERSTELLT: WALB                                             *
      *    DATUM:    08.07.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01332!08.07.15!2.1 ! NEU                       ! WALB      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0052) EHNPB                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0052)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0052 TABLE
CCC004*    ( EHNPB_VS_ZEITRAUM           SMALLINT NOT NULL
CCC004*    , EHNPB_BETR                  INTEGER NOT NULL
CCC004*    , EHNPB_UNTST                 INTEGER NOT NULL
CCC004*    , EHNPB_EBENE                 CHAR(01) NOT NULL
CCC004*    , EHNPB_DAT_VON               DATE NOT NULL
CCC004*    , EHNPB_DAT_BIS               DATE NOT NULL
CCC004*    , EHNPB_ERSTELL_DAT           DATE NOT NULL
CCC004*    , EHNPB_EH_NEU_EIG            DECIMAL(10, 1) NOT NULL
CCC004*    , EHNPB_NP_VON                DATE NOT NULL
CCC004*    , EHNPB_NP_BIS                DATE NOT NULL
CCC004*    , EHNPB_ABZPROZ_KL            DECIMAL(05, 2)
CCC004*    , EHNPB_EH_SOLLANT            DECIMAL(10, 1)
CCC004*    , EHNPB_EH_ABZUG_KL           DECIMAL(10, 1)
CCC004*    , EHNPB_BESTUEB_R_K           CHAR(01) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0052                           *
      *****************************************************************

       01  VIVB0052-GESAMT.
        05 NI-VIVB0052-TAB.
           10 NI-VIVB0052-ELE      PIC S9(04)  BINARY OCCURS  14.
        05 NI-VIVB0052             REDEFINES   NI-VIVB0052-TAB.
           10 NI-EHNPB-VS-ZEITRAUM        PIC S9(04)       BINARY.
           10 NI-EHNPB-BETR               PIC S9(04)       BINARY.
           10 NI-EHNPB-UNTST              PIC S9(04)       BINARY.
           10 NI-EHNPB-EBENE              PIC S9(04)       BINARY.
           10 NI-EHNPB-DAT-VON            PIC S9(04)       BINARY.
           10 NI-EHNPB-DAT-BIS            PIC S9(04)       BINARY.
           10 NI-EHNPB-ERSTELL-DAT        PIC S9(04)       BINARY.
           10 NI-EHNPB-EH-NEU-EIG         PIC S9(04)       BINARY.
           10 NI-EHNPB-NP-VON             PIC S9(04)       BINARY.
           10 NI-EHNPB-NP-BIS             PIC S9(04)       BINARY.
           10 NI-EHNPB-ABZPROZ-KL         PIC S9(04)       BINARY.
           10 NI-EHNPB-EH-SOLLANT         PIC S9(04)       BINARY.
           10 NI-EHNPB-EH-ABZUG-KL        PIC S9(04)       BINARY.
           10 NI-EHNPB-BESTUEB-R-K        PIC S9(04)       BINARY.

        05 DCL-VIVB0052.
           10 EHNPB-VS-ZEITRAUM           PIC S9(04)       BINARY.
           10 EHNPB-BETR                  PIC S9(09)       BINARY.
           10 EHNPB-UNTST                 PIC S9(09)       BINARY.
           10 EHNPB-EBENE                 PIC  X(01).
           10 EHNPB-DAT-VON               PIC  X(10).
           10 EHNPB-DAT-BIS               PIC  X(10).
           10 EHNPB-ERSTELL-DAT           PIC  X(10).
           10 EHNPB-EH-NEU-EIG            PIC S9(09)V9(01) COMP-3.
           10 EHNPB-NP-VON                PIC  X(10).
           10 EHNPB-NP-BIS                PIC  X(10).
           10 EHNPB-ABZPROZ-KL            PIC S9(03)V9(02) COMP-3.
           10 EHNPB-EH-SOLLANT            PIC S9(09)V9(01) COMP-3.
           10 EHNPB-EH-ABZUG-KL           PIC S9(09)V9(01) COMP-3.
           10 EHNPB-BESTUEB-R-K           PIC  X(01).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  14                    *
      *****************************************************************

