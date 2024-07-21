      *---------------------------------------------------------------*
      *    STANDARD F‹R ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0070.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0070.CPv  $
      *
      *   Rev 2.1   Feb 26 2015 15:40:14   t13ae35
      *ARW00102: Ausbilderzuschuﬂ monatlich automatisch verbuchen -
      * Zugriffsmodule
      *
      *   Rev 2.0   Feb 27 1999 15:56:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   30 Jan 1996 11:47:14   P13SP99
      *PVCS-Keywords angepaﬂt
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: J.HERING                                         *
      *    DATUM:    26.02.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ARW00102!26.02.15!2.1 ! generiert mit PGAT0027    ! J.HERING  *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0070) AUSBIF                                 *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0070)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0070 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , AUSBIF_AZUBI                INTEGER NOT NULL
CCC004*    , KTY_AUSBIF_TYP              SMALLINT NOT NULL
CCC004*    , KNR_AUSBIF_TYP              SMALLINT NOT NULL
CCC004*    , AUSBIF_DAT_ANSPRU           DATE NOT NULL
CCC004*    , AUSBIF_DAT_VON              DATE NOT NULL
CCC004*    , AUSBIF_DAT_BIS              DATE NOT NULL
CCC004*    , AUSBIF_LAST_UPDATE          TIMESTAMP NOT NULL
CCC004*    , AUSBIF_ANSPR_BETR           DECIMAL(11, 2) NOT NULL
CCC004*    , KTY_WAEHRUNG                INTEGER NOT NULL
CCC004*    , KNR_WAEHRUNG                INTEGER NOT NULL
CCC004*    , KTY_ANSPR_STATUS            INTEGER NOT NULL
CCC004*    , KNR_ANSPR_STATUS            INTEGER NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION F‹R VIEW VIVB0070                           *
      *****************************************************************
 
       01  VIVB0070-GESAMT.
        05 NI-VIVB0070-TAB.
           10 NI-VIVB0070-ELE      PIC S9(04)  BINARY OCCURS  13.
        05 NI-VIVB0070             REDEFINES   NI-VIVB0070-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-AUSBIF-AZUBI             PIC S9(04)       BINARY.
           10 NI-KTY-AUSBIF-TYP           PIC S9(04)       BINARY.
           10 NI-KNR-AUSBIF-TYP           PIC S9(04)       BINARY.
           10 NI-AUSBIF-DAT-ANSPRU        PIC S9(04)       BINARY.
           10 NI-AUSBIF-DAT-VON           PIC S9(04)       BINARY.
           10 NI-AUSBIF-DAT-BIS           PIC S9(04)       BINARY.
           10 NI-AUSBIF-LAST-UPDATE       PIC S9(04)       BINARY.
           10 NI-AUSBIF-ANSPR-BETR        PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.
 
        05 DCL-VIVB0070.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 AUSBIF-AZUBI                PIC S9(09)       BINARY.
           10 KTY-AUSBIF-TYP              PIC S9(04)       BINARY.
           10 KNR-AUSBIF-TYP              PIC S9(04)       BINARY.
           10 AUSBIF-DAT-ANSPRU           PIC  X(10).
           10 AUSBIF-DAT-VON              PIC  X(10).
           10 AUSBIF-DAT-BIS              PIC  X(10).
           10 AUSBIF-LAST-UPDATE          PIC  X(26).
           10 AUSBIF-ANSPR-BETR           PIC S9(09)V9(02) COMP-3.
           10 KTY-WAEHRUNG                PIC S9(09)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(09)       BINARY.
           10 KTY-ANSPR-STATUS            PIC S9(09)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(09)       BINARY.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  13                    *
      *****************************************************************

