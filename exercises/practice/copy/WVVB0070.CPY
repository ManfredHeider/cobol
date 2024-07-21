      *---------------------------------------------------------------*
      *    STANDARD F‹R ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0070.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0070.CPv  $
      *
      *   Rev 2.1   Feb 26 2015 15:40:18   t13ae35
      *ARW00102: Ausbilderzuschuﬂ monatlich automatisch verbuchen -
      * Zugriffsmodule
      *
      *   Rev 2.0   Feb 27 1999 15:59:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   30 Jan 1996 11:50:28   P13SP99
      *PVCS-Keywords angepaﬂt
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0070                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH F‹R VIVB0070 (AUSBIF  )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.HERING                                         *
      *    DATUM:    26.02.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ARW00102!26.02.15!2.1 ! generiert mit PGAT0027    ! J.HERING  *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION F‹R VIEW VIVB0070                           *
      *****************************************************************
 
        05 WB-NI-VIVB0070-TAB.
           10 WB-NI-VIVB0070-ELE   PIC S9(04)  BINARY OCCURS  13.
        05 WB-NI-VIVB0070          REDEFINES   WB-NI-VIVB0070-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-AUSBIF-AZUBI          PIC S9(04)       BINARY.
           10 WB-NI-KTY-AUSBIF-TYP        PIC S9(04)       BINARY.
           10 WB-NI-KNR-AUSBIF-TYP        PIC S9(04)       BINARY.
           10 WB-NI-AUSBIF-DAT-ANSPRU     PIC S9(04)       BINARY.
           10 WB-NI-AUSBIF-DAT-VON        PIC S9(04)       BINARY.
           10 WB-NI-AUSBIF-DAT-BIS        PIC S9(04)       BINARY.
           10 WB-NI-AUSBIF-LAST-UPDATE    PIC S9(04)       BINARY.
           10 WB-NI-AUSBIF-ANSPR-BETR     PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.
 
        05 WB-VIVB0070.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-AUSBIF-AZUBI             PIC S9(09)       BINARY.
           10 WB-KTY-AUSBIF-TYP           PIC S9(04)       BINARY.
           10 WB-KNR-AUSBIF-TYP           PIC S9(04)       BINARY.
           10 WB-AUSBIF-DAT-ANSPRU        PIC  X(10).
           10 WB-AUSBIF-DAT-VON           PIC  X(10).
           10 WB-AUSBIF-DAT-BIS           PIC  X(10).
           10 WB-AUSBIF-LAST-UPDATE       PIC  X(26).
           10 WB-AUSBIF-ANSPR-BETR        PIC S9(09)V9(02) COMP-3.
           10 WB-KTY-WAEHRUNG             PIC S9(09)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(09)       BINARY.
           10 WB-KTY-ANSPR-STATUS         PIC S9(09)       BINARY.
           10 WB-KNR-ANSPR-STATUS         PIC S9(09)       BINARY.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  13                    *
      *****************************************************************

