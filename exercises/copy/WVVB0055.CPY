      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0055.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0055.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:28:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010001
      *    COPY-MEMBER: WVVB0055                                      * 00020001
      *    --------------------------------------------------------   * 00030001
      *    ERSTELLT: M. KUHN                                          * 00040001
      *    DATUM:    10.11.1993                                       * 00050001
      ***************************************************************** 00060001
      *    VERSION   02   20.01.94                                   *  00070003
      ****************************************************************  00080001
      * AENDERUNGEN:                                                 *  00090001
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00100001
      * -------------------------------------------------------------*  00110001
      *         !10.11.93! 01 ! NEU                       ! KUHN     *  00120001
      * AFVB0061!10.01.94!    ! ALLE 'AUN' NACH 'MAST' UM-! SCHÖLLH. *  00130001
      *         !        !    !   BENANNT; AUN-STATUS GEL.!          *  00140001
      *         !20.01.94! 02 ! PROD.UEBERGABE            ! HECKER   *  00141002
      *         !        !    !                           !          *  00150001
      *         !        !    !                           !          *  00160001
      *         !        !    !                           !          *  00170001
      *                                                              *  00180001
      * ENDE-LOGBUCH                                                 *  00190001
      ****************************************************************  00200001
      *                                                                 00210001
100194******************************************************************00220001
100194* COBOL DECLARATION FOR TABLE VIVB0055                           *00230001
100194******************************************************************00240001
100194* 05  WB-NI-VIVB0055-TAB.                                         00250001
100194*    10  WB-NI-VIVB0055-ELE  PIC S9(04)  COMP   OCCURS   7 TIMES. 00260001
100194* 05  WB-NI-VIVB0055         REDEFINES   WB-NI-VIVB0055-TAB.      00270001
100194*    10 WB-NI-MA-ST-NR          PIC S9(04)  COMP.                 00280001
100194*    10 WB-NI-AUN-DAT-VON       PIC S9(04)  COMP.                 00290001
100194*    10 WB-NI-KTY-AUN-TYP       PIC S9(04)  COMP.                 00300001
100194*    10 WB-NI-KNR-AUN-TYP       PIC S9(04)  COMP.                 00310001
100194*    10 WB-NI-AUN-LAST-UPDATE   PIC S9(04)  COMP.                 00320001
100194*    10 WB-NI-AUN-STATUS        PIC S9(04)  COMP.                 00330001
100194*    10 WB-NI-AUN-DAT-BIS       PIC S9(04)  COMP.                 00340001
100194* 05  WB-VIVB0055.                                                00350001
100194*    10 WB-MA-ST-NR          PIC S9(9) USAGE COMP     VALUE ZERO. 00360001
100194*    10 WB-AUN-DAT-VON       PIC X(26)                VALUE SPACE.00370001
100194*    10 WB-KTY-AUN-TYP       PIC S9(4) USAGE COMP     VALUE ZERO. 00380001
100194*    10 WB-KNR-AUN-TYP       PIC S9(4) USAGE COMP     VALUE ZERO. 00390001
100194*    10 WB-AUN-LAST-UPDATE   PIC X(26)                VALUE SPACE.00400001
100194*    10 WB-AUN-STATUS        PIC X(1)                 VALUE SPACE.00410001
100194*    10 WB-AUN-DAT-BIS       PIC X(10)                VALUE SPACE.00420001
100194******************************************************************00430001
100194* THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *00440001
100194******************************************************************00450001
                                                                        00460001
100194******************************************************************00470001
100194* COBOL DECLARATION FOR TABLE VIVB0055                           *00480001
100194******************************************************************00490001
100194  05  WB-NI-VIVB0055-TAB.                                         00500001
100194     10  WB-NI-VIVB0055-ELE  PIC S9(04)  COMP   OCCURS   6 TIMES. 00510001
100194  05  WB-NI-VIVB0055         REDEFINES   WB-NI-VIVB0055-TAB.      00520001
100194     10 WB-NI-MA-ST-NR          PIC S9(04)  COMP.                 00530001
100194     10 WB-NI-MAST-DAT-VON      PIC S9(04)  COMP.                 00540001
100194     10 WB-NI-KTY-MAST-TYP      PIC S9(04)  COMP.                 00550001
100194     10 WB-NI-KNR-MAST-TYP      PIC S9(04)  COMP.                 00560001
100194     10 WB-NI-MAST-LAST-UPDATE  PIC S9(04)  COMP.                 00570001
100194     10 WB-NI-MAST-DAT-BIS      PIC S9(04)  COMP.                 00580001
100194  05  WB-VIVB0055.                                                00590001
100194     10 WB-MA-ST-NR          PIC S9(9) USAGE COMP     VALUE ZERO. 00600001
100194     10 WB-MAST-DAT-VON      PIC X(26)                VALUE SPACE.00610001
100194     10 WB-KTY-MAST-TYP      PIC S9(4) USAGE COMP     VALUE ZERO. 00620001
100194     10 WB-KNR-MAST-TYP      PIC S9(4) USAGE COMP     VALUE ZERO. 00630001
100194     10 WB-MAST-LAST-UPDATE  PIC X(26)                VALUE SPACE.00640001
100194     10 WB-MAST-DAT-BIS      PIC X(10)                VALUE SPACE.00650001
100194******************************************************************00660001
100194* THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *00670001
100194******************************************************************00680001
