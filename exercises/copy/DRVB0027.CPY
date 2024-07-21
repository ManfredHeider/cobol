      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0027.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0027.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:08:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010009
      *    COPY-MEMBER: DRVB0027                                      * 00020009
      *    --------------------------------------------------------   * 00030009
      *    SCHNITTSTELLE UPVB0015 DIRWECHSEL                          * 00040009
      *    --------------------------------------------------------   * 00041009
      *    ERSTELLT: GIESE                                            * 00042209
      *    DATUM:    24.04.1993                                       * 00043009
      ***************************************************************** 00045009
      *    VERSION   02   15.10.93                                   *  00045116
      ****************************************************************  00045214
      * AENDERUNGEN:                                                 *  00045314
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00045414
      * -------------------------------------------------------------*  00045514
      *         !16.09.93! 01 ! NEU                       ! GIESE    *  00045614
      *         !07.10.93!    ! NEUE FUNKTION             ! GIESE    *  00045715
      *         !15.10.93! 02 ! PROD.ÜBERGABE             ! HECKER   *  00045816
      *         !        !    !                           !          *  00045914
      *         !        !    !                           !          *  00046014
      *         !        !    !                           !          *  00046114
      *         !        !    !                           !          *  00046214
      *         !        !    !                           !          *  00046314
      *         !        !    !                           !          *  00046414
      *         !        !    !                           !          *  00046514
      *                                                              *  00046614
      * ENDE-LOGBUCH                                                 *  00046714
      ****************************************************************  00046814
      *                                                                 00046909
      *   PI-UPVB0015-MA-ST-NR       DER MA, FÜR DEN DIRWECHSEL ODER    00047010
      *                              NEUDIREKTION  DURCHGEFÜHRT WIRD    00047110
      *                                                                 00047210
      *   PI-UPVB0015-FKT                                               00047315
      *                                                                 00047410
      *   PO-UPVB0015-RETCODE        0 --> OK                           00047510
      *                              1 --> PARAMTER FEHLER              00047610
      *                              2 --> FEHLER BEI SCHREIBMODUL      00047710
      *   PO-UPVB0015-PR-PARAMETER   RÜCKGABE-BEREICH VON SCHREIBMODUL  00047810
      *                                                                 00047910
       01  PI-UPVB0015.                                                 00048009
           05 PI-UPVB0015-MA-ST-NR           PIC S9(09) COMP.           00048109
           05 PI-UPVB0015-ST-TYP             PIC S9(04) COMP.           00048209
           05 PI-UPVB0015-MA-DIR-NR          PIC S9(04) COMP.           00048311
           05 PI-UPVB0015-FKT                PIC  X(02).                00048410
               88  UPVB15-FKT-NEUDIR                       VALUE 'ND'.  00048510
               88  UPVB15-FKT-DIRWECHSEL                   VALUE 'DW'.  00048610
071093         88  UPVB15-FKT-DIR-X-AUSGESCHIEDEN          VALUE 'DA'.  00048715
           05 PI-UPVB0015-LAST-UPDATE-DZ     PIC  X(26).                00049009
           05 PI-UPVB0015-CICS-TIMESTAMP     PIC  X(26).                00049112
           05 PI-UPVB0015-USER-ID            PIC  X(08).                00049209
           05 PI-UPVB0015-TERMINAL-ID        PIC  X(08).                00049309
      *                                                                 00049409
       01  PO-UPVB0015.                                                 00049510
           05  PO-UPVB0015-RETCODE           PIC  9(03).                00049610
           05  PO-UPVB0015-PR-PARAMETER      PIC  X(82).                00049710
      *                                                                 00049810
      ******** ENDE COPY-MEMBER (DRVB0027) *******************          00070009
