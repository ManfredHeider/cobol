      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0041.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0041.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:11:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DRVB0041                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    SCHNITTSTELLE UPVB0040 ERMITTELN NÄCHSTE FREIE ST-NR       * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: HECKER                                           * 00060000
      *    DATUM:    11.07.1993                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    31.03.94 17:55:48
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00111000
      * -------------------------------------------------------------*  00112000
      *         !16.09.93! 01 ! NEU                       ! HECKER   *  00113000
      * AFVB0044!30.11.93!    ! AVBG-ERWEITERUNG UM GESELL! GIESE    *  00114000
      *         !30.03.94! 02 ! PROD.UEBERGABE            ! SCHÖLLH. *  00114102
      *         !        !    !                           !          *  00115000
      *         !        !    !                           !          *  00116000
      *         !        !    !                           !          *  00117000
      *         !        !    !                           !          *  00118000
      *         !        !    !                           !          *  00119000
      *         !        !    !                           !          *  00119100
      *         !        !    !                           !          *  00119200
      *                                                              *  00119300
      * ENDE-LOGBUCH                                                 *  00119400
      ****************************************************************  00119500
      *                                                                 00119600
      *   PI-UPVB0040-GESELL-TYP     DIE GESELLSCHAFT, FÜR DIE DIE      00119701
      *                              NÄCHSTE FREIE NUMMER GEHOLT WIRD   00119800
      *   PI-UPVB0040-ST-NR          DIE ST-NR, AB DER DIE NÄCHSTE FREIE00119900
      *                              STELLEN-NUMMER GESUCHT WERDEN SOLL 00120000
      *                              BEI 0 WIRD DIE ERSTE FREIE GENOMMEN00130000
      *                                                                 00140000
      *   PO-UPVB0040-RETCODE        0 --> OK                           00150000
      *                              1 --> KEINE FREIE ST-NR GEFUNDEN   00160000
      *                              2 --> FEHLER BEI SCHREIBMODUL      00170000
      *   PO-UPVB0040-ST-NR-FREI     GEFUNDENE FREIE STELLEN-NUMMER     00170100
      *   PO-UPVB0040-ST-PRZ-FREI    ZUGEHÖRIGE PRÜFZIFFER              00170200
      *   PO-UPVB0040-PR-PARAMETER   RÜCKGABE-BEREICH VON SCHREIBMODUL  00170300
      *                                                                 00170400
       01  PI-UPVB0040.                                                 00170500
301193     05  PI-UPVB0040-GESELL-TYP        PIC S9(04) COMP.           00170601
           05  PI-UPVB0040-ST-NR             PIC S9(09) COMP.           00170700
           05  PI-UPVB0040-DATE-AEND         PIC  X(10).                00170800
           05  PI-UPVB0040-DATE-VON          PIC  X(10).                00170900
           05  PI-UPVB0040-USER-ID           PIC  X(08).                00171000
           05  PI-UPVB0040-TERMINAL-ID       PIC  X(08).                00172000
      *                                                                 00173000
       01  PO-UPVB0040.                                                 00174000
           05  PO-UPVB0040-RETCODE           PIC  9(03).                00175000
           05  PO-UPVB0040-ST-NR-FREI        PIC S9(09) COMP.           00176000
           05  PO-UPVB0040-ST-PRZ-FREI       PIC  X(01).                00177000
           05  PO-UPVB0040-PR-PARAMETER      PIC  X(82).                00178000
      *                                                                 00179000
      ******** ENDE COPY-MEMBER (DRVB0041) *******************          00180000
