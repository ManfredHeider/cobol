      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0082.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0082.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   19 Jun 1998 15:34:00   T13AE34
      *AAT00006 - Konstantencopy
      *
      *   Rev 1.0   Nov 04 1995 08:49:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00001000
      *    COPY-MEMBER: DRAT0082   KNR-KONST. KTY-47 WÄHRUNGSTYP      * 00002000
      *    --------------------------------------------------------   * 00003000
      *    ERSTELLT: SCHMIDT                                          * 00006000
      *    DATUM:    12.10.1994                                       * 00007000
      ***************************************************************** 00009000
      *    VERSION   1    29.10.94 10:11:15
      ****************************************************************  00009200
      * AENDERUNGEN:                                                 *  00009300
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00009400
      * -------------------------------------------------------------*  00009500
      * AFVB0142!12.10.94! 01 ! NEU                       ! SCHMIDT  *  00009600
      *         !        !    !                           !          *  00009900
      * AFPA0012!30.03.98! 02 ! NEU: WAEHRUNG-UNBEKANNT   ! WOLFF    *  00010100
      * AAT00002!12.06.98! 03 ! NEU: WAEHRUNG-EURO        ! HEIDER   *  00010100
      *         !        !    !                           !          *  00010300
      *                                                              *  00010400
      * ENDE-LOGBUCH                                                 *  00010500
      ****************************************************************  00010600
      *                                                                 00011000
      *   SCHLÜSSELWERTE KTY-47 WÄHRUNGSTYP                             00020000
      *                                                                 00030000
       01  FILLER.                                                      00031000
           05 K-KNR-WAEHRUNG-DM                PIC S9(4) COMP VALUE 001.00032100
           05 K-KNR-WAEHRUNG-OES               PIC S9(4) COMP VALUE 002.00032100
           05 K-KNR-WAEHRUNG-EURO              PIC S9(4) COMP VALUE 003.00032100
300398     05 K-KNR-WAEHRUNG-UNBEKANNT         PIC S9(4) COMP VALUE 999.00032100
      *                                                                 00034200
      ***** ENDE COPY-MEMBER DRAT0082  KNR-KONST. WÄHRUNGSTYP     ******00035000
