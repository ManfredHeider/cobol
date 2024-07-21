      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   US200S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\US200S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:01:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   09 Oct 1997 16:07:24   T13AE28
      *AFVB0368 - Erweiterung wg. Fuellen Anrede mit akt. Werten
      *
      *   Rev 1.2   Apr 10 1997 14:40:20   T13AE24
      *AFVB0349, Release Maßnahmen PA-Druck
      *
      *   Rev 1.1   Jun 14 1996 14:41:52   T13AE24
      *FEVB0329, Prüfziffer auf Deckblatt mit ausgeben
      *
      *   Rev 1.0   Nov 04 1995 09:43:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************  00001000
      *    COPY-MEMBER: US0200S                                      *  00002001
      *                 ALLG. FÜR DRUCKZUSAMMENFÜHRUNG               *  00002100
      *    --------------------------------------------------------  *  00002300
      *    ERSTELLT: P. SCHMIDT                                      *  00002400
      *    DATUM:                                                    *  00002500
      ****************************************************************  00002600
      *    VERSION   2    07.08.95 11:32:18
      ****************************************************************  00002800
      * AENDERUNGEN:                                                 *  00002900
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00003000
      * -------------------------------------------------------------*  00003100
      * AFVB0213!23.05.95! 02 ! DIM7 ENTFÄLLT, DAFÜR GIBT !          *  00004003
      *         !        !    ! ES DIE STORNO-BEGLEITLISTE!          *  00004003
      *         !        !    ! NEUE SATZLÄNGE 170 BYTE   ! SCHMIDT  *  00005000
      * FEVB0329!14.06.96! 03 ! NEUES FELD PRüFZIFFER     ! SCHMIDT  *  00006000
      * AFVB0349!25.03.97! 04 ! NEUES FELD AUSLAND        ! SCHMIDT  *  00007000
      *         !        !    !                           !          *  00008000
      *         !        !    !                           !          *  00008000
      *                                                              *  00009000
      * ENDE-LOGBUCH                                                 *  00010000
      ****************************************************************  00020000
      *                                                                 00020000
       01  ZW-SATZ.
           05  ZW-VBNR.
              10 ZW-VBNR-ANF           PIC X(03).
              10 FILLER                PIC X.
              10 ZW-VBNR-END           PIC X(04).
           05  ZW-VERARB               PIC X.
           05  ZW-ABRECH               PIC X.
           05  ZW-SEITEN-ABRECH        PIC 9(04).
           05  ZW-ZAB                  PIC X.
           05  ZW-SEITEN-ZAB           PIC 9(04).
           05  ZW-RUECK                PIC X.
           05  ZW-STAT                 PIC X.
           05  ZW-STBL-EG              PIC X.
           05  ZW-SEITEN-STBL-EG       PIC 9(02).
           05  ZW-STBL-GR              PIC X.
           05  ZW-SEITEN-STBL-GR       PIC 9(02).
           05  ZW-DECKBL               PIC X.
           05  ZW-ANREDE               PIC X(10).
           05  ZW-NAME                 PIC X(40).
           05  ZW-STRASSE              PIC X(30).
           05  ZW-ORT                  PIC X(30).
           05  ZW-BLATT                PIC 9(05).
           05  ZW-DIR                  PIC X(03).
           05  ZW-PRZ                  PIC X(01).
250397*    05  ZW-REST                 PIC X(23).
250397     05  ZW-AUSLAND              PIC X(17).
250397     05  ZW-SEITEN-GES           PIC 9(06).
280897     05  ZW-ANREDE-GESZEILE      PIC X(80).
280897     05  FILLER                  PIC X(80).
