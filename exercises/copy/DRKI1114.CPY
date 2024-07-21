      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1114.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1114.CPv  $
      *
      *   Rev 2.4   Sep 19 2017 10:03:42   t13ex39
      *AKI01542: Erweitern Deuba Kreditkarten-DTA
      *
      *   Rev 2.3   Mar 09 2016 14:44:00   t13ex73
      *AAT00049: Fehlerverfahren mit Prozentkriterien - G48 PGMs
      *
      *   Rev 2.2   Aug 11 2004 14:41:56   t13ex39
      *AKI00249: Änderungen/Ergänzungen zum Datenspiegel der Deutsc
      *hen Bank   SERVICEKARTEN
      *
      *   Rev 2.1   Jul 13 2003 23:38:12   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.0   07 Jun 2000 17:08:08   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die Eingabedatei Änderungsbestand           *
      *                Kartendaten von der Deutschen Bank              *
      *                                                                *
      *    SATZLÄNGE 100                                               *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00327!21.01.03! Neuerstellung                    !Golücke    *
      *----------------------------------------------------------------*
      *AKI00249!14.07.04!      Löschkennzeichen 'D' ver-   !WHO        *
      *        !        !      arbeiten                    !           *
      *----------------------------------------------------------------*
      *AAT00049!03.03.16! Umbau auf Replacing by XX        !DDY        *
      *        !        ! - ohne Vermerk in AREA A -       !           *
      *----------------------------------------------------------------*
010917*AKI01542!01.09.17! Aufhname PIN                     !WHO        *
      *        !        !                                  !           *
      *        !        !                                  !           *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1114.
      *
      ******************************************************************
      *                                                                *
      *                 VORSATZ                                        *
      *                                                                *
      ******************************************************************
      *
           10  'XX'-VORSATZ.
               15  'XX'-V-SA                      PIC X(002).
      *            SATZART = '01'
               15  'XX'-V-ABSENDER                PIC X(008).
      *            ABSENDER = 'DB AG'
               15  'XX'-V-DATUM                   PIC 9(008).
      *            Buchungsdatum = 'JJJJMMTT'
               15  'XX'-V-EMPFAENGER              PIC X(004).
      *            EMPFAENGER = 'DVAG'
               15  'XX'-V-TEXT                    PIC X(008).
      *            INHALTSANGABE:
      *            Komplettlieferung 'KARTEK'
      *
               15  'XX'-V-WKZ                     PIC X(003).
      *            WÄHRUNGSKENNZEICHEN = 'EUR'
               15  FILLER                         PIC X(67).
      *            RESERVE
      ******************************************************************
      *                                                                *
      *         HAUPTSATZ                                              *
      *                                                                *
      ******************************************************************
      *
           10  'XX'-SATZ   REDEFINES      'XX'-VORSATZ.
      *
               15  'XX'-SA                        PIC  X(002).
      *            SATZART        = '11'
               15  'XX'-GES                       PIC  9(002).
      *            GESELLSCHAFT   = '48'
               15  'XX'-KEY.
                   20 'XX'-VNR.
                      25 'XX'-FILIALNUMMER        PIC  9(003).
                      25 'XX'-KUNDENNUMMER        PIC  9(007).
                   20 'XX'-UNTERKONTONUMMER       PIC  9(002).
                   20 'XX'-WAEHRUNG               PIC  9(003).
      *               Währung ist immer Euro (000)
                   20 'XX'-KARTENART              PIC  X(003).
      *               EMP - EC-Karte mit PIN
      *               EOP - EC-Karte ohne PIN
      *               BMP - Kundenkarte mit PIN
      *               BOP - Kundenkarte ohne PIN
      *               JMP - Motivkundenkarte mit PIN
      *               JOP - Motivkundenkarte ohne PIN
      *
               15  'XX'-ANZAHL-AKTIVE-KARTEN      PIC  9(003).
      *            Anzahl der aktiven Karten je Kartenart
      *            Addition aller Karten mit Kartenstatus = "AKT" je
      *            Kartenart, wenn Anzahl grösser Null Satz übertragen.
      *
140704         15  'XX'-DEL-KZ                    PIC  X(001).
140704*            Delete-Kennzeichen
010917         15  'XX'-PIN                       PIC 9(009).

140704         15  'XX'-RESERVE                   PIC  X(065).
      *            RESERVE
      *
      ******************************************************************
      *                                                                *
      *          NACHSATZ                                              *
      *                                                                *
      ******************************************************************
      *
           10  'XX'-NACHSATZ  REDEFINES     'XX'-VORSATZ.
               15  'XX'-N-SA                      PIC  X(002).
      *            SATZART '99'
               15  'XX'-N-ANZ                     PIC  9(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               15  FILLER                         PIC  X(091).



