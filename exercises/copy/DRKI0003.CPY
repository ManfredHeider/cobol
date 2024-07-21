      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0003.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0003.CPv  $
      *
      *   Rev 2.3   Jan 17 2017 07:22:40   t13ex39
      *AKI01410: FodB Goldsparplan  Preise
      *
      *   Rev 2.2   Dec 19 2016 10:52:42   t13ex39
      *AKI01410: FodB Goldsparplan
      *
      *   Rev 2.1   11 Jan 2008 11:06:18   t13ex12
      *AKI00693: Neue Preisdatei von AGI
      *
      *   Rev 1.1   19 May 1998 09:32:48   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:39:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR NEUE RUECKNAHMEPREISE DIT (DTA)             *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEINAME = KIDITPR                                         *
      *    LAENGE    = 40 BYTES
020108*    LAENGE    = 80 BYTES
      *                                                                *
      *    DIE DATEI KOMMT PER ISDN-LEITUNG VON DIT TÄGLICH ZU UNS     *
      *    UND ENTHÄLT DIE RÜCKNAHMEPREISE DER EINZELNEN FONDS.        *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *16.07.97!NEUERSTELLUNG                           !A.CECCONI! 01 *
      *KI-NEU  !                                        !         !    *
      *--------!----------------------------------------!--------_!----*
      *02.01.08!Änderungen wegen ISIN, neu ist auch ein !v.d.Goltz! 02 *
      *AKI00693!Datum für den Stand der Preisen.        !         !    *
      *        !Währungs-KZ wurde immer geliefert, war  !         !    *
      *        !aber nicht hier definiert.              !         !    *
      *        !Preise mit 4 Vorkommastellen.           !         !    *
      *--------!----------------------------------------!---------!----*
      *16.11.16!Preis-Felder vergrößert wg. GS          !who      ! 03 *
      *AKI01410!                                        !         !    *
      *        !                                        !         !    *
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *
CCC009*    SKIP1
       01  DRKI0003.
           10  DIT-VORSATZ.
      *
               20  DIT-V-SA            PIC X(001).
      *            SATZART = 'A'
               20  DIT-V-ABSENDER      PIC X(008).
      *            ABSENDER = 'DIT,HOF'
               20  DIT-V-DATUM         PIC 9(008).
      *            BUCHUNGSDATUM = 'JJJJMMTT'
               20  DIT-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER = 'DVAG'
               20  DIT-V-TEXT          PIC X(010).
      *            INHALTSANGABE = 'KUNDEN,PRE'
020108         20  FILLER              PIC X(049).
020108*        20  FILLER              PIC  X(09).

      *
           10  DIT-SATZ     REDEFINES      DIT-VORSATZ.
      *
               20  DIT-SA                  PIC  X(001).
      *            SATZART        = 'C'
               20  DIT-GES                 PIC  9(002).
      *            GESELLSCHAFT   = '40'
               20  DIT-WP-KENN-NR          PIC  X(006).
      *            WERTPAPIERKENN-NUMMER
020108         20  DIT-ISIN-NR             PIC  X(012).
      *            Internationale Wertpapierkennr.
020108*        20  DIT-RUECK-PREIS         PIC  9(003)V99.
161216*        20  DIT-RUECK-PREIS         PIC  9(004)V99.
161216         20  DIT-RUECK-PREIS         PIC  9(006)V99.
      *            RUECKNAHMEPREIS
020108*        20  DIT-AUS-PREIS           PIC  9(03)V99.
161216*        20  DIT-AUS-PREIS           PIC  9(004)V99.
161216         20  DIT-AUS-PREIS           PIC  9(006)V99.
      *            AUSGABEPREIS
               20  DIT-WERT-ENTW           PIC S9(003)V99.
      *            WERTENTWICKLUNG
020108         20  DIT-WERT-ENTW-VORZ      PIC  X(001).
      *            Vorzeichen WERTENTWICKLUNG
020108*        20  DIT-WERT-ENTW-DAT       PIC  9(008).
      *            DATUM WERTENTWICKLUNG
020108         20  DIT-PREIS-DAT           PIC  9(008).
      *            DATUM der Preisen

020108         20  DIT-WAEHRUNGS-KZ        PIC  X(003).
      *            Währungs-Kennz. = 'EUR'
020108         20  DIT-WERT-ENTW-DAT       PIC  9(008).
      *            DATUM WERTENTWICKLUNG
161216         20  FILLER                  PIC  X(018).
161216*        20  FILLER                  PIC  X(022).
      *            RESERVE

      *
           10  DIT-NACHSATZ    REDEFINES     DIT-VORSATZ.
      *
               20  DIT-N-SA            PIC X(001).
      *            SATZART             'E'
               20  DIT-N-ANZ           PIC 9(009).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
020108         20  FILLER              PIC X(070).
020108*        20  FILLER              PIC X(030).
      *            RESERVE


