      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0002.CPY  $




      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/DRKI0002.CPv  $
      *
      *   Rev 2.14   Jun 10 2014 14:45:14   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.13   Jun 10 2014 12:07:54   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.12   Jun 05 2014 13:57:38   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.11   Jun 05 2014 13:50:38   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.10   May 28 2014 10:33:54   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.9   May 28 2014 10:19:26   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.8   Feb 26 2014 16:53:42   t13ex15
      *FKI01447: AGI Bestands-DTA Ergänzung ISIN
      *
      *   Rev 2.7   Jun 15 2009 16:32:58   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik 
      *
      *   Rev 2.6   Jan 07 2008 16:13:24   t13ex50
      *AKI00693: Copystrecken DRKI0001 +  DRKI0002
      *
      *   Rev 2.5   Dec 19 2007 13:55:40   t13ex50
      *AKI00639: PremiumDepot: Datenspiegel und Programmanpassungen
      *
      *   Rev 2.4   Mar 07 2002 17:42:46   t13ex12
      *AKI00162 Neue Felder von AGI
      *
      *   Rev 2.3   28 Mar 2001 16:27:32   T13EX12
      *AKI00191 Prüfen Währungs-KZ
      *
      *   Rev 2.2   Mar 09 2000 13:10:34   T13EX12
      *AKI00069 Umstellung auf Euro
      *
      *   Rev 2.1   Apr 22 1999 13:31:54   T13EX39
      *FKI00018 Feld für  Kontoart
      *
      *   Rev 1.1   19 May 1998 09:32:46   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:39:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR BESTANDSÄNDERUNG AGI (DTA)                  *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEINAME = DTAAGIB                                         *
      *                                                                *
141101* Neu in 2001:                                                   *
141101*    SATZLÄNGE   400                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *16.07.97!NEUERSTELLUNG                           !A.CECCONI! 01 *
      *KI-NEU  !                                        !         !    *
      *--------!----------------------------------------!--------_!----*
      *19.04.99!Text für Kontoart definieren.           !v.d.Goltz! 02 *
      *FKI00018!                                        !         !    *
      *--------!----------------------------------------!--------_!----*
      *23.01.00!Verbr. FSA In EURO aufgenommen.         !v.d.Goltz! 03 *
      *AKI00069!                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *22.03.01! Währungs-KZ-Feld einfügen.             !v.d.Goltz!    *
      *AKI00191!                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *14.11.01!Neue Felder von AGI, wie z.B.           !v.d.Goltz! 04 *
      *AKI00162!Zwischengewinn, Betrag netto.           !         !    *
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *14.11.07!Neue Felder von AGI, GEBBETRAG,         !SCHLEICHER 2.5*
      *AKI00639!GEBSCHLUESSEL, GEBTEXT                  !         !    *
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *16.01.09!NEUE AUFBAU PRIMARYKEY                  !WHO      !2.6 *
      *--------!----------------------------------------!---------!----*
      *18.02.14!Umstellung wegen zusätzlicher ISIN      !SWZ      !2.7 *
      *--------!----------------------------------------!---------!----*
      *26.05.14!OIF neue Felder (RECL von 200 auf 400)  !SWZ      !2.8 *
      *--------!----------------------------------------!---------!----*
      *        !                                        !         !    *
      *        !                                        !         !    *
      *
CCC009*    SKIP1
       01  DRKI0002.
           10  G40-VORSATZ.
      *
               20  G40-V-SA            PIC X(001).
      *            SATZART = 'A'
               20  G40-V-ABSENDER      PIC X(008).
      *            ABSENDER = 'AGI,HOF'
               20  G40-V-DATUM         PIC 9(008).
      *            BUCHUNGSDATUM = 'JJJJMMTT'
               20  G40-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER = 'DVAG'
               20  G40-V-TEXT          PIC X(010).
      *            INHALTSANGABE:
141101*            Tägliche Lieferung normale Fonds     'BUCHDE'
141101*            Tägliche Lieferung VL                'BUCHVL'
141101*            Lieferung Ausschüttung normale Fonds 'BUCHDEAUS'
141101*            Lieferung Ausschüttung VL            'BUCHVLAUS'
141101*            Komplettbestand normale Fonds        'BUCHDEKPŸ
141101*            Komplettbestand VL                   'BUCHVLKPŸ

               20  G40-V-INHALT        PIC X(001).
      *            DATENSÄTZE ÜBERTRAGEN: JA = SPACE   NEIN = 'K'

220301*        20  FILLER              PIC X(098).

220301         20  G40-V-WKZ           PIC X(003).
      *            Währungs-Kennz. =  'EUR'
141101*        20  FILLER              PIC X(095).
141101         20  FILLER              PIC X(365).
      *            RESERVE

      *
           10  G40-SATZ     REDEFINES      G40-VORSATZ.
      *
               20  G40-SA                  PIC  X(001).
      *            SATZART        = 'C'
               20  G40-GES                 PIC  9(002).
      *            GESELLSCHAFT   = '40'
160109         20  G40-KEY.
>>                 25  G40-VNR                 PIC  9(015).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   25 FILLER REDEFINES G40-VNR.
                      30  G40-VNR-1-13         PIC  9(013).
                      30  G40-UNTER-NR         PIC  9(002).
      *            UNTERDEPOTNUMMER)
                   25  G40-WP-KENN-NR          PIC  X(06).
      *            WERTPAPIERKENN-NUMMER

180214*        hierher verschoben 18.02.2014 mit Einführung ISIN
<<             20  G40-KTO-ART             PIC  X(02).
160109*            Kontoart (DE, VL)

               20  G40-ANTEILE             PIC  9(06)V999.
      *            ANTEILE (BESTAND STÜCK)
               20  G40-BUCH-DAT            PIC  9(08).
      *            BUCHUNGSDATUM
               20  G40-BUCH-BETRAG         PIC  9(07)V99.
      *            BETRAG LETZTE BUCHUNG
               20  G40-BUCH-ST-BETRAG      PIC  9(07)V99.
      *            STEUERBETRAG LETZTE BUCHUNG
               20  G40-BUCH-SCHL           PIC  X(02).
      *            BUCHUNGSSCHLÜSSEL
               20  G40-EINZ-KUM            PIC  9(13).
      *            KUMULIERTE EINZAHLUNGEN
               20  G40-AUSZ-KUM            PIC  9(13).
      *            KUMULIERTE AUSZAHLUNGEN
140314         20  FILLER                  PIC  X(22).
...            20  G40-BUCH-ANTEILE        PIC  9(06)V999.
...   *            BUCHUNGSBESTAND STÜCKZAHL
...            20  G40-UMSATZ-NETTO        PIC  9(07)V99.
...   *            NETTOBETRAG = RÜCKNAHMEPREIS X ANTEILE
141107         20  G40-GEB-BETRAG          PIC  9(07)V99.
..!   *            GEBÜHRENBETRAG
..!            20  G40-GEB-SCHLUESSEL      PIC  9(02).
..!   *            GEBÜHRENSCHLÜSSEL
..!            20  G40-GEB-TEXT            PIC  X(20).
..!   *            GEBÜHRENTEXT
180214         20  G40-ISIN                PIC  X(12).
180214*            ISIN

      *        *****************************************************
      *        *   OIF/ANSFUG
      *        *****************************************************
140314         20  G40-ANZ-ANT-UN-RCK-GEK  PIC  9(07)V9999.
      *            Anzahl wegen unwiderruflicher Rückgabe gekündigte
      *            Anteile bei offenen Immobilienfonds
140314         20  G40-ANZ-ANT-ABL-MHF-ERR PIC  9(07)V9999.
      *            Anzahl Anteile OIF, Ablauf der MHF für Kündigung
      *            erreicht (12 Monate oder länger im Bestand des
      *            Kunden)
140314         20  G40-ANZ-ANT-ABL-MHF-N-E PIC  9(07)V9999.
      *            Anzahl Anteile OIF, Ablauf der MHF für Kündigung
      *            noch nicht erreicht (kürzer als 12 Monate im
      *            Bestand des Kunden)
140314         20  G40-ANZ-ANT-ERW-VOR-JU13 PIC  9(07)V9999.
      *            Anzahl Anteile OIF mit Erwerb vor dem 22.07.2013
      *            (Anzahl der Anteile, die im Rahmen des halbjährl.
      *            Kontingentes für vor dem 22.07.2013 erworbene
      *            Anteile ohne Kündigung zurückgegeben werden können)
140314         20  G40-EUR-BER-VERK-JU13 PIC  9(06)V99.
      *            Bereits ausgenutztes Halbjahres-Kontingent dieser
      *            WKN, zur Rückgabe von OIF ( Volumen in Euro, das im
      *            Rahmen des halbjährlichen Kontingentes dieser WKN
      *            für die Rückgabe von vor dem 22.07.2013 erworbene
      *            Anteile im aktuellen Kalenderhalbjahr bereits in
      *            Anspruch genommen wurdech genommen wurde)
140314         20  G40-ANLEGER-ID        PIC  X(14).
      *            Anleger-ID zu bereits ausgenutztem
      *            Halbjahres-Kontingent dieser WKN, zur Rückgabe
      *            von OIF
      *        *****************************************************
      *        *   Reserve
      *        *****************************************************
140314         20  FILLER                  PIC  X(162).
      *            RESERVE

      *
           10  G40-NACHSATZ    REDEFINES     G40-VORSATZ.
      *
               20  G40-N-SA            PIC X(001).
      *            SATZART             'E'
               20  G40-N-ANZ           PIC 9(009).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
141101*        20  FILLER              PIC X(120).
141101         20  FILLER              PIC X(390).
      *            RESERVE
