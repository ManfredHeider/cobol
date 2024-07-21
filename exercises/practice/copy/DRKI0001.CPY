      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0001.CPY  $                                 00010000




      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI0001.CPv  $
      *
      *   Rev 2.8   Jan 12 2021 15:39:16   u130684
      *AKI01775: Neues Ermittlungsprogramm Fondsdepot Bank
      *
      *   Rev 2.7   Oct 10 2013 10:52:28   t13ex15
      *AKI01204: SEPA - DB-Online AGI Programme und Copys
      *
      *   Rev 2.6   Aug 18 2008 14:46:32   t13ex50
      *FKI01178: Ursprüngl. Eröffnungsdaten aus dem alten AGI-Syste
      *m übernehm
      *
      *   Rev 2.5   Jan 07 2008 16:13:18   t13ex50
      *AKI00693: Copystrecken DRKI0001 +  DRKI0002
      *
      *   Rev 2.4   Dec 19 2007 14:44:04   t13ex50
      *AKI00611: dit-AufbauPlan Plus/VL: Datenspiegel und Maskenanp
      *assungen
      *
      *   Rev 2.3   Mar 07 2002 17:42:30   t13ex12
      *AKI00162 Neue Felder von DIT
      *
      *   Rev 2.2   31 Oct 2000 16:54:02   T13EX12
      *AKI00145 Risikoklasse
      *
      *   Rev 2.1   Mar 09 2000 10:59:46   T13EX39
      *AKI00069 WKZ
      *
      *   Rev 2.0   Feb 27 1999 12:52:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   19 May 1998 09:32:44   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:38:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER ZU DEN AUFBEREITUNGSPROGRAMMEN KI-NEU           *
      *            - ALLGEMEINE KUNDEN-/PERSONENDATEN DIT -            *
      *                                                                *
151101*    SATZLÄNGE   1264  (HAUPTSATZ OHNE FILLER: 1145)             *
151101* Neu in 2001:                                                   *
151101*    SATZLÄNGE   1600  (HAUPTSATZ OHNE FILLER: 1536)             *
241007* Neu in 2007:                                                   *
241007*    SATZLÄNGE   1100  (HAUPTSATZ OHNE FILLER: 1049)             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *07.07.97!NEUERSTELLUNG                           !A.CECCONI! 01 *
      *KI-NEU  !                                        !         !    *
      *--------!----------------------------------------!--------_!----*
      *17.12.99!WKZ aufgenommen u. FAS In EURO aufgen.  !WHO      ! 01 *
      *AKI00069!                                        !         !    *
      *--------!----------------------------------------!--------_!----*
      *06.09.00!Risikoklasse aufgenommen. WKZ entfernt. !v.d.Goltz! 03 *
      *AKI00145!Faxrevers aufgenommen.                  !         !    *
      *--------!----------------------------------------!--------_!----*
      *15.11.01!Neue Felder von DIT, wie z.B.           !v.d.Goltz! 04 *
      *AKI00162!Auszahlungen.                           !         !    *
      *--------!----------------------------------------!--------_!----*
      *24.10.07!SPARPLÄNE AUS DER SATZART ENTFERNT.    !SCHLEICHER! 04 *
      *AKI00611!LÄNGE NEU: 1100 BYTE                   !          !    *
      *--------!---------------------------------------!----------!----*
130808*13.08.08!NEUES FELD HIST. ERÖFFNUNGDATUM EIN-   !SCHLEICHER!2.6 *
130808*FKI01178! GESTELLT - FILLER ANGEPASST           !          !    *
130808*        !DIT-DYNAMISIERUNG ENTFERNT - STEHT IN  !          !    *
130808*        ! KEINER DOKU                           !          !    *
      *--------!---------------------------------------!----------!----*
230913*23.09.13!Anpassungen SEPA                       !SWZ       !2.7 *
      *--------!---------------------------------------!----------!----*
      *05.01.21! INHALTSANGABE FÜR G40-V-TEXT          ! U130684  !2.8 *
      *AKI01775! ANGEPASST                             !          !    *
      *--------!---------------------------------------!----------!----*
      *
CCC009*    SKIP1
       01  DRKI0001.

      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************

           10  G40-VORSATZ.
      *
               20  G40-V-SA            PIC X(001).
      *            SATZART = 'A'
               20  G40-V-ABSENDER      PIC X(008).
      *            ABSENDER = 'AGI,HOF'
               20  G40-V-DATUM         PIC 9(008).
      *            ERFASSUNGS-/AENDERUNGSDATUM = 'JJJJMMTT'
               20  G40-V-EMPFAENGER    PIC X(004).
      *            EMPFÄNGER = 'DVAG'
               20  G40-V-TEXT          PIC X(010).
      *            INHALTSANGABE:
      *            TÄGLICHE LIEFERUNG DVAG    'KUNDENFB  '
      *            TÄGLICHE LIEFERUNG ALLF    'KUNDENAF  '
      *            TÄGLICHE LIEFERUNG ALAG    'KUNDENAL  '
      *            KOMPLETTBESTAND    DVAG    'KUNDENFBKP'
      *            KOMPLETTBESTAND    ALLF    'KUNDENAFKP'
      *            KOMPLETTBESTAND    ALAG    'KUNDENALKP'
               20  G40-V-WKZ           PIC X(003).
      *            WÄHRUNGSKENNZEICHEN = 'EUR'
      *
151101*        20  FILLER              PIC X(1566).
241007         20  FILLER              PIC X(1066).
      *            RESERVE
      *
      ***********************************************************
      *                                                         *
      *         HAUPTSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  G40-SATZ     REDEFINES      G40-VORSATZ.
      *
151101       15  G40-ALTER-SATZ1.
               20  G40-SA              PIC  X(001).
      *            SATZART        = 'C'
               20  G40-GES             PIC  9(002).
      *            GESELLSCHAFT   = '40'
230913         20  G40-VNR             PIC  9(013).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               20  G40-VBNR            PIC  9(007).
      *            ABSCHLUSSVERMITTLER
230913*        20  G40-PERSTAB    OCCURS 4.
230913         20  G40-PERSTAB    OCCURS 2.
                   30  G40-ANREDE          PIC 9(001).
      *                ANREDE...........*1 : HERR
      *                ANREDE...........*2 : FRAU
      *                ANREDE...........*5 : FIRMA
      *                ANREDE...........*9 : KEINE ANREDE
                   30  G40-NNAME           PIC X(040).
      *                NACHNAME DES KONTOINHABERS
                   30  G40-VNAME           PIC X(020).
      *                VORNAME DES KONTOINHABERS
                   30  G40-STR             PIC X(040).
      *                STRASSE/POSTFACH
                   30  G40-AUSL-KZ         PIC X(003).
      *                AUSLANDSKENNZEICHEN
                   30  G40-PLZ             PIC X(009).
      *                POSTLEITZAHL
                   30  G40-ORT             PIC X(040).
      *                ORT
230913*            30  G40-TEL             PIC X(015).
      *                TELEFON-NR.
230913*            30  G40-FAX             PIC X(015).
      *                FAX-NR.
                   30  G40-GEBDAT          PIC 9(008).
      *                GEBURTSTAGSDATUM    JJJJMMTT
230313         20  G40-BANK                PIC X(020).
      *                BANKVERBINDUNG
230913*            30  G40-BLZ             PIC 9(011).
      *                BANKLEITZAHL
230913*            30  G40-KTO-NR          PIC 9(018).
      *                KONTO-NUMMER
               20  G40-BIC                 PIC X(011).
      *            BIC
               20  G40-IBAN                PIC X(034).
      *            IBAN
230913*        20  G40-VERFUEG-KZ          PIC  X(001).
      *            KENNZEICHEN VERFÜGUNG
               20  G40-NACHLASS-KZ         PIC  X(001).
      *            KENNZEICHEN NACHLASS
230913*        20  G40-FREI-BETRAG         PIC  9(05)V99.
      *            FREISTELLUNGSBETRAG GESAMT, in Euro
230913*        20  G40-FREI-ABLAUF-DAT     PIC  9(08).
      *            ABLAUFDATUM FREISTELLUNGSBETRAG
230913*        20  G40-ANZ-DEPOT           PIC  9(02).
      *            ANZAHL DEPOTS FÜR FREISTELLUNGSBETRAG
               20  G40-INT-VNR            PIC  9(09).
      *            INTERNE VERTRAGS-NR.
               20  G40-VBEGINN            PIC  9(08).
      *            DATUM VERTRAGSBEGINN
               20  G40-VBEGINN-KZ         PIC  X(01).
      *            N = NEUES DEPOT
      *            S = INFOSPERRE AUFGEHOBEN
      *            R = REAKTIVIERT
               20  G40-VABLAUF            PIC  9(08).
      *            DATUM VERTRAGSABLAUF
               20  G40-VABLAUF-KZ         PIC  X(01).
      *            L = DEPOTAUFLÖSUNG (VON KD BEANTRAGT)
      *            S = INFOSPERRE GESETZT
      *            D = PHYSISCH GELÖSCHT BEI AGI
230913*        20  G40-AEND-NAME-KZ       PIC  X(01).
      *            J = NAMENSAENDERUNG (NNAME)
230913*        20  G40-AEND-ADR-KZ        PIC  X(01).
      *            J = ADRESSAENDERUNG (STRASSE,AUSL-KZ,PLZ,ORT)
230913*        20  G40-AEND-KD-KZ         PIC  X(01).
      *            J = SONSTIGE KD-DATENAENDERUNG
230913*        20  G40-AEND-VERTRAG-KZ    PIC X(01).
      *            J = SONSTIGE VERTRAGDATEN-AENDERUNG
               20  G40-KTO-ART            PIC X(02).
      *            KONTOART (SPACE,'VL')
171299*        20  G40-FSA-EURO           PIC 9(05)V99.
171299*            FREISTELLUNGSBETRAG GESAMT IN EURO

060900         20  G40-RISIKOKLASSE       PIC 9(001).
...   *            Risikoklasse 0 - 9
230913*        20  G40-FAXREVERS          PIC X(001).
...   *            Faxrevers = 'J' oder 'N'

230913*        20  G40-TELREVERS          PIC X(001).
...   *            Telefonrevers = 'J' oder 'N'
230913*        20  G40-ONLINEREVERS       PIC X(001).
...   *            ONLINEREVERS = 'J' ODER 'N'
               20  G40-LETZT-AENDDAT      PIC  9(08).
      *            DATUM LETZTER DATENSATZAENDERUNG
130808         20  G40-HIST-VBEGINN       PIC  9(08).
130808*            HISTORISCHES VERTRAGSBEGINNDATUM
      *             WEGEN SYSTEMUMSTELLUNG AGI IST HIER DAS HIST.
      *             ERÖFF.DAT. AUS DEM ALTEN SYSTEM


130808*        20  G40-RESERVE            PIC X(043).
230913         20  G40-RESERVE            PIC X(643).
      *            RESERVE
      ***********************************************************
      *                                                         *
      *          NACHSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  G40-NACHSATZ    REDEFINES     G40-VORSATZ.
      *
               20  G40-N-SA            PIC X(001).
      *            SATZART             'E'
               20  G40-N-ANZ           PIC 9(007).
      *            ANZAHL SAETZE INKL. VOR- UND NACHSATZ
      *
151101*        20  FILLER              PIC X(1256).
...   *        20  FILLER              PIC X(1592).
241007         20  FILLER              PIC X(1092).
      *            RESERVE

