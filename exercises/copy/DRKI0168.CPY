      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0168.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/DRKI0168.CPv  $
      *
      *   Rev 2.3   Nov 28 2002 08:39:34   t13ex49
      *AKI00294 - ISIN
      *
      *   Rev 2.2   31 Oct 2000 16:47:18   T13EX12
      *AKI00145 Risikoklasse
      *
      *   Rev 2.1   Mar 09 2000 10:59:52   T13EX39
      *AKI00069 WKZ
      *
      *   Rev 2.0   Feb 27 1999 12:53:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 17:51:10   T13AE38
      *AKI00014 - Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:54:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR BESTANDSÄNDERUNG BNK (DTA)   KOMPLETTBESTAND*
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEITYP  = KSDS                                            *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *09.06.98!NEUERSTELLUNG                           !S.Jost   ! 01 *
      *AKI00001!                                        !         !    *
      *--------!----------------------------------------!--------_!----*
      *17.12.99!WKZ aufgenommen + FSA in EURO aufgen.   !WHO      ! 01 *
      *AKI00069!                                        !         !    *
      *--------!----------------------------------------!--------_!----*
      *06.09.00!Risikoklasse aufgenommen. WKZ entfernt. !v.d.Goltz! 03 *
      *AKI00145!Faxrevers aufgenommen.                  !         !    *
      *--------!----------------------------------------!--------_!----*
      *        !                                        !         !    *
130902*13.09.02!Umstellung wg. ISIN                     !P. Pohl  !    *
130902*AKI00294!in diesem Zusammenhang                  !         !    *
130902*        !- KOMPL-BNK-FSA-EURO                    !         !    *
130902*        !- KOMPL-BNK-KTO-ART                     !         !    *
130902*        !durch Filler ersetzt                    !         !    *
130902*        !Ausserdem                               !         !    *
130902*        !- KOMPL-BNK-FONDTAB  OCCURS 5           !         !    *
130902*        !durch Filler ersatzt. Diese Daten werden!         !    *
130902*        !zukünftig in eigenem DTA geliefert      !         !    *
      *        !                                        !         !    *
      *        !                                        !         !    *
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *
       01  DRKI0168.
      *
               05  KOMPL-BNK-SA            PIC  X(001).
      *            SATZART        = 'C'
               05  KOMPL-BNK-GES           PIC  9(002).
      *            GESELLSCHAFT
      *
               05  KOMPL-BNK-KEY.
                   10 KOMPL-BNK-VNR           PIC  9(015).
      *               VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               05  KOMPL-BNK-VBNR      PIC  9(007).
      *            ABSCHLUSSVERMITTLER
               05  KOMPL-BNK-PERSTAB OCCURS 4.
                   10  KOMPL-BNK-ANREDE    PIC 9(001).
      *                ANREDE...........*1 : HERR
      *                ANREDE...........*2 : FRAU
      *                ANREDE...........*5 : FIRMA
      *                ANREDE...........*9 : KEINE ANREDE
                   10  KOMPL-BNK-NNAME     PIC X(040).
      *                NACHNAME DES KONTOINHABERS
                   10  KOMPL-BNK-VNAME     PIC X(020).
      *                VORNAME DES KONTOINHABERS
                   10  KOMPL-BNK-STR       PIC X(040).
      *                STRASSE/POSTFACH
                   10  KOMPL-BNK-AUSL-KZ   PIC X(003).
      *                AUSLANDSKENNZEICHEN
                   10  KOMPL-BNK-PLZ       PIC X(009).
      *                POSTLEITZAHL
                   10  KOMPL-BNK-ORT       PIC X(040).
      *                ORT
                   10  KOMPL-BNK-TEL       PIC X(015).
      *                TELEFON-NR.
                   10  KOMPL-BNK-FAX       PIC X(015).
      *                FAX-NR.
                   10  KOMPL-BNK-GEBDAT    PIC 9(008).
      *                GEBURTSTAGSDATUM    JJJJMMTT
                   10  KOMPL-BNK-BANK      PIC X(020).
      *                BANKVERBINDUNG
                   10  KOMPL-BNK-BLZ       PIC 9(011).
      *                BANKLEITZAHL
                   10  KOMPL-BNK-KTO-NR    PIC 9(018).
      *                KONTO-NUMMER
               05  KOMPL-BNK-VERFUEG-KZ PIC X(001).
      *            KENNZEICHEN VERFÜGUNG
               05  KOMPL-BNK-NACHLASS-KZ PIC X(001).
      *            KENNZEICHEN NACHLASS
               05  KOMPL-BNK-FREI-BETRAG PIC  9(05)V99.
      *            FREISTELLUNGSBETRAG GESAMT
               05  KOMPL-BNK-FREI-ABLAUF-DAT PIC 9(08).
      *            ABLAUFDATUM FREISTELLUNGSBETRAG
               05  KOMPL-BNK-ANZ-DEPOT   PIC  9(02).
      *            ANZAHL DEPOTS FÜR FREISTELLUNGSBETRAG
130902         05  FILLER                   PIC X(100).
130902*        05  KOMPL-BNK-FONDTAB OCCURS 5.
130902*            10 KOMPL-BNK-WP-KENN-NR PIC X(06).
130902*               WERTPAPIERKENN-NUMMER
130902*            10 KOMPL-BNK-SPARLEIST PIC  9(07)V99.
130902*               SPARMISIERUNG FUER SPARPLAENE
130902*            10 KOMPL-BNK-DYNAM     PIC  9(02)V99.
130902*               DYNAMISIERUNG FUER SPARPLAENE
130902*            10 KOMPL-BNK-ZAHLWEISE PIC  X(01).
130902*               ZAHLWEISE......***0 : ZAHLWEISE FEHLT
130902*               ZAHLWEISE........*1 : JAEHRLICH
130902*               ZAHLWEISE........*2 : HALBJAEHRLICH
130902*               ZAHLWEISE........*4 : VIERTELJAEHRLICH
130902*               ZAHLWEISE........*5 : 2-MONATLICH
130902*               ZAHLWEISE........*6 : MONATLICH
130902*               ZAHLWEISE.........8 : EINMALPRAEMIE
               05  KOMPL-BNK-INT-VNR      PIC  9(09).
      *            INTERNE VERTRAGS-NR.
               05  KOMPL-BNK-VBEGINN      PIC  9(08).
      *            DATUM VERTRAGSBEGINN
               05  KOMPL-BNK-VBEGINN-KZ   PIC  X(01).
      *            DATUM VERTRAGSBEGINN-KENNZEICHEN
               05  KOMPL-BNK-VABLAUF      PIC  9(08).
      *            DATUM VERTRAGSABLAUF
               05  KOMPL-BNK-VABLAUF-KZ   PIC  X(01).
      *            L = DEPOTAUFLÖSUNG (VON KD BEANTRAGT)
      *            S = INFOSPERRE GESETZT
      *            D = PHYSISCH GELÖSCHT BEI GESELLSCHAFT
               05  KOMPL-BNK-LETZT-AENDDAT PIC 9(08).
      *            DATUM LETZTER DATENSATZAENDERUNG
               05  KOMPL-BNK-AEND-NAME-KZ PIC  X(01).
      *            J = NAMENSAENDERUNG (NNAME)
               05  KOMPL-BNK-AEND-ADR-KZ  PIC  X(01).
      *            J = ADRESSAENDERUNG (STRASSE,AUSL-KZ,PLZ,ORT)
               05  KOMPL-BNK-AEND-KD-KZ   PIC  X(01).
      *            J = SONSTIGE KD-DATENAENDERUNG
               05  KOMPL-BNK-AEND-VERTRAG-KZ PIC X(01).
      *            J = SONSTIGE VERTRAGDATEN-AENDERUNG
130902         05  FILLER                 PIC  X(09).
130902*        05  KOMPL-BNK-KTO-ART      PIC  X(02).
130902*            KONTOART
130902* 171299 05  KOMPL-BNK-FSA-EURO     PIC  9(05)V99.
171299*            FREISTELLUNGSBETRAG GESAMT IN EURO
060900*        05  KOMPL-BNK-WKZ          PIC X(003).
171299*            WÄHRUNGSKENNZEICHEN
060900         05  KOMPL-BNK-RISIKOKLASSE PIC X(001).
...   *            Risikoklasse 0 - 9
...            05  KOMPL-BNK-FAXREVERS    PIC X(001).
...   *            J(a) oder N(ein)
060900*        05  KOMPL-BNK-RESERVE      PIC X(109).
060900         05  KOMPL-BNK-RESERVE      PIC X(110).
      *            RESERVE
