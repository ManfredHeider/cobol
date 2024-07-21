      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0167.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/DRKI0167.CPv  $
      *
      *   Rev 2.3   Nov 28 2002 08:39:18   t13ex49
      *AKI00294 - ISIN
      *
      *   Rev 2.2   31 Oct 2000 16:46:54   T13EX12
      *AKI00145 Risikoklasse
      *
      *   Rev 2.1   Mar 09 2000 10:59:50   T13EX39
      *AKI00069 WKZ
      *
      *   Rev 2.0   Feb 27 1999 12:53:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 17:50:38   T13AE38
      *AKI00014 - Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:53:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                  ###############                               *
      *         #######  D R K I 0 1 6 7  #######                      *
      *                  ###############                               *
      *                                                                *
      *    COPY-MEMBER FÜR AUSGABEDATEI VERäNDRUNGSBESTAND DEPOTERöFF- *
      *    NUNGEN UND KUNDENDATENäNDERUNGEN BANKEN                     *
      *                                                                *
      *    LAENGE =  1900 BYTES      ERSTELLT  AM 25.05.98 / S.JOST    *
130902*    LAENGE =  1772 BYTES      geändert  am 13.09.02 / P. Pohl   *
      *                                                                *
      *                                                                *
130902*                      DATEILÄNGE = 1900 / 1772                  *
      *                      DATEITYP   = QSAM                         *
      *                                                                *
      *  DATEINAME =                                                   *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
      *07.04.98!NEUERSTELLUNG                           !S.JOST   ! 01 *
      *AKI00001!                                        !         !    *
      *--------!----------------------------------------!--------_!----*
      *17.12.99!WKZ aufgenommen                         !WHO      ! 01 *
      *AKI00069!                                        !         !    *
      *--------!----------------------------------------!--------_!----*
      *06.09.00!Risikoklasse, Faxrevers aufgenommen. Vor!v.d.Goltz! 03 *
      *AKI00145!WKZ definiert weil DIT uns WKZ nicht    !         !    *
      *        !liefert!                                !         !    *
      *--------!----------------------------------------!--------_!----*
130902*13.09.02!Umstellung ISIN                         !         !    *
130902*        !Tabelle der Sparpläne entfällt          !         !    *
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      ******************************************************************
      *
       01  DRKI0167.
               05  BNK-SA              PIC  X(001).
      *            SATZART
               05  BNK-GES             PIC  9(002).
      *            GESELLSCHAFT
               05  BNK-VNR             PIC  9(015).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               05  BNK-VERARB-KZ       PIC  X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *                I = INSERT
      *                U = UPDATE
               05  BNK-VBNR            PIC  9(007).
      *            ABSCHLUSSVERMITTLER
               05  BNK-AEND-VBNR-KZ       PIC  X(01).
      *            J = ABSCHLUSSVERMITTLERÄNDERUNG
               05  BNK-PERSTAB    OCCURS 4.
                   10  BNK-ANREDE          PIC 9(001).
      *                ANREDEKENNZEICHEN
                   10  BNK-AEND-ANREDE-KZ  PIC X(01).
      *                J = ANREDEÄNDERUNG
                   10  BNK-NAMENSAENDERUNG-KZ  PIC  X(001).
      *            VERRBEITUNGSKENNZEICHEN
      *                I = INSERT
      *                U = UPDATE
                   10  BNK-NNAME           PIC X(040).
      *                NACHNAME DES KONTOINHABERS
                   10  BNK-AEND-NNAME-KZ   PIC X(01).
      *                J = NACHNAMEÄNDERUNG
                   10  BNK-VNAME           PIC X(020).
      *                VORNAME DES KONTOINHABERS
                   10  BNK-AEND-VNAME-KZ   PIC X(01).
      *                J = VORNAMEÄNDERUNG
                   10  BNK-STR             PIC X(040).
      *                STRASSE/POSTFACH
                   10  BNK-AEND-STR-KZ     PIC X(01).
      *                J = STRASSENNAMEÄNDERUNG
                   10  BNK-AUSL-KZ         PIC X(003).
      *                AUSLANDSKENNZEICHEN
                   10  BNK-AEND-AUSL-KZ    PIC X(01).
      *                J = AUSLANDSKENN. ÄNDERUNG
                   10  BNK-PLZ             PIC X(009).
      *                POSTLEITZAHL
                   10  BNK-AEND-PLZ-KZ     PIC X(01).
      *                J = PLZ ÄNDERUNG
                   10  BNK-ORT             PIC X(040).
      *                ORT
                   10  BNK-AEND-ORT-KZ     PIC X(01).
      *                J = ORT ÄNDERUNG
                   10  BNK-TEL             PIC X(015).
      *                TELEFON-NR.
                   10  BNK-AEND-TEL-KZ     PIC X(01).
      *                J = TELEFONNR ÄNDERUNG
                   10  BNK-FAX             PIC X(015).
      *                FAX-NR.
                   10  BNK-AEND-FAX-KZ     PIC X(01).
      *                J = FAXNNR ÄNDERUNG
                   10  BNK-GEBDAT          PIC 9(008).
      *                GEBURTSTAGSDATUM    JJJJMMTT
                   10  BNK-AEND-GEBDAT-KZ  PIC X(01).
      *                J = GEBURTSTAG ÄNDERUNG
                   10  BNK-BANK            PIC X(020).
      *                BANKVERBINDUNG
                   10  BNK-AEND-BANK-KZ    PIC X(01).
      *                J = BANKVERBINDUNG ÄNDERUNG
                   10  BNK-BLZ             PIC 9(011).
      *                BANKLEITZAHL
                   10  BNK-AEND-BLZ-KZ     PIC X(01).
      *                J = BANKLEITZAHL ÄNDERUNG
                   10  BNK-KTO-NR          PIC 9(018).
      *                KONTO-NUMMER
                   10  BNK-AEND-KTO-KZ     PIC X(01).
      *                J = KONTONR. ÄNDERUNG
                   10  BNK-TEL-ALT         PIC X(015).
      *                VORHERIGE TELEFONNR DES KONTOINHABERS
                   10  BNK-FAX-ALT         PIC X(015).
      *                VORHERIGE FAXNR DES KONTOINHABERS
                   10  BNK-NNAME-ALT       PIC X(040).
      *                  NACHNAME DES KONTOINHABERS AUS KOMPLETTBESTAND
                   10  BNK-VNAME-ALT       PIC X(020).
      *                  VORNAME DES KONTOINHABERS AUS KOMPLETTBESTAND
               05  BNK-NAMETAB-KOMPL  OCCURS 4.
                   10  BNK-NAMENSAENDERUNG-KOMPL-KZ  PIC  X(001).
      *            VERRBEITUNGSKENNZEICHEN
      *                D = DELETE
                   10  BNK-NNAME-KOMPL     PIC X(040).
      *                  NACHNAME DES KONTOINHABERS AUS KOMPLETTBESTAND
                   10  BNK-VNAME-KOMPL     PIC X(020).
      *                  VORNAME DES KONTOINHABERS AUS KOMPLETTBESTAND
                   10  BNK-GEBDAT-KOMPL    PIC 9(008).
      *                GEBURTSTAGSDATUM    JJJJMMTT
               05  BNK-VERFUEG-KZ      PIC  X(001).
      *            KENNZEICHEN VERFÜGUNG
               05  BNK-AEND-VERFUEG-KZ PIC  X(01).
      *            J = VERFÜGUNGSKENNZ. ÄNDERUNG
               05  BNK-NACHLASS-KZ     PIC  X(001).
      *            KENNZEICHEN NACHLASS
               05  BNK-AEND-NACHLASS-KZ PIC X(01).
      *            J = NACHLASSKENNZ. ÄNDERUNG
               05  BNK-FREI-BETRAG       PIC  9(05)V99.
      *            FREISTELLUNGSBETRAG GESAMT
               05  BNK-AEND-FREI-BETRAG-KZ PIC X(01).
      *            J = FREISTELLUNGSBETRAG ÄNDERUNG
               05  BNK-FREI-ABLAUF-DAT   PIC  9(08).
      *            ABLAUFDATUM FREISTELLUNGSBETRAG
               05  BNK-AEND-FREI-ABLAUF-KZ PIC X(01).
      *            J = ABLAUFDATUM FREISTELLUNGSBETRAG ÄNDERUNG
               05  BNK-ANZ-DEPOT         PIC  9(02).
      *            ANZAHL DEPOTS FÜR FREISTELLUNGSBETRAG
               05  BNK-AEND-ANZ-DEPOT-KZ PIC X(01).
      *            J = ANZAHL DEPOTS ÄNDERUNG
130902*        05  BNK-FONDTAB      OCCURS 5.
130902*            10 BNK-WP-KENN-NR      PIC  X(06).
130902*               WERTPAPIERKENN-NUMMER
130902*            10 BNK-AEND-WP-KENN-NR-KZ PIC X(01).
130902*               J = WERTPAPIERKENNNR ÄNDERUNG
130902*            10 BNK-SPARLEIST       PIC  9(07)V99.
130902*               SPARMISIERUNG FUER SPARPLAENE
130902*            10 BNK-AEND-SPARLEIST-KZ PIC X(01).
130902*               J = SPARMISIERUNG ÄNDERUNG
130902*            10 BNK-DYNAM           PIC  9(02)V99.
130902*               DYNAMISIERUNG FUER SPARPLAENE
130902*            10 BNK-AEND-DYNAM-KZ   PIC X(01).
130902*               J = DYNAMISIERUNG ÄNDERUNG
130902*            10 BNK-ZAHLWEISE       PIC  X(01).
130902*            10 BNK-AEND-ZAHLWEISE-KZ   PIC X(01).
130902*               J = ZAHLWEISE ÄNDERUNG
               05  BNK-INT-VNR            PIC  9(09).
      *            INTERNE VERTRAGS-NR.
               05  BNK-AEND-INT-VNR-KZ    PIC  X(01).
      *            J = INTERNE VERTRAGSNR ÄNDERUNG
               05  BNK-VBEGINN            PIC  9(08).
      *            DATUM VERTRAGSBEGINN
               05  BNK-AEND-VBEGINN-KZ    PIC  X(01).
      *            J = DATUM VERTRAGSBEGINN ÄNDERUNG
               05  BNK-VBEGINN-KZ         PIC  X(01).
               05  BNK-AEND-VBEGINNKZ-KZ  PIC  X(01).
      *            J = VERTRAGSBEGINNKENNZEICHEN ÄNDERUNG
               05  BNK-VABLAUF            PIC  9(08).
      *            DATUM VERTRAGSABLAUF
               05  BNK-AEND-VABLAUF-KZ    PIC  X(01).
      *            J = DATUM VERTRAGSABLAUF ÄNDERUNG
               05  BNK-VABLAUF-KZ         PIC  X(01).
      *            DATUM VERTRAGSABLAUF
               05  BNK-AEND-VABLAUFKZ-KZ  PIC  X(01).
      *            J = VERTRAGSABLAUFKENNZEICHEN ÄNDERUNG
               05  BNK-LETZT-AENDDAT      PIC  9(08).
      *            DATUM LETZTER DATENSATZAENDERUNG
               05  BNK-AEND-AENDDAT-KZ    PIC  X(01).
      *            J = LETZTES AENDDATUM GEäNDERT
               05  BNK-AEND-NAME-KZ       PIC  X(01).
      *            J = NAMENSAENDERUNG (NNAME)
               05  BNK-AEND-ADR-KZ        PIC  X(01).
      *            J = ADRESSAENDERUNG (STRASSE,AUSL-KZ,PLZ,ORT)
               05  BNK-AEND-KD-KZ         PIC  X(01).
      *            J = SONSTIGE KD-DATENAENDERUNG
               05  BNK-KTO-ART            PIC  X(02).
      *            KONTOART
               05  BNK-AEND-KTO-ART-KZ    PIC  X(01).
      *            J = KONTOART ÄNDERUNG
130902* 171299 05  BNK-FSA-EURO          PIC  9(05)V99.
130902* 171299     FREISTELLUNGSBETRAG GESAMT IN EURO
130902* 171299 05  BNK-AEND-FSA-EURO-KZ    PIC X(01).
130902* 171299     J = FREISTELLUNGSBETRAG ÄNDERUNG
060900         05  BNK-RISIKOKLASSE       PIC X(001).
...   *            Risikoklasse 0 - 9
...            05  BNK-AEND-RISIKO-KZ     PIC X(001).
...   *            J = Änderung der Risikoklasse
...            05  BNK-FAXREVERS          PIC X(001).
...   *            J(a) oder N(ein)
...            05  BNK-AEND-FAXREV-KZ     PIC X(001).
...   *            J = Änderung Faxrevers
171299         05  BNK-WKZ                PIC X(003).
171299*            WÄHRUNGSKENNZEICEHN
171299         05  BNK-AEND-WKZ-KZ        PIC X(001).
      *            J = WÄHRUNGSKENNZEICHEN ÄNDERUNG
060900*        05  BNK-RESERVE            PIC  X(018).
060900         05  BNK-RESERVE            PIC  X(014).
