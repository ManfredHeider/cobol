      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1157.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1157.CPv  $
      *
      *   Rev 2.2   Oct 10 2013 10:52:36   t13ex15
      *AKI01204: SEPA - DB-Online AGI Programme und Copys
      *
      *   Rev 2.1   Mar 07 2002 17:45:12   t13ex12
      *AKI00162 Neue Felder von DIT
      *
      *   Rev 2.0   07 Dec 2000 09:36:08   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                  ###############                               *
      *         #######  D R K I 1 1 5 7  #######                      *
      *                  ###############                               *
      *                                                                *
      *    COPY-MEMBER FÜR AUSGABEDATEI VERäNDRUNGSBESTAND DEPOTERöFF- *
      *    NUNGEN UND KUNDENDATENäNDERUNGEN DIT                        *
      *                                                                *
      *    LAENGE =  1900 BYTES      ERSTELLT  AM 04.12.00             *
      *                                                                *
      *                                                                *
      *                      DATEILÄNGE = 1900                         *
      *                      DATEITYP   = QSAM                         *
      *                                                                *
      *                                                                *
      *    Ersetzt DRKI0167 in PMKI0085.                               *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
      *15.11.01!NEUERSTELLUNG für DIT wegen neue Felder !v.d.Goltz! 01 *
      *AKI00162!Tel.- und Online-Revers                 !         !    *
      *        !und Wegfall von Einzahlungstabelle.     !         !    *
      *--------!----------------------------------------!---------!----*
      *23.09.13!Anpassungen SEPA                        !SWZ      !2.1 *
      *        !                                        !         !    *
      ******************************************************************
      *
       01  DRKI1157.
               05  BNK-SA                  PIC  X(001).
      *            SATZART
               05  BNK-GES                 PIC  9(002).
      *            GESELLSCHAFT
               05  BNK-VNR                 PIC  9(013).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               05  BNK-VERARB-KZ           PIC  X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *                I = INSERT
      *                U = UPDATE
               05  BNK-VBNR                PIC  9(007).
      *            ABSCHLUSSVERMITTLER
               05  BNK-AEND-VBNR-KZ        PIC  X(001).
      *            J = ABSCHLUSSVERMITTLERÄNDERUNG
               05  BNK-PERSTAB    OCCURS 2.
                   10  BNK-ANREDE          PIC  9(001).
      *                ANREDEKENNZEICHEN
                   10  BNK-AEND-ANREDE-KZ  PIC  X(001).
      *                J = ANREDEÄNDERUNG
                   10  BNK-NAMENSAENDERUNG-KZ  PIC  X(001).
      *            VERRBEITUNGSKENNZEICHEN
      *                I = INSERT
      *                U = UPDATE
                   10  BNK-NNAME           PIC  X(040).
      *                NACHNAME DES KONTOINHABERS
                   10  BNK-AEND-NNAME-KZ   PIC  X(001).
      *                J = NACHNAMEÄNDERUNG
                   10  BNK-VNAME           PIC  X(020).
      *                VORNAME DES KONTOINHABERS
                   10  BNK-AEND-VNAME-KZ   PIC  X(001).
      *                J = VORNAMEÄNDERUNG
                   10  BNK-STR             PIC  X(040).
      *                STRASSE/POSTFACH
                   10  BNK-AEND-STR-KZ     PIC  X(001).
      *                J = STRASSENNAMEÄNDERUNG
                   10  BNK-AUSL-KZ         PIC  X(003).
      *                AUSLANDSKENNZEICHEN
                   10  BNK-AEND-AUSL-KZ    PIC  X(001).
      *                J = AUSLANDSKENN. ÄNDERUNG
                   10  BNK-PLZ             PIC  X(009).
      *                POSTLEITZAHL
                   10  BNK-AEND-PLZ-KZ     PIC  X(001).
      *                J = PLZ ÄNDERUNG
                   10  BNK-ORT             PIC  X(040).
      *                ORT
                   10  BNK-AEND-ORT-KZ     PIC  X(001).
      *                J = ORT ÄNDERUNG
230913*            10  BNK-TEL             PIC  X(015).
      *                TELEFON-NR.
230913*            10  BNK-AEND-TEL-KZ     PIC  X(001).
      *                J = TELEFONNR ÄNDERUNG
230913*            10  BNK-FAX             PIC  X(015).
      *                FAX-NR.
230913*            10  BNK-AEND-FAX-KZ     PIC  X(001).
      *                J = FAXNNR ÄNDERUNG
                   10  BNK-GEBDAT          PIC  9(008).
      *                GEBURTSTAGSDATUM    JJJJMMTT
                   10  BNK-AEND-GEBDAT-KZ  PIC  X(001).
      *                J = GEBURTSTAG ÄNDERUNG
230913*            10  BNK-BLZ             PIC  9(011).
      *                BANKLEITZAHL
230913*            10  BNK-AEND-BLZ-KZ     PIC  X(001).
      *                J = BANKLEITZAHL ÄNDERUNG
230913*            10  BNK-KTO-NR          PIC  9(018).
      *                KONTO-NUMMER
230913*            10  BNK-AEND-KTO-KZ     PIC  X(001).
      *                J = KONTONR. ÄNDERUNG
230913*            10  BNK-TEL-ALT         PIC  X(015).
      *                VORHERIGE TELEFONNR DES KONTOINHABERS
230913*            10  BNK-FAX-ALT         PIC  X(015).
      *                VORHERIGE FAXNR DES KONTOINHABERS
                   10  BNK-NNAME-ALT       PIC  X(040).
      *                  NACHNAME DES KONTOINHABERS AUS KOMPLETTBESTAND
                   10  BNK-VNAME-ALT       PIC  X(020).
      *                  VORNAME DES KONTOINHABERS AUS KOMPLETTBESTAND

               05  BNK-NAMETAB-KOMPL  OCCURS 2.
                   10  BNK-NAMENSAENDERUNG-KOMPL-KZ  PIC X(001).
      *            VERRBEITUNGSKENNZEICHEN
      *                D = DELETE
                   10  BNK-NNAME-KOMPL     PIC  X(040).
      *                  NACHNAME DES KONTOINHABERS AUS KOMPLETTBESTAND
                   10  BNK-VNAME-KOMPL     PIC  X(020).
      *                  VORNAME DES KONTOINHABERS AUS KOMPLETTBESTAND
                   10  BNK-GEBDAT-KOMPL    PIC  9(008).
      *                GEBURTSTAGSDATUM    JJJJMMTT

               05  BNK-BANK                PIC  X(020).
      *            BANKVERBINDUNG
               05  BNK-AEND-BANK-KZ        PIC  X(001).
      *            J = BANKVERBINDUNG ÄNDERUNG
               05  BNK-BIC                 PIC  X(011).
      *            BIC
               05  BNK-AEND-BIC-KZ         PIC  X(001).
      *            J = BIC ÄNDERUNG
               05  BNK-IBAN                PIC  X(034).
      *            BIC
               05  BNK-AEND-IBAN-KZ        PIC  X(001).
      *            J = BIC ÄNDERUNG
230913*        05  BNK-VERFUEG-KZ          PIC  X(001).
      *            KENNZEICHEN VERFÜGUNG
230913*        05  BNK-AEND-VERFUEG-KZ     PIC  X(001).
      *            J = VERFÜGUNGSKENNZ. ÄNDERUNG
               05  BNK-NACHLASS-KZ         PIC  X(001).
      *            KENNZEICHEN NACHLASS
               05  BNK-AEND-NACHLASS-KZ    PIC  X(001).
      *            J = NACHLASSKENNZ. ÄNDERUNG
230913*        05  BNK-FREI-BETRAG         PIC  9(005)V99.
      *            FREISTELLUNGSBETRAG GESAMT in Euro
230913*        05  BNK-AEND-FREI-BETRAG-KZ PIC  X(001).
      *            J = FREISTELLUNGSBETRAG ÄNDERUNG
230913*        05  BNK-FREI-ABLAUF-DAT     PIC  9(008).
      *            ABLAUFDATUM FREISTELLUNGSBETRAG
230913*        05  BNK-AEND-FREI-ABLAUF-KZ PIC  X(001).
      *            J = ABLAUFDATUM FREISTELLUNGSBETRAG ÄNDERUNG
230913*        05  BNK-ANZ-DEPOT           PIC  9(002).
      *            ANZAHL DEPOTS FÜR FREISTELLUNGSBETRAG
230913*        05  BNK-AEND-ANZ-DEPOT-KZ   PIC  X(001).
      *            J = ANZAHL DEPOTS ÄNDERUNG

               05  BNK-INT-VNR             PIC  9(009).
      *            INTERNE VERTRAGS-NR.
               05  BNK-AEND-INT-VNR-KZ     PIC  X(001).
      *            J = INTERNE VERTRAGSNR ÄNDERUNG
               05  BNK-VBEGINN             PIC  9(008).
      *            DATUM VERTRAGSBEGINN
               05  BNK-AEND-VBEGINN-KZ     PIC  X(001).
      *            J = DATUM VERTRAGSBEGINN ÄNDERUNG
               05  BNK-VBEGINN-KZ          PIC  X(001).
               05  BNK-AEND-VBEGINNKZ-KZ   PIC  X(001).
      *            J = VERTRAGSBEGINNKENNZEICHEN ÄNDERUNG
               05  BNK-VABLAUF             PIC  9(008).
      *            DATUM VERTRAGSABLAUF
               05  BNK-AEND-VABLAUF-KZ     PIC  X(001).
      *            J = DATUM VERTRAGSABLAUF ÄNDERUNG
               05  BNK-VABLAUF-KZ          PIC  X(001).
      *            DATUM VERTRAGSABLAUF
               05  BNK-AEND-VABLAUFKZ-KZ   PIC  X(001).
      *            J = VERTRAGSABLAUFKENNZEICHEN ÄNDERUNG
230913*        05  BNK-AEND-NAME-KZ        PIC  X(001).
      *            J = NAMENSAENDERUNG (NNAME)
230913*        05  BNK-AEND-ADR-KZ         PIC  X(001).
      *            J = ADRESSAENDERUNG (STRASSE,AUSL-KZ,PLZ,ORT)
230913*        05  BNK-AEND-KD-KZ          PIC  X(001).
      *            J = SONSTIGE KD-DATENAENDERUNG
               05  BNK-KTO-ART             PIC  X(002).
      *            KONTOART
               05  BNK-AEND-KTO-ART-KZ     PIC  X(001).
      *            J = KONTOART ÄNDERUNG

               05  BNK-RISIKOKLASSE        PIC  X(001).
      *            Risikoklasse 0 - 9
               05  BNK-AEND-RISIKO-KZ      PIC  X(001).
      *            J = Änderung der Risikoklasse
230913*        05  BNK-FAXREVERS           PIC  X(001).
      *            J(a) oder N(ein)
230913*        05  BNK-AEND-FAXREV-KZ      PIC  X(001).
      *            J = Änderung Faxrevers

230913*        05  BNK-TELREVERS           PIC  X(001).
      *            Telefonrevers = 'J' oder 'N'
230913*        05  BNK-AEND-TELREV-KZ      PIC  X(001).
      *            J = Änderung Telrevers
230913*        05  BNK-ONLINEREVERS        PIC  X(001).
      *            Onlinerevers = 'J' oder 'N'
230913*        05  BNK-AEND-ONLREV-KZ      PIC  X(001).
      *            J = Änderung Onlinerevers
               05  BNK-LETZT-AENDDAT       PIC  9(008).
      *            DATUM LETZTER DATENSATZAENDERUNG
               05  BNK-AEND-AENDDAT-KZ     PIC  X(001).
      *            J = LETZTES AENDDATUM GEäNDERT

               05  BNK-WKZ                 PIC  X(003).
      *            WÄHRUNGSKENNZEICEHN
               05  BNK-AEND-WKZ-KZ         PIC  X(001).
      *            J = WÄHRUNGSKENNZEICHEN ÄNDERUNG

               05  BNK-RESERVE             PIC  X(1157).

