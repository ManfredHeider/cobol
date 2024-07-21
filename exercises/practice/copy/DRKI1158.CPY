      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1158.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1158.CPv  $
      *
      *   Rev 2.3   Oct 10 2013 10:52:42   t13ex15
      *AKI01204: SEPA - DB-Online AGI Programme und Copys
      *
      *   Rev 2.2   Aug 18 2008 14:46:36   t13ex50
      *FKI01178: Ursprüngl. Eröffnungsdaten aus dem alten AGI-Syste
      *m übernehm
      *
      *   Rev 2.1   Mar 07 2002 17:45:12   t13ex12
      *AKI00162 Neue Felder von DIT
      *
      *   Rev 2.0   07 Dec 2000 09:36:06   p13sp16
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR DIT-Kunden- und Vertragsdaten               *
      *    KOMPLETTBESTAND                                             *
      *                                                                *
      *    DATEITYP  = KSDS                                            *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    Ersetzt DRKI0168 in PMKI0085.                               *
      *    Länge = 1200 bytes                                          *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
      *15.11.01!NEUERSTELLUNG FÜR DIT WEGEN NEUE FELDER.!V.D.GOLTZ! 01 *
      *AKI00162!EINZAHLUNGSFELDER WURDEN AUSGELAGERT.   !         !    *
      *--------!----------------------------------------!---------!----*
130808*13.08.08!HIST. ERÖFFNUNGSDATUM EINGEFÜGT        !SCHLEICHER!2.2 *
130808*FKI01178!                                       !          !    *
      *--------!---------------------------------------!----------!----*
230913*23.09.13!ANPASSUNGEN SEPA                       !SWZ       !2.3 *
      *        !                                       !          !    *
      *        !                                       !          !    *
      *        !                                       !          !    *
      *        !                                       !          !    *
      *        !                                       !          !    *
      *        !                                       !          !    *
      *--------!----------------------------------------!---------!----*
      *
       01  DRKI1158.
      *
             03  KOMPL-ALTER-SATZ1.
               05  KOMPL-BNK-SA            PIC  X(001).
      *            SATZART        = 'C'
               05  KOMPL-BNK-GES           PIC  9(002).
      *            GESELLSCHAFT
      *
               05  KOMPL-BNK-KEY.
                   10 KOMPL-BNK-VNR           PIC  9(013).
      *               VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               05  KOMPL-BNK-VBNR      PIC  9(007).
      *            ABSCHLUSSVERMITTLER
               05  KOMPL-BNK-PERSTAB OCCURS 2.
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
230913*            10  KOMPL-BNK-TEL       PIC X(015).
      *                TELEFON-NR.
230913*            10  KOMPL-BNK-FAX       PIC X(015).
      *                FAX-NR.
                   10  KOMPL-BNK-GEBDAT    PIC 9(008).
      *                GEBURTSTAGSDATUM    JJJJMMTT
230913         05  KOMPL-BNK-BANK      PIC X(020).
      *            BANKVERBINDUNG
230913*        05  KOMPL-BNK-BLZ       PIC 9(011).
      *            BANKLEITZAHL
230913*        05  KOMPL-BNK-KTO-NR    PIC 9(018).
      *            KONTO-NUMMER
230913         05  KOMPL-BNK-BIC        PIC X(011).
      *            BANKLEITZAHL
230913         05  KOMPL-BNK-IBAN       PIC X(034).
      *            KONTO-NUMMER
230913*        05  KOMPL-BNK-VERFUEG-KZ PIC X(001).
      *            KENNZEICHEN VERFÜGUNG
               05  KOMPL-BNK-NACHLASS-KZ PIC X(001).
      *            KENNZEICHEN NACHLASS
230913*        05  KOMPL-BNK-FREI-BETRAG PIC  9(05)V99.
      *            FREISTELLUNGSBETRAG GESAMT in Euro
230913*        05  KOMPL-BNK-FREI-ABLAUF-DAT PIC 9(08).
      *            ABLAUFDATUM FREISTELLUNGSBETRAG
230913*        05  KOMPL-BNK-ANZ-DEPOT   PIC  9(02).
      *            ANZAHL DEPOTS FÜR FREISTELLUNGSBETRAG

230913*      03  KOMPL-ALTER-SATZ2.
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
230913*        05  KOMPL-BNK-AEND-NAME-KZ PIC  X(01).
      *            J = NAMENSAENDERUNG (NNAME)
230913*        05  KOMPL-BNK-AEND-ADR-KZ  PIC  X(01).
      *            J = ADRESSAENDERUNG (STRASSE,AUSL-KZ,PLZ,ORT)
230913*        05  KOMPL-BNK-AEND-KD-KZ   PIC  X(01).
      *            J = SONSTIGE KD-DATENAENDERUNG
230913*        05  KOMPL-BNK-AEND-VERTRAG-KZ PIC X(01).
      *            J = SONSTIGE VERTRAGDATEN-AENDERUNG
               05  KOMPL-BNK-KTO-ART      PIC  X(02).
      *            KONTOART
               05  KOMPL-BNK-RISIKOKLASSE PIC X(001).
      *            Risikoklasse 0 - 9
230913*        05  KOMPL-BNK-FAXREVERS    PIC X(001).
      *            J(a) oder N(ein)

230913*        05  KOMPL-BNK-TELREVERS    PIC X(001).
      *            Telefonrevers = 'J' oder 'N'
230913*        05  KOMPL-BNK-ONLINEREVERS PIC X(001).
      *            ONLINEREVERS = 'J' ODER 'N'
               05  KOMPL-BNK-LETZT-AENDDAT PIC 9(08).
      *            DATUM LETZTER DATENSATZAENDERUNG
130808         05  KOMPL-BNK-HIST-VBEGINN PIC  9(08).
130808*            DATUM AUS DEM ALTSYSTEM - ODER 0



130808*        05  KOMPL-BNK-RESERVE      PIC X(151).
130808         05  KOMPL-BNK-RESERVE      PIC X(743).
      *            RESERVE

