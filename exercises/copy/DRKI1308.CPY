      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1308.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1308.CPv  $
      *
      *   Rev 2.4   Mar 01 2006 08:15:12   t13ae31
      *AKI00513: Bestandsdatenerweiterung in der KI
      *
      *   Rev 2.3   Jul 21 2004 15:38:48   t13ae31
      *AKI00396: Bestandsdatenerweiterung bei der AMV`ers
      *
      *   Rev 2.2   Mar 04 2002 09:47:12   t13ex58
      *AKI00260
      *Neues Feld für Währungskennzeichen
      *
      *   Rev 2.1   Feb 12 2002 15:00:42   t13ex58
      *AKI00260:
      *Haftpflichtstruktur DRKI0264 erweitert durch die Vertragsid
      *
      *   Rev 2.0   18 Sep 2001 10:07:22   p13sp16
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 1 3 0 8    ########                    *
      *              #####################                             *
      *                                                                *
      *  AUSGABE AUS AUFBEREITUNGSPROGRAMMEN AM-VERS,                  *
      *  COPYSTRECKE FUER HAFTPFLICHT - VERTRAGSDATEN incl VERTRID.    *
      *                                                                *
      *  Satzlänge 534                                                 *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *12.12.01!NEUERSTELLUNG                          !W.MORFELD! 01  *
      *AKI00260!Ermitteln VERTRAGSID                   !         !     *
      *        !Wird benutzt im PGM PMKI0559 und       !         !     *
      *        !PMKI0162                               !         !     *
      *        !                                       !         !     *
      *AKI00396! Bestandserweiterung                   !Hübner   !     *
      *        !                                       !         !     *
      *AKI00513! Bestandserweiterung                   !Hübner   !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************

       01  DRKI1308.

           05  HF-VNRID                   PIC  X(013).

           05  HF-GESAMT.

              10 HF-GES                   PIC  9(002).
              10 HF-VNR                   PIC  9(015).

              10 HF-ZWEIG                 PIC  9(002).
              10 HF-ZWEIG-TEXT            PIC  X(003).

              10 HF-PRODUKT-ID-ALT        PIC  9(002).

              10 HF-PRODUKT-ID            PIC  9(002).
              10 HF-AEND-PRODUKT-ID-KZ    PIC  X(001).
      *     = 'J', wenn Produkt geändert

              10 HF-LEBENSPARTNER         PIC  X(021).
              10 HF-AEND-LEBENSPART-KZ    PIC  X(001).
      *     = 'J', wenn Lebenspartner geändert

              10 HF-GEWERB-PRIVAT-KZ      PIC  X(001).
              10 HF-AEND-GEWERB-PRIVAT-KZ PIC  X(001).
      *     = 'J', wenn Gewerblich-/Privat-Kz. geändert

              10 HF-VERSSUMME-PERS        PIC  9(008)V99.
              10 HF-AEND-VERSSUMME-PERS-KZ PIC X(001).
      *     = 'J', wenn Versicherungssumme Person geändert

              10 HF-VERSSUMME-SACH        PIC  9(008)V99.
              10 HF-AEND-VERSSUMME-SACH-KZ PIC X(001).
      *     = 'J', wenn Versicherungssumme Sach geändert

              10 HF-VERSSUMME-VERM        PIC  9(008)V99.
              10 HF-AEND-VERSSUMME-VERM-KZ PIC X(001).
      *     = 'J', wenn Versicherungssumme Vermögen geändert

              10 HF-VERSSUMME-EK          PIC  9(008)V99.
              10 HF-AEND-VERSSUMME-EK-KZ  PIC  X(001).
      *     = 'J', wenn Versicherungssumme EK geändert

              10 HF-VERSSUMME-MIETS       PIC  9(008)V99.
              10 HF-AEND-VERSSUMME-MIETS-KZ PIC X(001).
      *     = 'J', wenn Versicherungssumme Mietsach geändert

              10 HF-SELBSTBETEIL-PERS     PIC  9(008)V99.
              10 HF-AEND-SELBSTB-PERS-KZ  PIC  X(001).
      *     = 'J', wenn Selbstbeteiligung Person geändert

              10 HF-SELBSTBETEIL-SACH     PIC  9(008)V99.
              10 HF-AEND-SELBSTB-SACH-KZ  PIC  X(001).
      *     = 'J', wenn Selbstbeteiligung Sach geändert

              10 HF-SELBSTBETEIL-VERM     PIC  9(008)V99.
              10 HF-AEND-SELBSTB-VERM-KZ  PIC  X(001).
      *     = 'J', wenn Selbstbeteiligung Vermögen geändert

              10 HF-SELBSTBETEIL-EK       PIC  9(008)V99.
              10 HF-AEND-SELBSTB-EK-KZ    PIC  X(001).
      *     = 'J', wenn Selbstbeteiligung EK geändert

              10 HF-SELBSTBETEIL-MIETS    PIC  9(008)V99.
              10 HF-AEND-SELBSTB-MIETS-KZ PIC  X(001).
      *     = 'J', wenn Selbstbeteiligung Mietsach geändert

              10 HF-NETTO-PRAEMIE         PIC  9(005)V99.
              10 HF-AEND-NETTO-PRAEMIE-KZ PIC  X(001).
      *        = 'J', wenn Netto Prämie geändert

           10  HF-NETTO-PRAEMIE-Z           PIC  9(008)V99.
           10  HF-AEND-NETTO-PRAEMIE-Z-KZ   PIC  X(001).
      *        = 'J', wenn zukünftige Netto-Prämie geändert

      **** Gesamtprämie
              10  HF-NET-PRAEMIE-GES         PIC  9(006)V99.
              10  HF-AEND-NET-PRAEMIE-GES-KZ PIC  X(001).
      *        = 'J', wenn Netto Prämie Gesamt geändert

           10  HF-NETTO-PRAEMIE-Z-GES           PIC  9(008)V99.
           10  HF-AEND-NETTO-PRAEMIE-Z-GES-KZ   PIC  X(001).
      *        = 'J', wenn zukünftige Netto-Prämie Gesamt geändert

               10  HF-PHV-PAKET             PIC X(30).
               10  HF-PHV-PAKET-KZ          PIC X(01).
      *            J = Änderung Paket
               10  HF-NAME-ELT              PIC X(40).
               10  HF-NAME-ELT-KZ           PIC X(01).
      *            J = Änderung Name Eltern
               10  HF-NAME-ELT-ANZ          PIC 9(001).
               10  HF-NAME-ELTANZ-KZ        PIC X(01).
      *            J = Änderung Name Eltern Anzahl
               10  HF-NAME-KIND             PIC X(40).
               10  HF-NAME-KIND-KZ          PIC X(01).
      *            J = Änderung Name Kind
               10  HF-NAME-KIND-ANZ         PIC 9(001).
               10  HF-NAME-KIND-ANZ-KZ      PIC X(01).
      *            J = Änderung Name Kind Anzahl
               10  HF-AUSFDECK              PIC X(001).
               10  HF-AUSFDECK-KZ           PIC X(01).
      *            J = Änderung Ausfalldeckung
               10  HF-AUSFDECK-ELT          PIC X(001).
               10  HF-AUSFD-ELT-KZ          PIC X(01).
      *            J = Änderung Ausfalldeckung Eltern
               10  HF-AUSFDECK-KIND         PIC X(001).
               10  HF-AUSFD-KIND-KZ         PIC X(01).
      *            J = Änderung Ausfalldeckung Kind Anzahl
               10  HF-ANZ-HUNDE             PIC 9(002).
               10  HF-ANZ-HUNDE-KZ          PIC X(01).
      *            J = Änderung Anzahl Hunde
               10  HF-ANZ-PFERD             PIC 9(002).
               10  HF-ANZ-PFERD-KZ          PIC X(01).
      *            J = Änderung Anzahl Pferde
               10  HF-ANZ-EIGWO             PIC 9(002).
               10  HF-ANZ-EIGWO-KZ          PIC X(01).
      *            J = Änderung Anzahl Eigentumswohnungen
               10  HF-ANZ-MOTOV              PIC 9(002).
               10  HF-ANZ-MOTOV-KZ           PIC X(01).
      *            J = Änderung Anzahl Motorboote ohne Vermietung
               10  HF-ANZ-MOTMV             PIC 9(002).
               10  HF-ANZ-MOTMV-KZ          PIC X(01).
      *            J = Änderung Anzahl Motorboote mit Vermietung
               10  HF-ANZ-SEGOV             PIC 9(002).
               10  HF-ANZ-SEGOV-KZ          PIC X(01).
      *            J = Änderung Anzahl Segelboote ohne Vermietung
               10  HF-ANZ-SEGMV             PIC 9(002).
               10  HF-ANZ-SEGMV-KZ          PIC X(01).
      *            J = Änderung Anzahl Segelboote mit Vermietung

           10  HF-DIFFWG                  PIC X(01).
           10  HF-AEND-DIFFWG             PIC X(01).
      **       'J' wenn Differenzierungskz geändert

           10  HF-DIFFGES                 PIC X(90).
           10  HF-AEND-DIFFGES            PIC X(01).
      **       'J' wenn Differenzierungsgesellschaft geändert

           10  HF-DIFF-VSNR               PIC X(40).
           10  HF-AEND-DIFF-VSNR          PIC X(01).
      **       'J' wenn DifferenzierungsVersicherungsnr geändert

           10  HF-DIFF-ABLAUF             PIC X(10).
           10  HF-AEND-DIFF-ABLAUF        PIC X(01).
      **       'J' wenn Differenzierungsablauf geändert

           10  HF-DIFF-KUEND-KNZ          PIC X(01).
           10  HF-AEND-DIFF-KUEND-KNZ     PIC X(01).
      **       'J' wenn Differenzierungskündigungskz geändert

           10  FILLER                     PIC  X(028).
              10 HF-AKTUAL-KZ             PIC  X(001).
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE

              10 HF-WAEHRUNGS-KZ          PIC  X(001).
      *        E = EURO

      ******************************************************************
      *          E n d e    Copy-Buch   DRKI1308                       *
      ******************************************************************
