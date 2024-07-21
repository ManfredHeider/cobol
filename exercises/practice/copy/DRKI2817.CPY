      * --------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2817.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2817.CPv  $
      *
      *   Rev 2.6   Oct 01 2018 09:34:38   t13ae52
      *AKI01632: G48 - Hinzufügen neuer Kreditkarten
      *
      *   Rev 2.5   Sep 27 2018 16:56:20   t13ae52
      *AKI01632: G48 - Hinzufügen neuer Kreditkarten
      *
      *   Rev 2.4   Jan 25 2018 07:04:02   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs  KARTEN
      *
      *   Rev 2.3   Nov 16 2015 10:30:36   t13ex39
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens    WHO
      *
      *   Rev 2.2   Apr 16 2014 17:11:42   t13ex19
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *
      *   Rev 2.1   Jan 23 2014 15:35:46   i13az40
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *
      *   Rev 2.0   Nov 26 2013 15:59:58   i13az40
      *Archiverstellung
      *****************************************************************
      *              ##################################               *
      *      ########         D R K I 2 8 1 7          ########       *
      *              ##################################               *
      *                                                               *
      *          DEUTSCHE BANK COPY AKTUALISIEREN KREDITKARTE         *
      *                                                               *
      *          SATZLÄENGE: 121+77 FILLER                            *
      *                                                               *
      *---------------------------------------------------------------*
      *   LOGBUCH                                                     *
      *DATUM   !    ÄNDERUNG                 !NAME              !VERS.*
      *AUFTRAG !                             !                  !     *
      *---------------------------------------------------------------*
      *26.11.13!NEUERSTELLUNG                !HARALANOVA        !01   *
      *--------------------------------------------------------------*
240915*AKI01385!24.09.15!5.0 ! neues Atrtrbut Provisions-  ! WHO      *
      *        !        !    ! relevant                    !          *
      *--------------------------------------------------------------*
101217*AKI01556!10.12.17! PIN verarbeiten                  ! WHO      *
      *        !        !                                  !          *
      *---------------------------------------------------------------*
      *AKI01632!27.09.18!2.5 ! NEUE KREDITKARTEN           !SCHUMACHER*
      *        !        !    ! UND KREDITKARTEN-LIMIT      !          *
      *****************************************************************
       01 DRKI2817.

      *   KKA = Kreditkarte

          05 KKA-GES                        PIC 9(02).
          05 KKA-VNR                        PIC 9(15).
          05 KKA-PRODUKT-LFDNR              PIC 9(04).
      *   Produkt laufende Nummer
          05 KKA-PRODUKT-ID                 PIC S9(04).
      *   Produkt-ID
          05 KKA-KARTE-LFDNR                PIC S9(04).
      *   Kredikarte laufende Nummer
          05 KKA-KARTENART                  PIC X(03).
      *   Kartenart
      *            ECH - EUROCARD Hauptkarte
      *            MCH - MASTERCARD Hauptkarte
      *            VCH - VISA Hauptkarte
      *            ECZ - EUROCARD Zusatzkarte
      *            MCZ - MASTERCARD Zusatzkarte
      *            VCZ - VISA Zusatzkarte
      *            BBH - Schumikarte
      *            MSK - Schumikarte
      *            DVR - DVAG-Karte
      *
      *         ***AB 24.09.2018***
*     *            BCS - KREDITKARTE BUSINESSCARD
*     *            DBU - DEUTSCHE BANK BUSINESSCARD DIRECT
*     *            MGH - MASTERCARD GOLD HAUPTKARTE
*     *            MPH - MASTERCARD PLATIN HAUPTKARTE
*     *            TRV - MASTERCARD TRAVEL
*     *            VGH - VISA GOLD HAUPTKARTE

          05 KKA-AEND-KARTENART-KZ          PIC X(01).
      *   J wenn     geaendert
          05 KKA-NR                         PIC X(16).
      *   Kreditkartennummer
          05 KKA-AEND-KKA-NR                PIC X(01).
      *   J wenn     geaendert
          05 KKA-HAUPT-KARTEN-NR            PIC X(16).
      *   Hauptkartennummer
          05 KKA-AEND-HAUPT-KARTEN-NR       PIC X(01).
      *   J wenn     geaendert
          05 KKA-VNR-HAUPT-KARTEN-NR        PIC X(15).
      *
          05 KKA-AEND-HAUPT-KARTEN-NR-KZ    PIC X(01).
      *   J wenn     geaendert
          05 KKA-ABRECH-KONTO-NR            PIC X(10).
      *   Abrechnungskonto-Nummer
          05 KKA-AEND-ABRECH-KONTO-NR-KZ    PIC X(01).
      *   J wenn     geandert
          05 KKA-BLZ-ABRECH-KONTO           PIC X(08).
      *   BLZ der Abrechnungskonto-Nummer
          05 KKA-AEND-BLZ-ABRECH-KONTO-KZ   PIC X(01).
      *   J wenn     geandert
          05 KKA-FILIAL-NR                  PIC S9(04).
      *   Filial-Nummer der Abrechnungskonto-Nummer
          05 KKA-AEND-FILIAL-NR-KZ          PIC X(01).
      *   J wenn     geandert
          05 KKA-MAHNSTATUS                 PIC X(01).
      *   Mahnstatus
          05 KKA-AEND-MAHNSTATUS-KZ         PIC X(01).
      *   J wenn     geandert
          05 KKA-DATUM-EROEFFNUNG           PIC X(10).
      *   Eroeffnungsdatum
          05 KKA-AEND-DATUM-E-KZ            PIC X(01).
      *   J wenn     geandert
          05 KKA-EIGEN-VERM-KZ              PIC X(01).
      *   Eigenvermittelte-Kennzeichnung
          05 KKA-ABSCHLUSS-VBNR             PIC 9(07).
      *   VBNR, der den Vertrag abgeschlossen hat
          05 KKA-AKTUAL-KZ                  PIC X(01).
      *   I = INSERT
      *   U = UPDATE
      *   D = DELETE
241115    05  KKA-PROVISIONS-KZ             PIC  X(001).
101217    05  KKA-PIN                       PIC  9(009).
101217    05  KKA-AEND-PIN-KZ               PIC  X(001).
 
          05  KKA-LIMIT                     PIC  S9(006)V99.
          05  KKA-AEND-LIMIT-KZ             PIC  X(001).
      *       J = ÄNDERUNG LIMIT

101217*   05  KKA-RESERVE                    PIC X(069).
101217*   05  KKA-RESERVE                    PIC X(059).
270918    05  KKA-RESERVE                    PIC X(050).
      *           RESERVE



