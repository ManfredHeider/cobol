      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  16.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1906.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1906.CPv  $
      *
      *   Rev 2.1   Sep 26 2018 12:42:46   t13ex67
      *APA02676: Daisy Auskunftsdialog - Ges. 20 - zulässige Produk
      *tschlüssel zu PGV´s
      *
      *   Rev 2.0   Aug 16 2018 09:33:54   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1906 FUNKTION LIST PRODUKT-PGV-          *
      *                          ZULÄASSIGKEIT FÜR DIALOG SF46       *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: M. STORCH-SCHAEFER                              *
      *    DATUM:    16.08.2018                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02676!16.08.18!2.01! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1906                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1906.
          05 PI-DRPA1906-UFKT                    PIC  9(02).
          05 PI-DRPA1906-AUSWAHL-KRITERIEN.
               10 PI-DRPA1906-PGEB-GES             PIC 9(002).
               10 PI-DRPA1906-PROD-SCHL            PIC S9(004) BINARY.
               10 PI-DRPA1906-S-PROD-SCHL          PIC X(001).
               10 PI-DRPA1906-PGV                  PIC S9(004) BINARY.
               10 PI-DRPA1906-S-PGV                PIC X(001).
      *
       01 PO-DRPA1906.
          05  PO-DRPA1906-RC                     PIC  X(003).
              88 PO-DRPA1906-RC-OK                        VALUE 'OK '.
              88 PO-DRPA1906-RC-EOF                       VALUE 'EOF'.
              88 PO-DRPA1906-RC-LEER                      VALUE 'LER'.

          05 PO-DRPA1906-ERGEBNISZEILE.
             35 KNR-PGV                          PIC S9(004) BINARY.
             35 PPGZ-GUELTIG-AB                  PIC  X(010).
             35 PROD-PROD-SCHL                   PIC S9(004) BINARY.
      *
      ******** ENDE COPY-MEMBER **************************************


