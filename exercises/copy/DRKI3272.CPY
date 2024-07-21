      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  19.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3272.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3272.CPv  $
      *
      *   Rev 2.2   Dec 15 2020 15:31:44   t13ae31
      *AKI01812: G21 Leben Basis Viridium Programme
      *
      *   Rev 2.1   Apr 13 2018 10:13:50   t13ex19
      *AKI01570: FW - Generali-Partnergesellschaften - Basisdaten-V
      *ertrag
      *
      *   Rev 2.0   Feb 19 2018 16:58:00   t13ex19
      *Archiverstellung
      *
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEM ERMITTLUNGSPROGRAMM PMKI1374            *
      *     - Ausgabedatei GEN-Änderungsdaten Vertrag                 *
      *                                                               *
      *    SATZLÄNGE   300                                            *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      AENDERUNG                        !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *20.02.18!NEUERSTELLUNG                          !BST      !2.0  *
      *--------!---------------------------------------!---------!-----*
      *18.11.20!Änderung zu Viridium neues Feld        !HHU      !     *
      *AKI01812! MIG-KZ ersetzt AKZ und Wagnis         !         !     *
      *        !                                       !         !     *
      ******************************************************************
      *****************************************************************

      *    Verarbeitungskennzeichen I=Insert U=Update
           05  'XX'-VERARBKZ                       PIC  X(001).
      *    Gesellschaft 11=Sach 21=Leben  42=Invest
           05  'XX'-GES-NR                         PIC  9(002).
      *    Vertragsnummer
           05  'XX'-VERTRNR                        PIC  9(015).
      *    Einzelproduktnummer
           05  'XX'-EZPNR                          PIC  9(004).
      *    VB-Nummer
           05  'XX'-VBNR                           PIC  9(007).
           05  'XX'-VBNR-AEND                      PIC  X(001).
      *    Datum Vertragsbeginn
           05  'XX'-BEGINN-DAT                     PIC  9(008).
           05  'XX'-BEGINN-DAT-AEND                PIC  X(001).
      *    Datum Vertragsende
           05  'XX'-ENDE-DAT                       PIC  9(008).
           05  'XX'-ENDE-DAT-AEND                  PIC  X(001).
      *    Vertragszustand
           05  'XX'-VERTRZUSTAND                   PIC  9(002).
           05  'XX'-VERTRZUSTAND-AEND              PIC  X(001).
      *    Beitrag
           05  'XX'-BEITRAG                        PIC  9(008)V99.
           05  'XX'-BEITRAG-AEND                   PIC  X(001).
      *    Jahresnettobeitrag
           05  'XX'-JNBEITRAG                      PIC  9(008)V99.
           05  'XX'-JNBEITRAG-AEND                 PIC  X(001).
      *    Zahlweise
           05  'XX'-ZAHLWEISE                      PIC  X(002).
           05  'XX'-ZAHLWEISE-AEND                 PIC  X(001).
      *    Generali-Sparte
           05  'XX'-SPARTE                         PIC  X(003).
           05  'XX'-SPARTE-AEND                    PIC  X(001).
      *    Produkt
           05  'XX'-PRODUKT                        PIC  X(050).
           05  'XX'-PRODUKT-AEND                   PIC  X(001).
      *    Tarif
           05  'XX'-TARIF                          PIC  X(050).
           05  'XX'-TARIF-AEND                     PIC  X(001).
      *    Summe
           05  'XX'-SUMME                          PIC  9(008)V99.
           05  'XX'-SUMME-AEND                     PIC  X(001).
      *    Amtliches Kennzeichen
           05   FILLER                             PIC  X(015).
      *    Migrationskz P = Viridium sonst SPACE
           05  'XX'-MIG-KZ                         PIC X(001).
           05  'XX'-MIG-KZ-AEND                    PIC X(001).
      *    Guthaben
           05  'XX'-GUTHABEN                       PIC  9(008)V99.
           05  'XX'-GUTHABEN-AEND                  PIC  X(001).
      *    aktueller Vertragswert
           05  'XX'-VERTRWERT                      PIC  9(008)V99.
           05  'XX'-VERTRWERT-AEND                 PIC  X(001).
      *    Kennzeichen Rentenbezug
           05  'XX'-RENTENBEZUG                    PIC  X(001).
           05  'XX'-RENTENBEZUG-AEND               PIC  X(001).
      *    voraussichtliche Rente
           05  'XX'-VORAUS-RENTE                   PIC  9(006).
           05  'XX'-VORAUS-RENTE-AEND              PIC  X(001).
      *    garantierte Rente
           05  'XX'-GARANT-RENTE                   PIC  9(006).
           05  'XX'-GARANT-RENTE-AEND              PIC  X(001).
      *    Datum der Berechnung Leben
           05  'XX'-BERECH-DAT                     PIC  9(008).
           05  'XX'-BERECH-DAT-AEND                PIC  X(001).
      *    Geschäftszweig
           05  'XX'-GESCHAEFTSZWEIG                PIC  X(002).
           05  'XX'-GESCHAEFTSZWEIG-AEND           PIC  X(001).
      *    Fläche
           05  'XX'-FLAECHE                        PIC  9(005).
           05  'XX'-FLAECHE-AEND                   PIC  X(001).
      *    Produkt/Produktkombinummer
           05  'XX'-PRODUKT-KOMBI-NR               PIC  9(005).
           05  'XX'-PRODUKT-KOMBI-NR-AEND          PIC  X(001).
      *    Reserve
           05  Filler                              PIC  X(028).

      *---------------------------------------------------------------*


