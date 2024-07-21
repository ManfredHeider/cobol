      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  25.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3707.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3707.CPv  $
      *
      *   Rev 2.3   Nov 14 2019 08:17:54   t13ac20
      *AKI01737: AMVers - Sach: Erweiterung der Tabelle "FlexN-Boos
      *ter-Potential": Source
      *
      *   Rev 2.2   Aug 23 2019 13:21:20   t13ac20
      *AKI01702: AMVers - Sach: FlexN-Booster Potential regelmäßige
      * Auswertung Erfolgskontrolle: Source
      *
      *   Rev 2.1   Jul 05 2019 08:17:28   t13ac20
      *AKI01690: Datenbereitstellung für Potentialabfrage FlexN-Boo
      *ster für den VB: Source
      *
      *   Rev 2.0   Jun 25 2019 12:13:40   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *    FLEXN-BOOSTER POTENTIAL GRUNDLAGE VERTRAG
      *
      *    SATZLÄNGE VARIABEL (125 BYTES)
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01690!25.06.19!2.1 ! NEU                       ! T13AC20
      * AKI01702!15.08.19!2.2 ! NEUES FELD LV-VOR-KZN     ! T13AC20
      * AKI01737!13.11.19!2.3 ! NEUE FELDER               ! T13AC20
      *         !        !    !    ZAHLUNGSWEISEID-VSP    !
      *         !        !    !    GESAMTBEITRAG-VSP      !
      *         !        !    !                           !
      *         !        !    !                           !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *
           05 DRKI3707.
              10 GRUNDLAGE-KUNDE.
                 15 KIPERSONENID                 PIC  X(013).
                 15 VERSICHRISIKO1-4             PIC  X(004).
                 15                              PIC  X(002).
                 15 VSP-KZN                      PIC  X(001).
                 15                              PIC  X(002).
                 15 KFZ-KZN                      PIC  X(001).
                 15                              PIC  X(002).
                 15 WG-KZN                       PIC  X(001).
                 15                              PIC  X(002).
                 15 KV-KZN                       PIC  X(001).
                 15                              PIC  X(002).
                 15 VASP-KZN                     PIC  X(001).
                 15                              PIC  X(002).
                 15 SK-KZN                       PIC  X(001).
                 15                              PIC  X(002).
                 15 SP-KZN                       PIC  X(001).
                 15                              PIC  X(002).
                 15 RIESTER-KZN                  PIC  X(001).
                 15                              PIC  X(002).
                 15 LV-VOR-KZN                   PIC  X(001).
                 15 KIPERSONENID-LES             PIC  X(026).
                 15 ZAHLUNGSWEISEID-VSP          PIC  9(002).
                 15 GESAMTBEITRAG-VSP            PIC  9(009)V99.
                 15                              PIC  X(001).
              10 GRUNDLAGE-VERTRAG.
                 15 VERTRAGSID                   PIC  X(013).
                 15 PARTGESELLSCHID              PIC S9(004) BINARY.
                 15 ZAHLUNGSWEISEID              PIC S9(004) BINARY.
                 15                              PIC  X(001).
                 15 GESAMTBEITRAG                PIC S9(009)V99
                                                     PACKED-DECIMAL.
                 15                              PIC  X(001).
                 15 VERTRAGSNUMMER               PIC  X(015).
                 15                              PIC  X(001).

