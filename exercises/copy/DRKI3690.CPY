      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  19.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3690.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI3690.CPv  $
      *
      *   Rev 2.5   Feb 17 2020 14:26:04   t13ac20
      *AKI01758: AMVers - Sach: FlexN-Booster Potential (Technische
      * Nacharbeiten um Architekturrichtlinien gerade zu ziehen) - 
      *
      *
      *   Rev 2.4   Nov 18 2019 10:52:02   t13ac20
      *AKI01737: AMVers - Sach: Erweiterung der Tabelle "FlexN-Boos
      *ter-Potential"
      *
      *   Rev 2.3   Nov 14 2019 08:15:00   t13ac20
      *AKI01737: AMVers - Sach: Erweiterung der Tabelle "FlexN-Boos
      *ter-Potential": Source
      *
      *   Rev 2.2   Aug 23 2019 13:14:54   t13ac20
      *AKI01702: AMVers - Sach: FlexN-Booster Potential regelmäßige
      * Auswertung Erfolgskontrolle: Source
      *
      *   Rev 2.1   Jul 05 2019 08:14:32   t13ac20
      *AKI01690: Datenbereitstellung für Potentialabfrage FlexN-Boo
      *ster für den VB: Source
      *
      *   Rev 2.0   Jun 19 2019 16:04:30   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *    FLEXN-BOOSTER POTENTIAL SST
      *
      *    SATZLÄNGE 180 BYTES
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01690!19.06.19!2.1 ! NEU                       ! T13AC20
      * AKI01702!16.08.19!2.2 ! NEUES FELD LV-VOR-KZN     ! T13AC20
      * AKI01737!13.11.19!2.3 ! NEUE FELDER               ! T13AC20
      *         !        !    !    ZAHLUNGSWEISEID-VSP    !
      *         !        !    !    GESAMTBEITRAG-VSP      !
      * AKI01737!18.11.19!2.4 ! KORREKTUR AUSPRÄGUNG      ! T13AC20
      *         !        !    !    GESAMTBEITRAG-VSP      !
      * AKI01758!21.01.20!2.5 ! Hinzufügen neues Feld     ! T13AC23
      *         !        !    ! + alte auf COMP setzen    !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *
           05 DRKI3690.
              10 KIPERSONENID-LES                PIC  X(026).
              10 VBPERSONENID-LES                PIC  X(026).
              10 STICHTAG                        PIC  X(026).
              10 VSP-KZN                         PIC  X(001).
              10 PROD-KOMBI-NR                   PIC  X(004).
              10 KFZ-KZN                         PIC  X(001).
              10 WG-KZN                          PIC  X(001).
              10 KV-KZN                          PIC  X(001).
              10 KV-BOOST-KZN                    PIC  X(001).
              10 VASP-KZN                        PIC  X(001).
              10 SK-KZN                          PIC  X(001).
              10 SP-KZN                          PIC  X(001).
              10 RIESTER-KZN                     PIC  X(001).
              10 LV-BOOST-KZN                    PIC  X(001).
              10 BOOSTER-BEWERTUNG               PIC  X(001).
              10 KNDNUMMER                       PIC S9(009) COMP.
              10 ANZNAME                         PIC  X(030).
              10 ANZVORNAME                      PIC  X(030).
              10 MITARBEITERNR                   PIC S9(009) COMP.
              10 LV-VOR-KZN                      PIC  X(001).
              10 ZAHLUNGSWEISEID-VSP             PIC S9(004) COMP.
      *       AUFBEREITUNG GESAMTBEITRAG-VSP IST MIT DEZIMALPUNKT
              10 GESAMTBEITRAG-VSP               PIC  X(012).
              10 LAUFENDE-NR                     PIC S9(009) COMP.



