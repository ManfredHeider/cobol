      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.11.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3114.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3114.CPv  $
      *
      *   Rev 2.2   Jan 11 2017 12:19:10   t13ac17
      *AKI01466: Aufbereitung
      *
      *   Rev 2.1   Dec 07 2016 13:26:08   t13ac17
      *AKI01466: Aufbereitung
      *
      *   Rev 2.0   Nov 02 2016 15:28:26   t13ac17
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    Copy-Member für die Aktualisieung der KVPERSONEN           *
      *    BKK Linde - G51                                            *
      *                                                               *
      *---------------------------------------------------------------*
      *    LOGBUCH                                                    *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01466!03.11.16! 2.1! Neuerstellung             ! Walbert   *
      *         !19.12.16! 2.2! Entfernen Feld            !           *
      *         !        !    ! ABRECHWIDERRUFDAT-VP      !           *
      * --------------------------------------------------------------*
      *         !        !    !                           !           *
      *****************************************************************
      *    Verarbeitungskennzeichen  I = Insert U = Update
           05 'XX'-VERARB-KZN                   PIC  X.
      *    Die Gesellschaftsnummer, sollte immer die 51 sein
           05 'XX'-GES                          PIC  9(002).
      *    Vertragsnummer
           05 'XX'-VERNR                        PIC  X(015).
      *    laufende Nummer VP
           05 'XX'-LFDNR-VP                     PIC  9(002).
      *    Produkt-ID Vertrag (5 = gesetzl. Pfli., 6 = gesetzl. freiw.)
           05 'XX'-PRODUKT-ID                   PIC S9(004).
      *    Anredeschlüssel VP
           05 'XX'-ANREDE-VP                    PIC S9(002).
      *    Nachname VP
           05 'XX'-NACHNAME-VP                  PIC  X(030).
      *    Vorname VP
           05 'XX'-VORNAME-VP                   PIC  X(030).
      *    Geburtsdatum VP
           05 'XX'-GEBDAT-VP                    PIC  X(010).
      *    Versicherungsbeginn Mitversicherung
           05 'XX'-BEGDAT-VP                    PIC  X(010).
      *    Versichrungsende    Mitversicherung
           05 'XX'-ENDDAT-VP                    PIC  X(010).
      *    Vorversicherungsgesellschaft (nur bei VN)
           05 'XX'-VORVERSGES                   PIC  9(005).
      *    Vorversicherung Kuendigungsbestaetigung (nur bei VN)
           05 'XX'-VORVERSKUENDKZN              PIC  X(001).
      *    Datum zu der die Vorversicherung gekündigt wurde (nur bei VN)
           05 'XX'-VORVERSDATKUEND              PIC  X(010).
      *    Versicherungsart auf Personenebene, mitversichert im
      *       Rahmen der Familienversicheung J oder 0)
           05 'XX'-VERSART-VP                   PIC  X(001).
      *    Kooperationsservice
      *       Teilnahme an der vereinfachten Abrechnung Ja/Nein
           05 'XX'-ABRECHKZN-VP                 PIC  X(001).
      *    Widerrufsdatum vereinfachte Abrechnung
           05 'XX'-ABRECHWIDERRUFDAT-VP         PIC  X(010).
      *    Rahmenvertrag Zusatzversicherung J oder N
           05 'XX'-RAHMENZUSATZVERS-VP          PIC  X(001).
 

