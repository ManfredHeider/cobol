      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  04.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3624.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3624.CPv  $
      *
      *   Rev 2.3   Jun 25 2019 09:14:10   t13ex39
      *AKI01670: Deutsche Bank - Erweiterung der Satzlängen für die
      * DTA (Kontokorrent und Kundenstamm)
      *
      *   Rev 2.2   Jun 04 2019 14:45:20   t13ex39
      *AKI01670: Deutsche Bank - Erweiterung der Satzlängen für die
      * DTA (Kontokorrent und Kundenstamm)
      *
      *   Rev 2.1   Mar 14 2019 14:45:30   t13ex39
      *AKI01670: Deutsche Bank - Erweiterung der Satzlängen für die
      * DTA (Kontokorrent und Kundenstamm)
      *
      *   Rev 2.0   Mar 04 2019 15:05:44   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für KUNDEN-/VERTRAGSDATEN VON DB              *
      *                                                                *
      *    SATZLÄNGE   450                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01670!17.04.19! Neuerstellung                    !WHO        *
      * ---------------------------------------------------------------*
      *        !        !                                  !           *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
CCC009*    SKIP1
       01  DRKI3624.

               05  'XX'-GES                 PIC  9(002).
               05  'XX'-VNR                 PIC  9(010).

               05  'XX'-FILIALE-ART         PIC  X(035).
      *            Art der Filiale
               05  'XX'-FILIALE-NAME        PIC  X(035).
      *            Name der Filiale
               05  'XX'-FILIALE-STR         PIC  X(035).
      *            Adresse (Straße und Hausnr.)        .
               05  'XX'-FILIALE-ORT         PIC  X(035).
      *            Adresse (PLZ und Ort)
               05  'XX'-AKTUAL-KZ           PIC  X(001).

               05  FILLER                   PIC  X(005).


