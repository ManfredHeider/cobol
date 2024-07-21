      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  25.10.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1912.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1912.CPv  $
      *
      *   Rev 2.2   Jun 21 2019 08:00:10   t13ex67
      *APA02573: DSGVO - Dialoge MSS
      *
      *   Rev 2.1   May 09 2019 13:02:22   t13ex67
      *APA02573: DSGVO - Dialoge MSS
      *
      *   Rev 2.0   Oct 25 2018 10:23:44   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1912  FUNKTION                           *
      *                           "PRÜFEN, OB VNR GESPERRT IST"      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: 25.10.2018                                      *
      *    DATUM:    25.10.2018                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME    *
      * -------------------------------------------------------------*
      * APA02573!25.10.18! 2.01! NEU                       ! STORCH  *
      *         !20.05.19! 2.02! NEUES PO-FELD             ! STORCH  *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA1912.
          05 PI-DRPA1912-PGEB-GES             PIC S9(004) BINARY.
          05 PI-DRPA1912-VNR                  PIC  X(015).

      ***************************************************************
       01 PO-DRPA1912.
          05 PO-DRPA1912-RC                   PIC  X(003).
             88 PO-DRPA1912-RC-OK             VALUE 'OK '.
             88 PO-DRPA1912-RC-NOT-FOUND      VALUE 'NFD'.
          05 PO-DRPA1912-DATEN.
              10 PO-DRPA1912-GESPERRT-KZ      PIC  X(001).
              10 PO-DRPA1912-FREI-BIS         PIC  X(010).
              10 PO-DRPA1912-DAISY-KI-KZ      PIC  X(001).
      *
      ******** ENDE COPY-MEMBER (DRPA1912) *******************


