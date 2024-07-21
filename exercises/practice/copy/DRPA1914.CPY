      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  07.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1914.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1914.CPv  $
      *
      *   Rev 2.2   Jul 19 2019 14:35:56   t13ex44
      *APA02814: Familieninterne DL-Nachfolge-Regelung: Übertragung
      * Praxisbestand
      *
      *   Rev 2.1   Mar 12 2019 15:06:22   t13ex44
      *APA02814: Familieninterne DL-Nachfolge-Regelung: Übertragung
      * Praxisbestand
      *
      *   Rev 2.0   Mar 07 2019 14:49:42   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1914  FUNKTION LESEN LETZTEAENDERUNG     *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: 07.03.2019                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME    *
      * -------------------------------------------------------------*
      * APA02814!07.03.19!2.01! NEU                        ! SCHEIDEL*
      * DSY04239!02.12.21!    ! NEUE RC KVT HINZU          ! STORCH  *
      *         !        !    !                            !         *
      *         !        !    !                            !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA1914.
          05 PI-DRPA1914-PGEB-GES             PIC S9(004) BINARY.
          05 PI-DRPA1914-VNR                  PIC  X(015).
 
       01 PO-DRPA1914.
          05 PO-DRPA1914-RC                   PIC  X(003).
             88 PO-DRPA1914-RC-OK             VALUE 'OK '.
             88 PO-DRPA1914-RC-NOT-FOUND      VALUE 'NFD'.
021221       88 PO-DRPA1914-RC-KEIN-VERTRAG   VALUE 'KVT'.
          05 PO-DRPA1914-DATEN.
              10 PO-DRPA1914-LETZTEAENDERUNG  PIC  X(026).
      *
      ******** ENDE COPY-MEMBER (DRPA1914) *******************


