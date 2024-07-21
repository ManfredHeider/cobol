      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  07.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3625.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3625.CPv  $
      *
      *   Rev 2.2   Sep 11 2019 13:16:14   t13ex44
      *APA02814: Familieninterne DL-Nachfolge-Regelung: Übertragung
      * Praxisbestand
      *
      *   Rev 2.1   Mar 12 2019 15:04:52   t13ex44
      *APA02814: Familieninterne DL-Nachfolge-Regelung: Übertragung
      * Praxisbestand
      *
      *   Rev 2.0   Mar 07 2019 12:37:14   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI3625  FUNKTION LESEN LETZTEAENDERUNG     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    07.03.2019                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02814!07.03.19! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
        01 PI-DRKI3625.
           05 PI-DRKI3625-VERTRAGS-ID        PIC  X(013).
      *
        01 PO-DRKI3625.
           05 PO-DRKI3625-RC                 PIC  X(003).
              88 PO-DRKI3625-RC-OK           VALUE 'OK '.
              88 PO-DRKI3625-RC-NOT-FOUND    VALUE 'NFD'.
           05 PO-DRKI3625-LETZTEAENDERUNG    PIC  X(026).
      *
      ******** ENDE COPY-MEMBER (DRKI3625) *******************
 


