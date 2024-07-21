      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1612.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1612.CPv  $
      *
      *   Rev 2.4   Oct 08 2014 15:18:48   t13ex67
      *APA01702: Erweiter. WEPA0008 um Historien-Tab. (TBPA0313/315
      */317/322)
      *
      *   Rev 2.3   Feb 28 2014 14:42:28   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.2   Feb 28 2014 14:37:40   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.1   Oct 12 2004 08:16:08   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.0   Jul 30 2004 13:09:10   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1612  FUNKTIONSAUFRUFE FÜR ZXPA0315      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    15.07.2004                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00218!15.07.04! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      * APA01702!11.12.13! 2.1! NEUE FUNKTION DRPA1783    ! STRAUSS  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
        01 PI-ZXPA0315.
           05 PI-ZXPA0315-FUNKTION     PIC  X(008).
               88 PI-ZXPA0315-DRPA1613      VALUE 'DRPA1613'.
               88 PI-ZXPA0315-DRPA1783      VALUE 'DRPA1783'.
