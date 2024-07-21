      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1616.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1616.CPv  $
      *
      *   Rev 2.2   Oct 08 2014 15:19:00   t13ex67
      *APA01702: Erweiter. WEPA0008 um Historien-Tab. (TBPA0313/315
      */317/322)
      *
      *   Rev 2.1   Oct 12 2004 08:16:44   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.0   Jul 30 2004 13:10:04   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1616  FUNKTIONSAUFRUFE FÜR ZXPA0322      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    15.07.2004                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00218!15.07.04! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      * APA01702!11.12.13! 2.1! NEUE FUNKTION DRPA1785    ! STRAUSS  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
        01 PI-ZXPA0322.
           05 PI-ZXPA0322-FUNKTION     PIC  X(008).
               88 PI-ZXPA0322-DRPA1617      VALUE 'DRPA1617'.
               88 PI-ZXPA0322-DRPA1785      VALUE 'DRPA1785'.
