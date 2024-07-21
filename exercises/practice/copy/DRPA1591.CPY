      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1591.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1591.CPv  $
      *
      *   Rev 2.2   Dec 08 2014 09:23:48   t13ex67
      *APA01809: Projekt Integration FVD    //       Migrationsprog
      *ramm
      *
      *   Rev 2.1   Oct 12 2004 08:13:04   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.0   Jul 16 2004 14:43:58   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1591  FUNKTIONSAUFRUFE FÜR ZTPA0322      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    15.07.2004                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00218!15.07.04! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      * APA01809!24.07.14! 2.1! NEUE FUNKTION DRPA1797    ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
        01 PI-ZTPA0322.
           05 PI-ZTPA0322-FUNKTION     PIC  X(008).
              88 PI-ZTPA0322-DRPA1592-INS   VALUE 'DRPA1592'.
              88 PI-ZTPA0322-DRPA1593-DEL   VALUE 'DRPA1593'.
              88 PI-ZTPA0322-DRPA1594-SEL-ALL
                                            VALUE 'DRPA1594'.
              88 PI-ZTPA0322-DRAT0228-COM   VALUE 'DRAT0228'.
              88 PI-ZTPA0322-DRPA1797-UPD   VALUE 'DRPA1797'.

