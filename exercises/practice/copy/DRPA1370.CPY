      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1370.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/MF/CPY/DRPA1370.CPv  $
      *
      *   Rev 2.4   Jul 18 2002 10:56:18   t13ex43
      *APA00206 : PEG S-Bausparkasse
      *
      *   Rev 2.3   30 Mar 2001 07:13:00   T13EX22
      *APA00131: PEG-BfG-Bank
      *
      *   Rev 2.2   09 Mar 2001 12:10:12   T13EX21
      *APA00131 - Neue Funktion DRPA1477
      *
      *   Rev 2.1   Apr 13 2000 16:28:18   T13EX21
      *APA00067 - Neuerstellung
      *
      *   Rev 2.0   Mar 01 2000 09:58:20   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1370  FUNKTION                           *
      *                 "BADNIA PM FORMAL PRÜFEN UND NORMIEREN"      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    12.04.2000                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00067!12.04.00!2.1 ! NEUERSTELLUNG             ! STORCH   *
      * APA00131!28.02.01!2.2 ! NEUE FUNKTION DRPA1477    ! STORCH   *
      * APA00131!09.03.01!2.3 ! NEUE FUNKTION DRPA1479    ! MUND     *
      * APA00206!19.06.02!2.4 ! NEUE FUNKTION DRPA1542    ! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UWPA0045.
           05 PI-UWPA0045-FUNKTION            PIC X(008).
              88 PI-UWPA0045-DRPA1365             VALUE 'DRPA1365'.
280201        88 PI-UWPA0045-DRPA1477             VALUE 'DRPA1477'.
090301        88 PI-UWPA0045-DRPA1479             VALUE 'DRPA1479'.
190602        88 PI-UWPA0045-DRPA1542             VALUE 'DRPA1542'.
      *
      ****** ENDE COPY-MEMBER ****************************************
