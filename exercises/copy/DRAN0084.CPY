      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0084.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0084.CPv  $
      *
      *   Rev 2.3   Mar 12 2018 10:40:32   t13ae34
      *AAN01351: M_9000_2b - Löschung der Antragsdaten nach Widersp
      *ruch
      *
      *   Rev 2.2   Jan 17 2006 13:57:20   t13ae23
      *AAN00633: Automatisierung Testdatenbereitstellung Advocard D
      *FÜ-Sätze
      *
      *   Rev 2.1   Jul 25 2003 09:06:08   t13ae23
      *AAN00422 - Elektron. Antrag ADVOCARD -
      *
      *   Rev 2.0   Mar 18 2002 09:16:12   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0084  FUNKTIONEN STANDARD-OZP            *
      *                           ADVC-ADVOCARD TBAN0005             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00422!25.07.03!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN00633!17.01.06!2.2 ! FUNKTION DELETE HINZU     ! RAHNER   *
      * AAN01351!12.03.18!2.3 ! FUNKTION DELETE PRODUKTIV ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTAN0005.
           03 PI-ZTAN0005-FUNKTION            PIC X(008).
              88 PI-ZTAN0005-DRAN0076-INS         VALUE 'DRAN0076'.
              88 PI-ZTAN0005-DRAN0085-UPD         VALUE 'DRAN0085'.
              88 PI-ZTAN0005-DRAN0078-SEL-VN      VALUE 'DRAN0078'.
              88 PI-ZTAN0005-DRAN0079-SEL-VN-AV   VALUE 'DRAN0079'.
              88 PI-ZTAN0005-DRAN0077-SEL-PIN-A   VALUE 'DRAN0077'.
              88 PI-ZTAN0005-DRAN0075-SEL-PIN     VALUE 'DRAN0075'.
              88 PI-ZTAN0005-DRAN0074-DEL-VN      VALUE 'DRAN0074'.


