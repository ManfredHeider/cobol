      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0074.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0074.CPv  $
      *
      *   Rev 2.3   Mar 12 2018 14:25:24   t13ae34
      *AAN01351: M_9000_2b - Löschung der Antragsdaten nach Widersp
      *ruch
      *
      *   Rev 2.2   Mar 12 2018 10:40:04   t13ae34
      *AAN01351: M_9000_2b - Löschung der Antragsdaten nach Widersp
      *ruch
      *
      *   Rev 2.1   Jan 17 2006 13:57:12   t13ae23
      *AAN00633: Automatisierung Testdatenbereitstellung Advocard D
      *FÜ-Sätze
      *
      *   Rev 2.0   Mar 18 2002 09:15:32   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0074  FUNKTION DELETE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00633!17.01.06!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN01351!12.03.18!2.2 ! DELETE PRODUKTIV          ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0074                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE TABELLE ADVC TBAN0005                         *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0074.
           05 PI-DRAN0074-ADVC-KEY.
               10 PI-DRAN0074-PGEB-GES          PIC S9(4) COMP.
               10 PI-DRAN0074-ADVC-HVANTNUM     PIC X(9).
      *
       01  PO-DRAN0074.
           05 PO-DRAN0074-RC                    PIC  X(003).
             88 PO-DRAN0074-RC-OK               VALUE 'OK '.
             88 PO-DRAN0074-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************


