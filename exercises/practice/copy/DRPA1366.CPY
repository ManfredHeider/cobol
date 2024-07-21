      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1366.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1366.CPv  $
      *
      *   Rev 2.1   Apr 14 2000 15:54:06   T13EX23
      *APA00093 - Neue Kopiestrecke 
      *
      *   Rev 2.0   Mar 01 2000 09:58:16   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1366                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE 'SUCHE IN PDTA MIT GESELLSCHAFT UND VSTAT'   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J. GRAY                                          *
      *    DATUM:    05.04.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00093!05.04.00! 01 ! NEUERSTELLUNG             ! GRAY     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *
      ****************************************************************
      *
       01 PI-DRPA1366.
          05 PI-DRPA1366-PGEB-GES                PIC S9(004) COMP.
          05 PI-DRPA1366-PDTA-VSTAT              PIC  X(004).
      *
       01 PO-DRPA1366.
          05 PO-DRPA1366-RC                      PIC  X(003).
             88 PO-DRPA1366-RC-OK                     VALUE 'OK '.
             88 PO-DRPA1366-RC-NOT-FOUND              VALUE 'NFD'.

         05 PO-DRPA1366-DATEN.
            10 PO-DRPA1366-PDTA-ID               PIC S9(015)V COMP-3.
      ******* ENDE COPY-MEMBER **************************************
