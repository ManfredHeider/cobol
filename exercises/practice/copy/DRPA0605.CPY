      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0605.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0605.CPv  $
      *
      *   Rev 2.1   Nov 25 1999 09:15:24   T13EX21
      *APA00063 - Neuerstellung
      *
      *   Rev 2.0   Feb 27 1999 14:29:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:54:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0605                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE 'SUCHE IN PDTA MIT GESELLSCHAFT, DTA-ART     *
      *                   UND ZUSTAND'                                *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. STORCH                                        *
      *    DATUM:    06.10.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00063!06.10.99! 01 ! NEUERSTELLUNG             ! STORCH   *
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
       01 PI-DRPA0605.
          05 PI-DRPA0605-PGEB-GES                PIC S9(004) COMP.
          05 PI-DRPA0605-KNR-DTA-ART             PIC S9(004) COMP.
          05 PI-DRPA0605-PDTA-VSTAT              PIC  X(004).
      *
       01 PO-DRPA0605.
          05 PO-DRPA0605-RC                      PIC  X(003).
             88 PO-DRPA0605-RC-OK                     VALUE 'OK '.
             88 PO-DRPA0605-RC-NOT-FOUND              VALUE 'NFD'.

         05 PO-DRPA0605-DATEN.
            10 PO-DRPA0605-PDTA-ID               PIC S9(015)V COMP-3.
      ******* ENDE COPY-MEMBER **************************************
