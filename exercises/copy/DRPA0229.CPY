      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0229.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:/MF/CPY/DRPA0229.CPv  $
      *
      *   Rev 2.0   12 Sep 2001 13:52:18   t13ae46
      *APA00144
      *
      *   Rev 1.0   Feb 19 1997 12:12:20   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0229                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE 'DTA-SUCHEN MIT VSTAT OHNE GESELLSCHAFT'     *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J. WANG                                          *
      *    DATUM:    10.08.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00144!10.08.01!2.0 ! NEUERSTELLUNG             ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      ****************************************************************
      *
       01 PI-DRPA0229.
          05 PI-DRPA0229-KNR-DTA-ART             PIC S9(004) COMP.
          05 PI-DRPA0229-PDTA-VSTAT              PIC  X(004).
      *
       01 PO-DRPA0229.
          05 PO-DRPA0229-RC                      PIC  X(003).
             88 PO-DRPA0229-RC-OK                     VALUE 'OK '.
             88 PO-DRPA0229-RC-NOT-FOUND              VALUE 'NFD'.

         05 PO-DRPA0229-DATEN.
            10 PO-DRPA0229-PDTA-ID               PIC S9(015)V COMP-3.
            10 PO-DRPA0229-PGEB-GES              PIC S9(004) COMP.
      ******* ENDE COPY-MEMBER **************************************
      *
