      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0993.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0993.CPv  $
      *
      *   Rev 2.1   May 28 1999 18:31:06   T13EX26
      *Prod-Übergabe 28.0598 hpa
      *
      *   Rev 2.0   May 26 1999 16:58:16   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:21:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0993  FUNKTION DGV/PRODART FÜR NEUTR. PR.*
      *                           ( FKPAW064 )                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    06.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY   !06.07.98! 01 ! NEUERSTELLUNG             ! PARENT   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRPA0993.
          05 PI-DRPA0993-DATEN.
             10 PI-DRPA0993-GB-DGV                  PIC S9(004) BINARY.
             10 PI-DRPA0993-GB-PRODART              PIC S9(004) BINARY.
             10 PI-DRPA0993-UB-DGV                  PIC S9(004) BINARY.
             10 PI-DRPA0993-UB-PRODART              PIC S9(004) BINARY.
      *
       01 PO-DRPA0993.
          05 PO-DRPA0993-RC                         PIC  X(003).
             88 PO-DRPA0993-RC-NEUTRAL-ZULAES             VALUE 'NZU'.
             88 PO-DRPA0993-RC-NEUTRAL-UNZUL              VALUE 'NUZ'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
