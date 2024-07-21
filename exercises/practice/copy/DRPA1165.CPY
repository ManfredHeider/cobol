      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1165.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1165.CPv  $
      *
      *   Rev 2.0   May 26 1999 16:02:12   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   21 Oct 1998 14:02:34   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1165  FUNKTION UPDATE ZUGRIFFS-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: LÜttges                                         *
      *    DATUM:    08.02.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!08.02.99! 01 ! NEUERSTELLUNG             ! LÜTTGES  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1165                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE BUCHUNGS-STAND <<<   TABELLE PDPT TBPA0122              *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1165.
          05 PI-DRPA1165-PDPT-LAST-UPDATE          PIC  X(026).
          05 PI-DRPA1165-PDPT-KEY.
           10 PI-DRPA1165-PDPT-ID             PIC S9(15)V USAGE COMP-3.
          05 PI-DRPA1165-PDPT-DATEN.
           10 PI-DRPA1165-KNR-BUCHUNGS-STAND  PIC S9(4) USAGE COMP.
      *
       01 PO-DRPA1165.
          05 PO-DRPA1165-RC                   PIC  X(003).
             88 PO-DRPA1165-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1165-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1165-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
