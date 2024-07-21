      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0911.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0911.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:33:58   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   10 Mar 1998 15:35:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0911                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<DATEN SPEICHERN NACH äNDERUNGEN IN PEG>> *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHRIEBER                                        *
      *    DATUM:    01.03.98                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!01.03.98! 01 ! NEU                       !SCHRIEBER *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0911                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0911.
          05 PI-DRPA0911-DATEN.
             10 PI-DRPA0911-UFKT                    PIC  9(002).
             10 PI-DRPA0911-PMPG-ID                 PIC S9(015) COMP-3.
             10 PI-DRPA0911-PMPG-VERSION            PIC S9(004) COMP.
             10 PI-DRPA0911-PDTA-ID                 PIC S9(015) COMP-3.
             10 PI-DRPA0911-PMPG-LFD-NR-VNR         PIC S9(004) COMP.
             10 PI-DRPA0911-PMPG-VSTAT              PIC  X(004).
             10 PI-DRPA0911-PMPG-INHALT-LEN         PIC S9(004) COMP.
             10 PI-DRPA0911-PMPG-INHALT             PIC X(3900).
             10 PI-DRPA0911-PMPG-LAST-UPDATE        PIC X(26).
             10 PI-DRPA0911-PMPG-KENNUNG            PIC X(08).
      *
       01 PO-DRPA0911.
          05 PO-DRPA0911-RC                         PIC  X(003).
             88 PO-DRPA0911-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0911-RC-NOT-OK                     VALUE 'NOK'.
          05 PO-DRPA0911-FEHLER-ORT                 PIC  9(001).
      *
      ******* ENDE COPY-MEMBER **************************************
