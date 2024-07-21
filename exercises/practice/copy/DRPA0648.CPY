      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0648.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0648.CPv  $
      *
      *   Rev 2.0   May 28 1999 13:24:58   T01JM2E
      *APA00042 NT-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:57:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0648                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF LIST ZU PMPG                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    24.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!24.11.97!    ! NEU                       ! D.WILHELM*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0648                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0648.
           05  PI-DRPA0648-UFKT                PIC  9(02).
           05  PI-DRPA0648-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA0648-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA0648-PMPG-ID             PIC  S9(15) COMP-3.
      *
       01 PO-DRPA0648.
          05 PO-DRPA0648-RC                  PIC  X(003).
             88 PO-DRPA0648-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0648-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA0648-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA0648-ANZAHL-ERG-ZEILEN    PIC 9(5).
          05 PO-DRPA0648-ERGEBNISZEILE.
             10 PO-DRPA0648-PMPG-VERSION      PIC 9(04).
             10 PO-DRPA0648-PMPG-VNR          PIC 9(15).
             10 PO-DRPA0648-PMPG-PIN          PIC 9(07).
             10 PO-DRPA0648-PMPG-LFD-NR-VNR   PIC 9(09).
             10 PO-DRPA0648-PMPG-ID           PIC 9(15).
