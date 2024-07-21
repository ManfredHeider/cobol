      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1298.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1298.CPv  $
      *
      *   Rev 2.1   Aug 31 1999 14:05:38   T13EX21
      *APA00053 - Neuerstellung
      *
      *   Rev 2.0   Mar 30 1999 08:44:28   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1298                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PMPE UND PMPP >>                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. STORCH                                        *
      *    DATUM:    24.08.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00053!24.08.99!    ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1298                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1298.
          05 PI-DRPA1298-PROGRAMM            PIC X(008).
          05 PI-DRPA1298-UFKT                PIC 9(002).
          05 PI-DRPA1298-DATEN.
             10 PI-DRPA1298-PDTA-ID          PIC S9(15)V USAGE COMP-3.
      *
       01 PO-DRPA1298.
          05 PO-DRPA1298-RC                  PIC X(003).
             88 PO-DRPA1298-RC-OK                VALUE 'OK '.
             88 PO-DRPA1298-RC-EOF               VALUE 'EOF'.
             88 PO-DRPA1298-RC-LEER              VALUE 'LER'.
          05 PO-DRPA1298-DATEN.
             10 PO-DRPA1298-PGEB-GES          PIC S9(04) COMP.
             10 PO-DRPA1298-PMPE-VNR          PIC X(15).
             10 PO-DRPA1298-PMPE-FAELLIG-VON  PIC X(10).
             10 PO-DRPA1298-PMPP-PG-GSMT-PROV PIC 9(7)V99 COMP-3.
             10 PO-DRPA1298-PMPP-PGV          PIC 9(04).
