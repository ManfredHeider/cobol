      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1350.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1350.CPv  $
      *
      *   Rev 2.1   Apr 04 2000 08:47:00   T13EX21
      *APA00088 - Neuerstellung
      *
      *   Rev 2.0   Mar 01 2000 09:58:00   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1350                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1350                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    15.03.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00088!15.03.00!2.1 ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1350
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>> ERMITTELN DER DTA-ID'S, DIE VON DER SST BEARBEITET
      *        WERDEN                                            <<<
      *
      ****************************************************************
      *
       01  PI-DRPA1350.
           05  PI-DRPA1350-UFKT                PIC X(002).
           05  PI-DRPA1350-PDTA-VSTAT          PIC X(004).
      *
       01  PO-DRPA1350.
           05  PO-DRPA1350-RC                 PIC  X(003).
               88 PO-DRPA1350-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1350-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1350-RC-LEER                      VALUE 'LER'.
      *
           05  PO-DRPA1350-PGEB-GES           PIC S9(004) COMP.
           05  PO-DRPA1350-DTA-ID             PIC S9(015)V COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************
