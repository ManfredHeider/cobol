      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0509.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0509.CPv  $
      *
      *   Rev 2.1   17 Oct 2000 13:35:46   T13AE30
      *FAN00264 - ANPASSUNG WG. ERMITTLUNG BD-STRUKTUR
      *
      *   Rev 2.0   07 Sep 2000 11:32:12   P13SP13
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: DRVB0509                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0509                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    17.10.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FAN00264!17.10.00! 01 ! NEU                       !TH.WILLIG *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0509: LESEN EDD
      *        -   PRüFEN, OB BETREUUNGSVERHÄLTNIS JE BESTANDEN HAT
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRVB0509.
           05  PI-DRVB0509-DATEN.
               10 PI-DRVB0509-ED-ST-NR         PIC S9(009) COMP.
               10 PI-DRVB0509-EDD-BEZUGS-NR    PIC S9(009) COMP.
      *
       01  PO-DRVB0509.
           05  PO-DRVB0509-RC                  PIC  X(003).
               88 PO-DRVB0509-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0509-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******************* ENDE COPY-MEMBER DRVB0509 ********************
