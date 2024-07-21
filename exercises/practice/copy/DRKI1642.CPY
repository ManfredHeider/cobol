      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.02.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1642.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1642.CPv  $
      *
      *   Rev 2.1   Feb 18 2004 16:09:52   t13ex39
      *AKI00391 Fonds aktualisieren
      *
      *   Rev 2.0   Feb 12 2004 08:43:22   T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1642                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1642                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  12.02.2004/12:22:56
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRKI1642
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKi1642.

           05  PI-DRKi1642-DATEN.
             10 PI-DRKi1642-FONDID    PIC S9(04) COMP.

      *
       01  PO-DRKi1642.

           05  PO-DRKi1642-DATEN.
             10 PO-DRKi1642-FONDID    PIC S9(04) COMP.

           05  PO-DRKi1642-RC                 PIC  X(003).
               88 PO-DRKi1642-RC-OK                        VALUE 'OK '.
               88 PO-DRKi1642-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
