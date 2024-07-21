      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0344.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0344.CPv  $
      *
      *   Rev 2.4   Nov 13 2019 18:26:54   t13ae34
      *AAN01396: Antragserfassung Geiger Gold in einem System 
      *
      *   Rev 2.0   Oct 1 2019 08:28:53 T13AE34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0344                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAN0344                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         01.10.2019 / 08:28:53 / T13AE34
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01396!10.10.19!    ! ATTRIBUTE                 ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAN0344
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAN0344.
           05  PI-DRAN0344-KEY.

             10 PI-DRAN0344-PGEB-GES   PIC S9(4) COMP.
             10 PI-DRAN0344-VORP-LFD-NR   PIC S9(4) COMP.
             10 PI-DRAN0344-VORP-VORSCHLAG-NR   PIC  X(0017).

           05  PI-DRAN0344-NI.

             15 FILLER                          PIC S9(4) COMP.

           05  PI-DRAN0344-DATEN.

             10 PI-DRAN0344-KNR-WAEHRUNG   PIC S9(4) COMP.
             10 PI-DRAN0344-PROD-PROD-SCHL   PIC S9(4) COMP.
             10 PI-DRAN0344-VORP-BETR   PIC S9(9)V9(2) COMP-3.
             10 PI-DRAN0344-VORP-EINH-AVERM   PIC S9(6)V9(2) COMP-3.
             10 PI-DRAN0344-VORP-LAUFZEIT   PIC S9(4) COMP.
             10 PI-DRAN0344-VORP-SUMME   PIC S9(9)V9(2) COMP-3.
             10 PI-DRAN0344-VORP-TARIF   PIC  X(0015).
             10 PI-DRAN0344-VORP-KENNUNG-INS   PIC  X(0015).
             10 PI-DRAN0344-VORP-ZAHLWEISE   PIC S9(4) COMP.
             10 PI-DRAN0344-VORP-ZUSATZ-A-1   PIC  X(0050).
             10 PI-DRAN0344-VORP-ZUSATZ-A-2   PIC  X(0050).
             10 PI-DRAN0344-VORP-ZUSATZ-N-1   PIC S9(9)V9(3) COMP-3.
             10 PI-DRAN0344-VORP-ZUSATZ-N-2   PIC S9(9)V9(3) COMP-3.
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRAN0344-NI1.
           05 PI-DRAN0344-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRAN0344-NI7.
           05 PI-DRAN0344-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.

      *
       01  PO-DRAN0344.
           05  PO-DRAN0344-RC                     PIC  X(003).
               88 PO-DRAN0344-RC-OK                  VALUE 'OK '.
               88 PO-DRAN0344-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

