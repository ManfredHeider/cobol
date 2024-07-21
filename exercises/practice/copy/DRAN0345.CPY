      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0345.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0345.CPv  $
      *
      *   Rev 2.4   Nov 13 2019 18:27:52   t13ae34
      *AAN01396: Antragserfassung Geiger Gold in einem System 
      *
      *   Rev 2.0   Oct 1 2019 08:28:56 T13AE34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0345                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAN0345                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         01.10.2019 / 08:28:56 / T13AE34
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
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAN0345
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAN0345.
           05  PI-DRAN0345-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRAN0345-PGEB-GES   PIC S9(4) COMP.
             10 PI-DRAN0345-VORP-LFD-NR   PIC S9(4) COMP.
             10 PI-DRAN0345-VORP-VORSCHLAG-NR   PIC  X(0017).
END   *

      *
       01  PO-DRAN0345.
           05  PO-DRAN0345-RC                     PIC  X(003).
               88 PO-DRAN0345-RC-OK               VALUE 'OK '.
               88 PO-DRAN0345-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRAN0345-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRAN0345-KNR-WAEHRUNG   PIC S9(4) COMP.
             10 PO-DRAN0345-PROD-PROD-SCHL   PIC S9(4) COMP.
             10 PO-DRAN0345-VORP-BETR   PIC S9(9)V9(2) COMP-3.
             10 PO-DRAN0345-VORP-EINH-AVERM   PIC S9(6)V9(2) COMP-3.
             10 PO-DRAN0345-VORP-KENNUNG-INS   PIC  X(0008).
             10 PO-DRAN0345-VORP-LAUFZEIT   PIC S9(4) COMP.
             10 PO-DRAN0345-VORP-SUMME   PIC S9(9)V9(2) COMP-3.
             10 PO-DRAN0345-VORP-TARIF   PIC  X(0015).
             10 PO-DRAN0345-VORP-ZAHLWEISE   PIC S9(4) COMP.
             10 PO-DRAN0345-VORP-ZEIT-INSERT   PIC  X(0026).
             10 PO-DRAN0345-VORP-ZUSATZ-A-1   PIC  X(0050).
             10 PO-DRAN0345-VORP-ZUSATZ-A-2   PIC  X(0050).
             10 PO-DRAN0345-VORP-ZUSATZ-N-1   PIC S9(9)V9(3) COMP-3.
             10 PO-DRAN0345-VORP-ZUSATZ-N-2   PIC S9(9)V9(3) COMP-3.
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

