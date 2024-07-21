      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0364.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0364.CPv  $
      *
      *   Rev 2.2   Aug 23 2019 10:29:46   t13ae52
      *AAT00073: Erweiterung Fehlerverfahren/Fehlertoleranz - NEU
      *
      *   Rev 2.0   Jun 24 2019 14:18:02 T13AE52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0364                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0364                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         01.07.2019 / 09:59:03 / T13AE52
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
      *    FÜR DIE FUNKTION DRAT0364
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0364.
           05  PI-DRAT0364-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRAT0364-FAB-JOBNAME   PIC  X(0008).
             10 PI-DRAT0364-FAB-PROGRAMMNAME   PIC  X(0008).
             10 PI-DRAT0364-FAB-RELEVANZ   PIC  X(0001).
             10 PI-DRAT0364-FAB-SYSCODE   PIC  X(0002).
END   *

      *
       01  PO-DRAT0364.
           05  PO-DRAT0364-RC                     PIC  X(003).
               88 PO-DRAT0364-RC-OK               VALUE 'OK '.
               88 PO-DRAT0364-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRAT0364-NI.
               10 NI-DRAT0364-TAB.
                  15 DRAT0364-ELE
                     PIC S9(04) COMP OCCURS 3 TIMES.
               10 NI-DRAT0364 REDEFINES NI-DRAT0364-TAB.

ANF   *        %M_PO_DATEN_NI%
             15 PO-DRAT0364-NI-FAB-ANZAHL   PIC S9(4) COMP.
             15 PO-DRAT0364-NI-FAB-PROZENT   PIC S9(4) COMP.
             15 PO-DRAT0364-NI-FAB-USERID   PIC S9(4) COMP.
END   *

           05  PO-DRAT0364-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRAT0364-FAB-ANZAHL   PIC S9(4) COMP.
             10 PO-DRAT0364-FAB-FAKTOR   PIC S9(4) COMP.
             10 PO-DRAT0364-FAB-GRENZE   PIC S9(9) COMP.
             10 PO-DRAT0364-FAB-PROZENT   PIC S9(2)V9(2) COMP-3.
             10 PO-DRAT0364-FAB-USERID   PIC  X(0007).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

