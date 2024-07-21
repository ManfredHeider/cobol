      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0366.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0366.CPv  $
      *
      *   Rev 2.2   Aug 23 2019 10:30:32   t13ae52
      *AAT00073: Erweiterung Fehlerverfahren/Fehlertoleranz - NEU
      *
      *   Rev 2.0   Jun 24 2019 14:18:05 T13AE52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0366                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0366                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         01.07.2019 / 09:59:07 / T13AE52
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
      *    FÜR DIE FUNKTION DRAT0366
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0366.
           05  PI-DRAT0366-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRAT0366-FAB-JOBNAME   PIC  X(0008).
             10 PI-DRAT0366-FAB-PROGRAMMNAME   PIC  X(0008).
             10 PI-DRAT0366-FAB-RELEVANZ   PIC  X(0001).
             10 PI-DRAT0366-FAB-SYSCODE   PIC  X(0002).
END   *

           05  PI-DRAT0366-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRAT0366-NI-FAB-ANZAHL       PIC S9(4) COMP.
             15 PI-DRAT0366-NI-FAB-PROZENT      PIC S9(4) COMP.
             15 PI-DRAT0366-NI-FAB-USERID       PIC S9(4) COMP.
END   *

           05  PI-DRAT0366-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRAT0366-FAB-ANZAHL   PIC S9(4) COMP.
             10 PI-DRAT0366-FAB-FAKTOR   PIC S9(4) COMP.
             10 PI-DRAT0366-FAB-GRENZE   PIC S9(9) COMP.
             10 PI-DRAT0366-FAB-PROZENT   PIC S9(2)V9(2) COMP-3.
             10 PI-DRAT0366-FAB-USERID   PIC  X(0007).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRAT0366-NI1.
           05 PI-DRAT0366-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 3.
       01  PI-DRAT0366-NI7.
           05 PI-DRAT0366-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 3.
      *
       01  PO-DRAT0366.
           05  PO-DRAT0366-RC                     PIC  X(003).
               88 PO-DRAT0366-RC-OK               VALUE 'OK '.
               88 PO-DRAT0366-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

