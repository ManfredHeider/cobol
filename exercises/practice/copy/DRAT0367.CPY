      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0367.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0367.CPv  $
      *
      *   Rev 2.2   Aug 23 2019 10:30:54   t13ae52
      *AAT00073: Erweiterung Fehlerverfahren/Fehlertoleranz - NEU
      *
      *   Rev 2.0   Jun 24 2019 14:18:06 T13AE52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0367                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0367                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         01.07.2019 / 09:59:09 / T13AE52
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
      *    FÜR DIE FUNKTION DRAT0367
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0367.
           05  PI-DRAT0367-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRAT0367-FAB-JOBNAME   PIC  X(0008).
             10 PI-DRAT0367-FAB-PROGRAMMNAME   PIC  X(0008).
             10 PI-DRAT0367-FAB-RELEVANZ   PIC  X(0001).
             10 PI-DRAT0367-FAB-SYSCODE   PIC  X(0002).
END   *

      *
       01  PO-DRAT0367.
           05  PO-DRAT0367-RC                     PIC  X(003).
               88 PO-DRAT0367-RC-OK               VALUE 'OK '.
               88 PO-DRAT0367-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************

