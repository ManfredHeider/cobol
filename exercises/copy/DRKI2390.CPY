      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2390.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2390.CPv  $
      *
      *   Rev 2.2   Sep 06 2012 14:28:18   t13ae49
      *AKI00943: Std.-OZP für Tabelle KRANKENPERSEIGEN
      *
      *   Rev 2.1   Sep 06 2012 10:58:24   t13ex14
      *AKI00943: Std.-OZP für Tabelle KRANKENPERSEIGEN
      *
      *   Rev 2.0   Mar 22 2010 07:41:52   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2390                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2390                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         03.11.2011 / 18:47:30 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2390
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2390.
           05  PI-DRKI2390-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2390-KPEPERSONLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2390-KPEVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2390.
           05  PO-DRKI2390-RC                     PIC  X(003).
               88 PO-DRKI2390-RC-OK               VALUE 'OK '.
               88 PO-DRKI2390-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
