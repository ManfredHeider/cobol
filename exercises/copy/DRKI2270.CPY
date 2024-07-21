      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2270.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2270.CPv  $
      *
      *   Rev 2.2   Nov 12 2013 12:03:36   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *
      *   Rev 2.1   Nov 19 2008 13:56:24   t13ex14
      *AKI00731: Bestandsdatenlieferung Kfz zukünftiger Beitrag
      *
      *   Rev 2.0   Sep 15 2008 10:22:32   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2270                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2270                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         28.10.2013 / 17:36:05 / T13EX52
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
      *    FÜR DIE FUNKTION DRKI2270
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2270.
           05  PI-DRKI2270-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2270-KFZZUKLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2270-KFZZUKVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2270-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2270-KFZZUKBEITRAGNEU   PIC S9(5)V9(2) COMP-3.
             10 PI-DRKI2270-KFZZUKSFJANZALT   PIC S9(4) COMP.
             10 PI-DRKI2270-KFZZUKSFJANZNEU   PIC S9(4) COMP.
             10 PI-DRKI2270-KFZZUKSPARTE   PIC  X(0006).
END   *


      *
       01  PO-DRKI2270.
           05  PO-DRKI2270-RC                     PIC  X(003).
               88 PO-DRKI2270-RC-OK               VALUE 'OK '.
               88 PO-DRKI2270-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
