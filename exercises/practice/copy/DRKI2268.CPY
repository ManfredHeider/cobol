      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2268.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2268.CPv  $
      *
      *   Rev 2.2   Nov 12 2013 12:03:30   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *
      *   Rev 2.1   Nov 19 2008 13:56:16   t13ex14
      *AKI00731: Bestandsdatenlieferung Kfz zukünftiger Beitrag
      *
      *   Rev 2.0   Sep 15 2008 10:22:22   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2268                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2268                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         28.10.2013 / 17:35:58 / T13EX52
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
      *    FÜR DIE FUNKTION DRKI2268
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2268.
           05  PI-DRKI2268-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2268-KFZZUKLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2268-KFZZUKVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2268.
           05  PO-DRKI2268-RC                     PIC  X(003).
               88 PO-DRKI2268-RC-OK               VALUE 'OK '.
               88 PO-DRKI2268-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2268-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2268-KFZZUKBEITRAGNEU   PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI2268-KFZZUKSFJANZALT   PIC S9(4) COMP.
             10 PO-DRKI2268-KFZZUKSFJANZNEU   PIC S9(4) COMP.
             10 PO-DRKI2268-KFZZUKSPARTE   PIC  X(0006).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
