      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2271.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2271.CPv  $
      *
      *   Rev 2.2   Nov 12 2013 12:03:42   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *
      *   Rev 2.1   Nov 19 2008 13:56:32   t13ex14
      *AKI00731: Bestandsdatenlieferung Kfz zukünftiger Beitrag
      *
      *   Rev 2.0   Sep 15 2008 10:22:36   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2271                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2271                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         28.10.2013 / 17:36:09 / T13EX52
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
      *    FÜR DIE FUNKTION DRKI2271
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2271.
           05  PI-DRKI2271-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2271-KFZZUKLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2271-KFZZUKVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2271.
           05  PO-DRKI2271-RC                     PIC  X(003).
               88 PO-DRKI2271-RC-OK               VALUE 'OK '.
               88 PO-DRKI2271-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
