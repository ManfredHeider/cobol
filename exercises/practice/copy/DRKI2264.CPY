      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2264.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2264.CPv  $
      *
      *   Rev 2.2   Nov 12 2013 12:02:34   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *
      *   Rev 2.1   Oct 10 2008 14:45:28   t13ex14
      *AKI00731: Bestandsdatenlieferung Kfz Tarifierung; OZPs
      *
      *   Rev 2.0   Sep 15 2008 10:22:00   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2264                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2264                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         29.10.2013 / 11:06:12 / T13EX52
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
      *    FÜR DIE FUNKTION DRKI2264
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2264.
           05  PI-DRKI2264-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2264-KFZSONLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2264-KFZSONVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2264-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2264-KFZSONTEILEWERT   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2264-KFZSONTEILNR-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2264-KFZSONTEILNR-KTY   PIC S9(4) COMP.
END   *

      *
       01  PO-DRKI2264.
           05  PO-DRKI2264-RC                     PIC  X(003).
               88 PO-DRKI2264-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2264-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
