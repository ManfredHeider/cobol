      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.05.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1911.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1911.CPv  $
      *
      *   Rev 2.2   Nov 12 2013 12:01:52   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *
      *   Rev 2.1   Jul 28 2006 07:30:32   t13ex14
      *AKI00553: OZPs
      *
      *   Rev 2.0   May 23 2006 10:56:22   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1911                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1911                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         28.10.2013 / 15:30:23 / T13EX52
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
      *    FÜR DIE FUNKTION DRKI1911
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1911.
           05  PI-DRKI1911-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI1911-KFZTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1911-KFZTARVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI1911-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI1911-KFZTARTBGDVKZ   PIC  X(0050).
             10 PI-DRKI1911-KFZTARTBKTX   PIC  X(0010).
             10 PI-DRKI1911-KFZTARTBSPARTE   PIC  X(0006).
             10 PI-DRKI1911-KFZTARTBTXT   PIC  X(0050).
END   *


      *
       01  PO-DRKI1911.
           05  PO-DRKI1911-RC                     PIC  X(003).
               88 PO-DRKI1911-RC-OK               VALUE 'OK '.
               88 PO-DRKI1911-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
