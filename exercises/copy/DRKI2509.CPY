      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2509.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2509.CPv  $
      *
      *   Rev 2.2   Sep 06 2012 14:28:24   t13ae49
      *AKI00943: Std.-OZP für Tabelle KRANKENPERSEIGEN
      *
      *   Rev 2.1   Sep 06 2012 10:58:32   t13ex14
      *AKI00943: Std.-OZP für Tabelle KRANKENPERSEIGEN
      *
      *   Rev 2.0   Nov 3 2011 18:47:33 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2509                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2509                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         03.11.2011 / 18:47:33 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2509
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2509.
           05  PI-DRKI2509-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2509-KPEPERSONLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2509-KPEVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2509-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2509-KPEPERSONENID   PIC  X(0013).
END   *


      *
       01  PO-DRKI2509.
           05  PO-DRKI2509-RC                     PIC  X(003).
               88 PO-DRKI2509-RC-OK               VALUE 'OK '.
               88 PO-DRKI2509-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
